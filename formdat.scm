;; Copyright 2007-2008, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(include "requirements.scm")

;; Old data format was something like this. BUT! 
;; Forms do not have names so the hierarcy is
;; unnecessary (I think)
;;
;; hashtable
;;   |-formname --> <formdat> 'form-name=formname
;;   |                        'form-data=hashtable
;;   |                                       | name => value
;;
;; New data format is only the <formdat> portion from above

(define-class <formdat> ()
   (form-data
   ))

(define-method (initialize (self <formdat>) initargs)
  (call-next-method)
  (slot-set! self 'form-data (make-hash-table))
  (initialize-slots self initargs))

(define-method (formdat:get (self <formdat>) key)
  (hash-table-ref/default (slot-ref self 'form-data) key #f))

;; change to convert data to list and append val if already exists
;; or is a list
(define-method (formdat:set! (self <formdat>) key val)
  (let ((prev-val (formdat:get self key))
        (ht       (slot-ref self 'form-data)))
    (if prev-val
        (if (list? prev-val)
            (hash-table-set! ht key (cons val prev-val))
            (hash-table-set! ht key (list val prev-val)))
        (hash-table-set! ht key val))
    self))

(define-method (formdat:keys (self <formdat>))
  (hash-table-keys (slot-ref self 'form-data)))

(define-method (formdat:printall (self <formdat>) printproc)
  (printproc "formdat:printall " (formdat:keys self))
  (for-each (lambda (k)
	      (printproc k " => " (formdat:get self k)))
	    (formdat:keys self)))

(define-method (formdat:all->strings (self <formdat>))
  (let ((res '()))
    (for-each (lambda (k)
                 (set! res (cons (conc k "=>" (formdat:get self k)) res)))
              (formdat:keys self))
        res))

;; call with *one* of the lists in the list of lists created by CGI:url-unquote
(define-method (formdat:load (self <formdat>) formlist)
  (let ((ht             (slot-ref self 'form-data)))
    (if (null? formlist) self ;; no values provided, return self for no good reason
        (let loop ((head (car formlist))
                   (tail (cdr formlist)))
          (let ((key (car head))
                (val (cdr head)))
            ;; (err:log "key=" key " val=" val)
	    (if (> (length val) 1)
		(formdat:set! self key val)
		(formdat:set! self key (car val)))
            (if (null? tail) self   ;; we are done
                (loop (car tail)(cdr tail))))))))

;; returns a hash with entries for all forms - could well use a proplist?
(define (formdat:load-all)
  (let* ((formdat        (make <formdat>))
         (request-method (getenv "REQUEST_METHOD"))
	 (debugp (open-output-file (conc "/tmp/stmlrun/delme-" (current-user-id) ".log")))
	 (mode   'start)
	 (delim-string #f)
	 (data-name #f)
	 (file-name #f)
	 (data-type #f)
	 (bin-dat (open-output-string))
	 (bin-data-disp-rex (regexp "^Content-Disposition:\\s+form-data;"))
	 (bin-data-name-rex (regexp "\\Wname=\"([^\"]+)\""))
	 (bin-file-name-rex (regexp "\\Wfilename=\"([^\"]+)\""))
	 (bin-file-type-rex (regexp "Content-Type:\\s+([^\\s]+)"))
	 (delim-patt-rex    (regexp "^-----")))
    ;;-----------------------------29932024411502323332136214973
    ;;Content-Disposition: form-data; name="input-picture"; filename="breadfruit.jpg"
    ;;Content-Type: image/jpeg
    (if request-method
        (if (string=? request-method "POST")
            (let loop ((l   (read-line)) ;; (if (eq? mode 'norm)(read-line)(read-char)))
		       (num 0))
	      (format debugp "~A\n" l)
              (if (not (eof-object? l))
		  (case mode
		    ((start)
		     (set! mode 'norm)
		     (if (string-match delim-patt-rex l)
			 (begin
			   (set! delim-string l)
			   (loop (read-line) 0))
			 (loop l 0)))
		    ((norm)
		     ;; I don't like how this gets checked on every single input. Must be a better way. FIXME
		     (if (and (string-match bin-data-disp-rex l)
			      (string-match bin-data-name-rex l)
			      (string-match bin-file-name-rex l))
			 (begin
			   (set! data-name (cadr (string-match bin-data-name-rex l)))
			   (set! file-name (cadr (string-match bin-file-name-rex l)))
			   (set! mode 'content)
			   (loop (read-line) num)))
		     (let* ((dat  (s:process-cgi-input l))) ;; (CGI:url-unquote l))
		       (format debugp "PROCESS-CGI-INPUT: ~A\n" (intersperse dat ","))
		       (formdat:load formdat dat)
		       (loop (read-line) num)))
		    ((content)
		     (if (string-match bin-file-type-rex l)
			 (begin 
			   (set! mode 'bin)
			   (set! data-type (cadr (string-match bin-file-type-rex l)))
			   (loop (read-line) num))))
		    ((bin)
		     (if (string=? l delim-string)
			 (let* ((fn      (string->symbol data-name)))
			   (formdat:set! formdat fn (list file-name data-type (string->blob (get-output-string bin-dat))))
			   (set! mode 'norm)
			   (loop (read-line) 0))
			 (begin
			   (write-line l bin-dat)
			   (if (> num 10e6)(set! mode 'dead))
			   (loop (read-line)(+ num (string-length l)))))))))))
;;    (formdat:printall formdat (lambda (x)(write-line x debugp)))
    (close-output-port debugp)
    formdat))
