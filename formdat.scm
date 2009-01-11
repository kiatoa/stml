;; Copyright 2007-2008, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(include "requirements.scm")

(define-class <formdat> ()
  (form-name
   form-data))

(define-method (initialize (self <formdat>) initargs)
  (call-next-method)
  (slot-set! self 'form-name "")
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
        (hash-table-set! ht key val))))

(define-method (formdat:keys (self <formdat>))
  (hash-table-keys (slot-ref self 'form-data)))

(define-method (formdat:->string (self <formdat>))
  (let ((name    (slot-ref self 'form-name))
        (keys    (formdat:keys self))
        (ht      (slot-ref self 'form-data)))
    (string-append "name: " name ", "
                   (string-join 
                    (map (lambda (k)
                           (let ((v (hash-table-ref ht k)))
                             (string-append (symbol->string k) "=" (conc v))))
                         keys)
                    ", "))))

;; call with *one* of the lists in the list of lists created by CGI:url-unquote
(define-method (formdat:load (self <formdat>) formlist)
  (let ((form-name (assoc 'form-name formlist)) ;; ( ('foo "bar") ('x "y") ('nada "a" "b") ...
        ;;                                         ;;     var  val ...
        (ht             (slot-ref self 'form-data)))
    (if form-name 
        (slot-set! self 'form-name (cadr form-name)))
    ;; (err:log "form-name=" form-name)
    (if (null? formlist) self ;; no values provided, return self for no good reason
        (let loop ((head (car formlist))
                   (tail (cdr formlist)))
          (let ((key (car head))
                (val (cdr head)))
            (err:log "key=" key " val=" val)
	    (if (> (length val) 1)
		(formdat:set! self key val)
		(formdat:set! self key (car val)))
            (if (null? tail) self   ;; we are done
                (loop (car tail)(cdr tail))))))))

;; convert all the formdats in hash table ht to strings
(define (formdat:all->strings ht)
  (map (lambda (form-name)
         (formdat:->string (hash-table-ref ht form-name)))
       (hash-table-keys ht)))

;; returns a hash with entries for all forms - could well use a proplist?
(define (formdat:load-all)
  (let* ((ht    (make-hash-table))
         (request-method (getenv "REQUEST_METHOD")))
    (if request-method
        (if (string=? request-method "POST")
            (let loop ((l (read-line)))
	      (session:log s:session "form l: " l)
              (if (not (eof-object? l))
                  (let ((dat (s:process-cgi-input l)) ;; (CGI:url-unquote l))
                        (form (make <formdat>)))
                    (formdat:load form dat)
                    (hash-table-set! ht (slot-ref form 'form-name) form)
                    (loop (read-line)))))))
    ht))
