;; Copyright 2007-2008, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(include "requirements.scm")

(define formdat:*debug* #f)

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

;; get the header from datstr
(define (formdat:read-header datstr) ;; datstr is an input string port
  (let loop ((hs (read-line datstr))
	     (header '()))
    (if (or (eof-object? hs)
	    (string=? hs ""))
	header
	(loop (read-line datstr)(append header (list hs))))))

;; get the data up to the next key. if there is no key then return #f
;; return (dat remdat)
(define (formdat:read-dat dat key)
  (let ((index (string-search-positions key dat)))
    (if (or (not index)
	    (null? index)) ;; the key was not found
	#f
	(let* ((datstr (open-input-string dat))
	       (result (read-string (caar index) datstr))
	       (remdat (read-string #f datstr)))
	  (close-input-port datstr)
	  (list result remdat)))))

 ;; inp is port to read data from, maxsize is max data allowed to read (total)
(define (formdat:dat->list inp maxsize)
  ;; read 1Meg chunks from the input port. If a block is not complete
  ;; tack on the next 1Meg chunk as needed. Set up so the header is always
  ;; at the beginning of the chunk
  ;;-----------------------------29932024411502323332136214973
  ;;Content-Disposition: form-data; name="input-picture"; filename="breadfruit.jpg"
  ;;Content-Type: image/jpeg
  (let loop ((dat (read-string 1000000 inp))
	     (res '())
	     (siz 0))
    (if (> siz maxsize)
	(begin
	  (print "DATA TOO BIG")
	  res)
	(let* ((datstr (open-input-string dat))
	       (header (formdat:read-header datstr))
	       (key    (if (not (null? header))(car header) #f))
	       (remdat (read-string #f datstr))          ;; used in next line, discard if got data, else revert to
	       (alldat (if key (formdat:read-dat remdat key) #f))    ;; try to extract the data
	       (thsdat (if alldat (car alldat)  #f))     ;; the data
	       (newdat (if alldat (cadr alldat) #f))     ;; left over data, must process ...
	       (thsres (list header thsdat))             ;; speculatively construct results
	       (newres (append res (list thsres))))      ;; speculatively construct results
	  (close-input-port datstr)
	  (cond
	   ;; either no header or single input
	   ((and (not alldat)
		 (or (null? header)
		     (not (string-match formdat:delim-patt-rex (car header)))))
	    ;; (print "Got here")
	    (cons (list header "") res)) ;; note use header as dat and use "" as header????
	   ;; didn't find end key in this block
	   ((not alldat)
	    (let ((mordat (read-string 1000000 inp)))
	      (if (string=? mordat "") ;; there is no more data, discard results and use remdat as data, this input is broken
		  (cons (list header remdat) res)
		  (loop (string-append dat mordat) res (+ siz 2000000))))) ;; add the extra 1000000
	   (alldat ;; got data, don't attempt to check if there is more, just loop and rely on (not alldat) to get more data
	    (loop newdat newres (+ siz 1000000))))))))

(define formdat:bin-data-disp-rex (regexp "^Content-Disposition:\\s+form-data;"))
(define formdat:bin-data-name-rex (regexp "\\Wname=\"([^\"]+)\""))
(define formdat:bin-file-name-rex (regexp "\\Wfilename=\"([^\"]+)\""))
(define formdat:bin-file-type-rex (regexp "Content-Type:\\s+([^\\s]+)"))
(define formdat:delim-patt-rex    (regexp "^\\-+[0-9]+\\-*$"))

;; returns a hash with entries for all forms - could well use a proplist?
(define (formdat:load-all)
  (let ((request-method (getenv "REQUEST_METHOD")))
    (if (and request-method
	     (string=? request-method "POST"))
	(formdat:load-all-port (current-input-port)))))

;; (s:process-cgi-input (caaar dat))
(define (formdat:load-all-port inp)
  (let* ((formdat        (make <formdat>)))
;;	 (debugp         (open-output-file (conc (slot-ref s:session 'sroot) "/delme-" (current-user-id) ".log"))))
    ;; (write-string (read-string #f inp) #f debugp)
    (let ((alldats (formdat:dat->list inp 10e6)))
      
      ;; (format debugp "formdat : alldats: ~A\n" alldats)
      (let ((firstitem   (car alldats))
	    (multipass #f)) 
	(if (not (null? firstitem))
	    (if (string-match formdat:delim-patt-rex (caar firstitem))
		(set! multipass #t)))
	(if multipass
	    ;; handle multi-part form
	    (for-each (lambda (datlst)
			(let* ((header (formdat:extract-header-info (car datlst)))
			       (name   (if (assoc 'name header)
					   (string->symbol (cadr (assoc 'name header)))
					   "")) ;; grumble
			       (fnamel  (assoc 'filename header))
			       (content (assoc 'content header))
			       (dat    (cadr datlst)))
			  ;; (print "header: " header " name: " name " fnamel: " fnamel " content: " content) ;;  " dat: " (dat)
			  (formdat:set! formdat 
					name
					(if fnamel 
					    (list (cadr fnamel)
						  (if content
						      (cadr content)
						      "unknown")
						  (string->blob dat))
					    dat))))
		      alldats)
	    ;; handle single part form
	    ;; 	(if (and (string? name)
	    ;; 		     (string=? name "")) ;; this is the short form input I guess
	    ;; 		(let* ((datstr (caar datlst))
	    ;; 		       (munged (s:process-cgi-input datstr)))
	    ;; 		  (print "datstr: " datstr " munged: " munged)
	    (formdat:load formdat  (s:process-cgi-input (caaar alldats)))) ;; munged))
	;;		    (format debugp "formdat : name: ~A content: ~A\n" name content)
	;; (close-output-port debugp)
	formdat))))
		
#|
(define inp (open-input-file "tests/example.post.in"))
(define dat (read-string #f inp))
(define datstr (open-input-string dat))

;; or

(define inp (open-input-file "tests/example.post.binary.in"))
(define dat (read-string #f inp))
(define datstr (open-input-string dat))

(formdat:read-header datstr)

(define dat (formdat:dat->list inp 10e6))
(close-input-port inp)
|#
  
(define (formdat:extract-header-info header)
  (if (null? header)
      '()
      (let loop ((hed (car header))
		 (tal (cdr header))
		 (res '()))
	(if (string-match formdat:bin-data-disp-rex hed) ;; 
	    (let* ((data-namem (string-match formdat:bin-data-name-rex hed))
		   (file-namem (string-match formdat:bin-file-name-rex hed))
		   (data-name  (if data-namem (cadr data-namem) #f))
		   (this       (if file-namem
				   (list (list 'name data-name)(list 'filename (cadr file-namem)))
				   (list (list 'name data-name)))))
	      (if (null? tal)
		  (append res this)
		  (loop (car tal)(cdr tal)(append res this))))
	    (let ((content (string-match formdat:bin-file-type-rex hed))) ;; this is the stanza for the content type
	      (if content
		  (let ((newres (cons (list 'content (cadr content)) res)))
		    (if (null? tal)
			newres
			(loop (car tal)(cdr tal) newres)))
		  (if (null? tal)
		      res
		      (loop (car tal)(cdr tal) res)
		      )))))))

;;	      (let loop ((l       (read-line)) ;; (if (eq? mode 'norm)(read-line)(read-char)))
;;			 (endline #f)
;;			 (num     0))
;;		;; (format debugp "~A\n" l)
;;              (if (or (not (eof-object? l))
;;		      (not (and (eq? mode 'bin)
;;				(string=? l "")))) ;; if in bin mode empty string is end of file
;;		  (case mode
;;		    ((start)
;;		     (set! mode 'norm)
;;		     (if (string-match delim-patt-rex l)
;;			 (begin
;;			   (set! delim-string l)
;;			   (set! delim-len    (string-length l))
;;			   (loop (read-line) #f 0))
;;			 (loop l #f 0)))
;;		    ((norm)
;;		     ;; I don't like how this gets checked on every single input. Must be a better way. FIXME
;;		     (if (and (string-match bin-data-disp-rex l)
;;			      (string-match bin-data-name-rex l)
;;			      (string-match bin-file-name-rex l))
;;			 (begin
;;			   (set! data-name (cadr (string-match bin-data-name-rex l)))
;;			   (set! file-name (cadr (string-match bin-file-name-rex l)))
;;			   (set! mode 'content)
;;			   (loop (read-line) #f num)))
;;		     (let* ((dat  (s:process-cgi-input l))) ;; (CGI:url-unquote l))
;;		       (format debugp "PROCESS-CGI-INPUT: ~A\n" (intersperse dat ","))
;;		       (formdat:load formdat dat)
;;		       (loop (read-line) #f num)))
;;		    ((content)
;;		     (if (string-match bin-file-type-rex l)
;;			 (begin 
;;			   (set! mode 'bin)
;;			   (set! data-type (cadr (string-match bin-file-type-rex l)))
;;			   (loop (read-string 1) #f num))))
;;		    ((bin)
;;		     ;; delim-string: \n"---------------12345"
;;		     ;;                  012345678901234567890
;;		     ;; endline:        "---------------12"
;;		     ;; l = "3"
;;		     ;; delim-len = 20
;;		     ;; (substring  "---------------12345" 17 18) => "3"
;;		     ;;
;;		     (cond
;;		       ;; haven't found the start of an endline, is the next char a newline?
;;		      ((and (not endline)
;;			    (string=? l "\n")) ;; required first character 
;;		       (let ((newendline (open-output-string)))
;;			 ;; (write-line l newendline) ;; discard the newline. add it back if don't have a lock on delim-string
;;			 (loop (read-string 1) newendline (+ num 1))))
;;		      ((not endline)
;;		       (write-string l #f bin-dat)
;;		       (loop (read-string 1) #f (+ num 1)))
;;		      ;; string so far matches delim-string
;;		      (endline
;;		       (let* ((endstr (get-output-string endline))
;;			      (endlen (string-length endstr)))
;;			 (if (> endlen 0)
;;			     (format debugp " delim: ~A\nendstr: ~A\n" delim-string endstr))
;;			 (if (and (> delim-len endlen)
;;				  (string=? l (substring delim-string endlen (+ endlen 1))))
;;			     ;; yes, this character matches the next in the delim-string
;;			     (if (eq? delim-len endlen) ;; have a match! Ignore that a newline is required. Lazy bugger.
;;				 (let* ((fn      (string->symbol data-name)))
;;				   (formdat:set! formdat fn (list file-name data-type (string->blob (get-output-string bin-dat))))
;;				   (set! mode 'norm)
;;				   (loop (read-line) #f 0))
;;				 (begin
;;				   (write-string l #f endline)
;;				   (loop (read-string 1) endline (+ num 1))))
;;			     ;; no, this character does NOT match the next in line in delim-string
;;			     (begin
;;			       (write-string "\n" #f bin-dat) ;; don't forget that newline we dropped
;;			       (write-string endstr #f bin-dat)
;;			       (write-string l #f bin-dat)
;;			       (loop (read-string 1) #f (+ num 1))))))))
;;		    )))))

;;    (formdat:printall formdat (lambda (x)(write-line x debugp)))

#|
(define inp (open-input-file "/tmp/stmlrun/delme-33.log.keep-for-ref"))
(define dat (read-string #f inp))
(close-input-port inp)
|#
