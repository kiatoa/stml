;; Copyright 2007-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;;======================================================================
;; dumbobj helpers
;;======================================================================

(declare (unit misc-stml))
(use regex)

;; given a list of symbols give the count of the matching symbol
;; l => '(a b c)  (dumobj:indx a 'b) => 1
(define (s:get-fieldnum lst field-name)
  (let loop ((head (car lst))
             (tail (cdr lst))
             (fnum 0))
    (if (eq? head field-name) fnum
        (if (null? tail) #f
            (loop (car tail)(cdr tail)(+ fnum 1))))))

(define (s:fields->string lst)
  (string-join (map symbol->string lst) ","))

(define (s:vector-get-field vec field field-list)
  (vector-ref vec (s:get-fieldnum field-list field)))

;;======================================================================
;;
;;======================================================================

(define (err:log . msg)
  (with-output-to-port (current-error-port) ;; (slot-ref self 'logpt)
    (lambda () 
      (apply print msg))))

(define (s:tidy-url url)
  (if url
      (let ((r1 (regexp "^http:\\/\\/"))
            (r2 (regexp "^[ \\t]*$"))) ;; blank
        (if (string-match r1 url) url
            (if (string-match r2 url) #f ;; convert a blank to #f
                (conc "http://" url))))
      url))

(define (s:lazy->num num)
  (if (number? num) num
      (if (string->number num) (string->number num)
	    (if num 1 0)))) ;; wierd eh! yep, #f=>0 #t=>1 

;;======================================================================
;; D B
;;======================================================================

;; convert values to appropriate strings
;;
(define (s:sqlparam-val->string val)
  (cond
   ((list?   val)(string-join (map symbol->string val) ",")) ;; (a b c) => a,b,c
   ((string? val)(conc "'" (dbi:escape-string val) "'"))
   ((number? val)(number->string val))
   ((symbol? val)(dbi:escape-string (symbol->string val)))
   ((boolean? val)
    (if val "TRUE" "FALSE"))  ;; should this be "TRUE" or 1?
                              ;; should this be "FALSE" or 0 or NULL?
   (else
    (err:log "sqlparam: unknown type for value: " val)
    "")))

;; (sqlparam "INSERT INTO foo(name,age) VALUES(?,?);" "bob" 20)
;; NB// 1. values only!! 
;;      2. terminating semicolon required (used as part of logic)
;;
;; a=? 1 (number) => a=1
;; a=? 1 (string) => a='1'
;; a=? #f         => a=FALSE 
;; a=? a (symbol) => a=a 
;;
(define (s:sqlparam query . args)
  (let* ((query-parts (string-split query "?"))
         (num-parts    (length query-parts))
         (num-args    (length args)))
    (if (not (= (+ num-args 1) num-parts))
        (err:log "ERROR, sqlparam: wrong number of arguments or missing semicolon, " num-args " for query " query)
        (if (= num-args 0) query
            (let loop ((section (car query-parts))
                       (tail    (cdr query-parts))
                       (result  "")
                       (arg     (car args))
                       (argtail (cdr args)))
              (let* ((valstr    (s:sqlparam-val->string arg))
                     (newresult (conc result section valstr)))
                (if (null? argtail) ;; we are done
                    (conc newresult (car tail))
                    (loop
                     (car tail)
                     (cdr tail)
                     newresult
                     (car argtail)
                     (cdr argtail)))))))))

;; random string stuff
(define (s:string-downcase str)
  (if (string? str)
      (string-translate str "ABCDEFGHIJKLMNOPQRSTUVWXYZ" "abcdefghijklmnopqrstuvwxyz")
      str)) 

;; (define session:valid-chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
(define session:valid-chars "abcdefghijklmnopqrstuvwxyz0123456789") ;; cookies are case insensitive.
(define session:num-valid-chars (string-length session:valid-chars))

(define (session:get-nth-char nth)
  (substring session:valid-chars nth  (+ nth 1)))

(define (session:get-rand-char)
  (session:get-nth-char (random session:num-valid-chars)))

(define (session:make-rand-string len)
  (let loop ((res "")
             (n   1))
    (if (> n len) res
        (loop (string-append res (session:get-rand-char))
              (+ n 1)))))

;; openssl passwd -crypt -salt xx password
;;
(define (s:crypt-passwd pw s)
  (let* ((salt (if s s (session:make-rand-string 2)))
	 (inp (open-input-pipe 
               ;;(string-append "echo " pw " | mkpasswd -S " salt " -s")))
	       (conc "mkpasswd " pw " " salt)))
         (res (read-line inp)))
    (close-input-port inp)
    res))

(define (s:password-match? password crypted)
  (let* ((salt (substring crypted 0 2))
         (pcrypted (s:crypt-passwd password salt)))
    (string=? pcrypted crypted)))

;; (read-line (open-input-pipe "echo foo | mkpasswd -S ab -s"))

(define (s:error-page . err)
  (s:cgi-out (cons "Content-type: text/html; charset=iso-8859-1\n\n"
		   (s:html (s:head 
			    (s:title err)
			    (s:body
			     (s:h1 "ERROR")
			     (s:p err)))))))

(define (s:validate-uri)
  (let ((uri (get-environment-variable "REQUEST_URI"))
	(qrs (get-environment-variable "QUERY_STRING")))
    (if (not uri)
	(set! uri qrs))
    (if uri
	(string-match 
	 (regexp "^(/[a-z\\-\\._:0-9]*)*(|\\?([A-Za-z0-9_\\-\\+]+=[A-Za-z0-9_\\-\\.\\+]*&{0,1})*)$") uri)
	(begin
	  (s:log "REQUEST URI NOT AVAILABLE!")
	  (let ((p (open-input-pipe "env")))
	    (let loop ((l (read-line p))
		       (res '()))
	      (if (eof-object? l)
		  (s:log res)
		  (loop (read-line p)(cons (list l "<BR>") res)))))
	  #t))))

(define (s:validate-inputs)
  (if (not (s:validate-uri))
      (begin (s:error-page "Bad URI" (let ((ref (get-environment-variable "HTTP_REFERER")))
				       (if ref
					   (list "referred from" ref)
					   "")))
	     (exit))))

;; anything except a list is converted to a string!!!
(define (s:any->string val)
  (cond
   ((string? val) val)
   ((number? val) (number->string val))
   ((symbol? val) (symbol->string val))
   ((eq? val #f) "")
   ((eq? val #t) "TRUE")
   ((list? val) val)
   (else 
    (let ((ostr (open-output-string)))
      (with-output-to-port ostr
	(lambda ()
	  (display val)))
      (get-output-string ostr)))))

(define (s:any->number val)
  (cond
   ((number? val)  val)
   ((string? val)  (string->number val))
   ((symbol? val)  (string->number (symbol->string val)))
   (else     #f)))

;; NB// this is *illegal* pgint
(define (s:illegal-pgint val)
  (cond
   ((> val 2147483647) 1)
   ((< val -2147483648) -1)
   (else #f)))

(define (s:any->pgint val)
  (let ((n (s:any->number val)))
    (if n
	(if (s:illegal-pgint n)
	    #f
	    n)
	n)))

;; string is a string and non-zero length
(define (misc:non-zero-string str)
  (if (and (string? str)
           (> (string-length str) 0))
      str
      #f))

;;======================================================================
;; P A R A M S
;;======================================================================

;; input: 'a ('a "val a" 'b "val b") => "val a"
(define (s:find-param key param-lst)
  (let loop ((head (car param-lst))
	     (tail (cdr param-lst)))
    (if (eq? head key)
	(car tail)
	(if (< (length tail) 2) #f
	    (loop (cadr tail)(cddr tail))))))

(define (s:param->string param)
  (conc (symbol->string (car param)) "=" "\"" (cadr param) "\""))

;; remove 'foo "bar" from ('foo "bar" 'bar "foo")
(define (s:remove-param-matching params key)
  (if (= (length params) 0)'() ;;  proper params list >= 2 items
      (let loop ((head     (car params))
                 (tail     (cdr params))
                 (result   '()))
        (if (symbol? head) ;; symbols have params
            (let ((val     (car tail))
                  (newtail (cdr tail)))
              (if (eq? head key)  ;; get rid of this one
                  (if (null? newtail) result
                      (loop (car newtail)(cdr newtail) result))
                  (let ((newresult (append result (list head val))))
                    (if (null? newtail) newresult
                        (loop (car newtail)(cdr newtail) newresult)))))
            (let ((newresult (append result (list head))))
              (if (null? tail) newresult
                  (loop (car tail)(cdr tail) newresult)))))))

(define (session:get-param-from params key)
  (let ((r1 (regexp (conc "^" (s:any->string key) "=(.*)$"))))
    (if (null? params) #f
        (let loop ((head (car params))
                   (tail (cdr params)))
          (let ((match (string-match r1 head)))
            (if match
                (list-ref match 1)
                (if (null? tail) #f
                    (loop (car tail)(cdr tail)))))))))

(define (s:process-params params)
  (if (null? params) ""
      (let loop ((res "")
                 (head (car params))
                 (tail (cdr params)))
        (if (null? tail)
            (conc res " " (s:param->string head))
            (loop
             (conc res " " (s:param->string head))
             (car tail)
             (cdr tail))))))

;; remove key=var from (key=var key1=var1 key2=var2 ...)
(define (k=v-params:remove-matching params key)
  (if (= (length params) 0) params
      (let ((r1 (regexp (conc "^" key "="))))
        (let loop ((head (car params))
                   (tail (cdr params))
                   (result '()))
          (if (string-match r1 head)
              (if (null? tail) result
                  (loop (car tail)(cdr tail) result))
              (let ((newlst (cons head result)))
                (if (null? tail) newlst
                    (loop (car tail)(cdr tail) newlst))))))))

