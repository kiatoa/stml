;;;; Copyright 2007-2008, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;; put (use sqlite3) or (use postgresql) in requirements.scm
(if (file-exists? "requirements.scm")
    (include "requirements.scm"))

;; ONLY A LOWEST COMMON DEMOMINATOR IS SUPPORTED!

;; d = db handle
;; t = statement handle
;; s = statement
;; l = proc
;; p = params
;;
;;          sqlite3                            postgres                   dbi
;; prepare: (sqlite3:prepare d s)              n/a                        NOT YET
;; for-each (sqlite3:for-each-row l d s . p)   (pg:query-for-each l s d)  dbi:for-each-row
;; for-each (sqlite3:for-each-row l t . p)     n/a                        NOT YET
;; exec     (sqlite3:exec d s . p)             (pg:query-tuples s d)      
;; exec     (sqlite3:exec t . p)               n/a

;; set to 'pg or 'sqlite3
;; (define dbi:type 'sqlite3) ;; or 'pg

;;======================================================================
;; D B I
;;======================================================================

(define-record-type dbi:db
  (make-dbi:db dbtype dbconn)
  dbi:db?
      (dbtype    dbi:db-dbtype      dbi:db-dbtype-set!)
      (dbconn    dbi:db-conn        dbi:db-conn-set!))
	
(define (dbi:db-new #!key (dbtype #f) (conn #f))
    (make-dbi:db dbtype conn))

(define (dbi:open dbtype dbinit)
  (if (eq? dbtype 'sqlite3)
      (dbi:db-new dbtype: 'sqlite3 conn: (sqlite3:open (alist-ref 'dbname dbinit)))
      (dbi:db-new dbtype: 'pg      conn: (pg:connect dbinit))))

;; '((dbname . "test.db"))
(define (dbi:for-each-row proc dbh stmt . params)
  (let ((dbtype (dbi:db-dbtype dbh))
	(conn   (dbi:db-conn dbh)))
    (if (eq? dbtype 'sqlite3)
	(sqlite3:for-each-row 
	 (lambda (first . remaining)
	   (let ((tuple (list->vector (cons first remaining))))
	     (proc tuple)))
	 conn
	 (apply dbi:sqlparam stmt params))
	(pg:query-for-each proc (apply dbi:sqlparam stmt params) conn))))

;; common idiom is to seek a single value, #f if no match
(define (dbi:get-one dbh stmt . params)
  (let ((res #f))
    (apply dbi:for-each-row
	   (lambda (row)
	     (set! res (vector-ref row 0)))
	   dbh
	   stmt 
	   params)
    res))

;; common idiom is to seek a single value, #f if no match
(define (dbi:get-one-row dbh stmt . params)
  (let ((res #f))
    (apply dbi:for-each-row
	   (lambda (row)
	     (set! res row))
	   dbh
	   stmt 
	   params)
    res))

;; common idiom is to seek a list of rows, '() if no match
(define (dbi:get-rows dbh stmt . params)
  (let ((res '()))
    (apply dbi:for-each-row
	   (lambda (row)
	     (set! res (cons row res)))
	   dbh
	   stmt 
	   params)
    (reverse res)))

(define (dbi:exec dbh stmt . params)
  (let ((dbtype (dbi:db-dbtype dbh))
	(conn   (dbi:db-conn dbh))
	(junk   #f))
    (if (eq? dbtype 'sqlite3)
	(apply sqlite3:exec conn stmt params)
	(pg:query-for-each (lambda (tuple) (set! junk tuple) )
			   (apply dbi:sqlparam stmt params)
			   conn))))

(define (dbi:close dbh)
  (let ((dbtype (dbi:db-dbtype dbh))
	(conn   (dbi:db-conn dbh)))
    (if (eq? dbtype 'sqlite3)
	(sqlite3:finalize! conn)
	(pg:close conn))))

;;======================================================================
;; D B   M I S C
;;======================================================================

(define (dbi:escape-string str)
      (let ((parts (dbi:split-string str "'")))
	(string-intersperse parts "''")))
;;      (pg:escape-string val)))

;; convert values to appropriate strings
;;
(define (dbi:sqlparam-val->string val)
  (cond
   ((list?   val)(string-join (map symbol->string val) ",")) ;; (a b c) => a,b,c
   ((string? val)(string-append "'" (dbi:escape-string val) "'"))
   ((number? val)(number->string val))
   ((symbol? val)(pg:escape-string (symbol->string val)))
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
(define (dbi:sqlparam query . args)
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
              (let* ((valstr    (dbi:sqlparam-val->string arg))
                     (newresult (string-append result section valstr)))
                (if (null? argtail) ;; we are done
                    (string-append newresult (car tail))
                    (loop
                     (car tail)
                     (cdr tail)
                     newresult
                     (car argtail)
                     (cdr argtail)))))))))

;; a poorly written but non-broken split-string
;;
(define (dbi:split-string strng delim)
  (if (eq? (string-length strng) 0) (list strng)
      (let loop ((head (make-string 1 (car (string->list strng))))
		 (tail (cdr (string->list strng)))
		 (dest '())
		 (temp ""))
	(cond ((equal? head delim)
	       (set! dest (append dest (list temp)))
	       (set! temp ""))
	      ((null? head) 
	       (set! dest (append dest (list temp))))
	      (else (set! temp (string-append temp head)))) ;; end if
	(cond ((null? tail)
	       (set! dest (append dest (list temp))) dest)
	      (else (loop (make-string 1 (car tail)) (cdr tail) dest temp))))))


#|

;; sqlite3
(use sqlite3)
(system "rm -f tests/test.db")
(load "dbi.scm")
(dbi:open 'sqlite3 '((dbname . "tests/test.db")))
(dbi:exec db "CREATE TABLE foo(id INTEGER PRIMARY KEY,name TEXT);")
(dbi:exec db "INSERT INTO foo(name) VALUES(?);" "Matt")
(dbi:for-each-row 
 (lambda (tuple)
   (print (vector-ref tuple 0) " " (vector-ref tuple 1)))
 db "SELECT * FROM foo;")
(dbi:close db)
(system "rm -f tests/test.db")

;; postgresql
(system "dropdb test")
(system "createdb test")
(use postgresql)
(load "mrwdbi.scm")
(define db (dbi:open 'pg '((dbname   . "test")
			   (user     . "matt")
			   (password . "Nada")
			   (host     . "localhost"))))
(dbi:exec db "CREATE TABLE foo(id SERIAL  PRIMARY KEY,name TEXT);")
(dbi:exec db "INSERT INTO foo(name) VALUES(?);" "Matt")
(dbi:for-each-row 
 (lambda (tuple)
   (print (vector-ref tuple 0) " " (vector-ref tuple 1)))
 db "SELECT * FROM foo;")
(dbi:close db)
(system "dropdb test")

|#
