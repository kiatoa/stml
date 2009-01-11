;; Copyright 2007-2008, Matthew Welland. matt@kiatoa.com All rights reserved.
;; 
;; models/person.scm
;;
(require "md5")

(define (person:get-dat email)
  (dbi:get-one-row (s:db) "SELECT id,name,email,status,password,score FROM people WHERE email=?;" email))

;; this effectively auto logs in using "" as the password
(define (person:create-or-get email)
  (let ((dat (person:get-dat email)))
    (if dat
	(person:authenticate email "")
	(person:set-password email ""))))

(define (person:password-match? password cryptedpw)
  (string=? (md5:digest password) cryptedpw))

(define (person:authenticate email password)
  (let ((pdat (person:get-dat email)))
    (if pdat
	;; (if (s:password-match? password (vector-ref pdat 4))
	(if (person:password-match? password (vector-ref pdat 4))
	    pdat ;; password matched, return basic record id,name,email,status
	    #f)
	#f)))

;; sets password, creates user if doesn't exist
(define (person:set-password email password)
  (let ((pdat (person:get-dat email))
	;; (cpwd (s:crypt-passwd password #f)))
        (cpwd (md5:digest password)))
    (if pdat
	(dbi:exec (s:db)
		  "UPDATE people SET password=? WHERE email=?;" 
		  cpwd
		  email)
	(dbi:exec (s:db)
		  "INSERT INTO people (name,email,password) VALUES(?,?,?);"
		  ""
		  email
		  cpwd))
    (if pdat 
	pdat
	(person:get-dat email))))

(define (person:learn_enabled? email)
  (eq? (dbi:get-one (s:db) "SELECT status FROM people WHERE email=?;" email)
       1))

(define(person:files_enabled? email)
  #f)

;; id,name,email,status,password,score
(define (person:get-id       dat)(vector-ref dat 0))
(define (person:get-name     dat)(vector-ref dat 1))
(define (person:get-email    dat)(vector-ref dat 2))
(define (person:get-status   dat)(vector-ref dat 3))
(define (person:get-password dat)(vector-ref dat 4))
(define (person:get-score    dat)(vector-ref dat 5))

(define (person:set-id!       dat val)(vector-set! dat 0 val))
(define (person:set-name!     dat val)(vector-set! dat 1 val))
(define (person:set-email!    dat val)(vector-set! dat 2 val))
(define (person:set-status!   dat val)(vector-set! dat 3 val))
(define (person:set-password! dat val)(vector-set! dat 4 val))
(define (person:set-score!    dat val)(vector-set! dat 5 val))
