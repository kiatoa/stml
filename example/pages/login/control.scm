;; Copyright 2007-2008, Matthew Welland. Kiatoa.com All rights reserved.
;; 
(load (s:model-path "person"))

(define (login-action action)
  (case (string->symbol action)
    ('login
     ;; the actual login code
     (s:log "Got here, doing login")
     (let ((email  (s:get-input 'email-address))
           (passwd (s:get-input 'password)))
	   ;; (person (make-person))) ;; DO WE NEED A PERSON "OBJECT"?
       (s:set! "email-address" email) ;; preserve user as email-address
       (if (and email passwd)
	   (let ((good-login (person:authenticate email passwd)))
	     (if good-login
		 (begin
		   (s:set! "msg" "Login successful!")
		   (s:session-var-set! "email" email))
		 (s:set! "msg" "Bad password or email. Please try again")))
	   (s:set! "msg" "Missing password or email"))))
    ('logout
     (s:delete-session))
    ('nada
     (s:log "Got here, action=" action))))
