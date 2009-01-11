;; Copyright 2007-2008, Matthew Welland. matt@iatoa.com All rights reserved.
;; 
;; new_account/control.scm

(load (s:model-path "person"))

(define (new_account:validate-inputs password password-again email-address email-address-again)
  (cond
   ((or (not password)(not password-again)
        (not email-address)(not email-address-again))
    (s:set-err "Form is incomplete. Please fill in all fields and try again")
    #f)
   ((< (string-length password) 2)
    (s:set-err "Password is too short. Please try again")
    #f)
   ((not (string=? password password-again))
    (s:set-err "Passwords do not match. Please try again")
    #f)
   ((> (string-length password) 9)
    (s:set-err "Password is too long. Please try again")
    #f)
   ((not (string=? email-address email-address-again))
    (s:set-err "Email addresses provided do not match. Please try again")
    #f)
   ((and (not (string-match (regexp "^\\s*$") email-address))
         (not (string-match (regexp "^[^@]+@[^@]+\\.[^@]+$") email-address)))
    (s:set-err "Not a valid email address, please try again")
    #f)
   (else #t)))

(define (new_account-action action)
  (case (string->symbol action)
    ('create
     (s:log "Got here, doing create new account")
     (let ((password            (s:get-input 'password))
           (password-again      (s:get-input 'password-again))
           (email-address       (s:string-downcase (s:get-input 'email-address)))
           (email-address-again (s:string-downcase (s:get-input 'email-address-again))))
       ;; save preserved inputs
       (s:set! "email-address" email-address)
       (s:log "Saved inputs. Now check inputs")
       (if (new_account:validate-inputs password password-again email-address 
                                        email-address-again)
           ;; Great!! Now have good inputs
           (if (person:get-dat email-address)
	       (s:set-err "There is already an account for that email address!")
	       (let ((pdat (person:set-password email-address password)))
		 (if pdat
		     (s:set-err "SUCCESS!! You can now log in with " email-address " and your password")
		     (s:set-err "ERROR!! Unable to automatically log you on with the same credentials used to create your account. This shouldn't happen. Please send email to matt@kiatoa.com about this"))))
           ;; bad inputs
           #f)))
    ('else (s:log "Placeholder for future actions. Shouldn't get here"))))
