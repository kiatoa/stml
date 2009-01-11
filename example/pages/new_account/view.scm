;; Copyright 2007-2008, Matthew Welland. Kiatoa.com All rights reserved.
;; 
;; new_account/view.scm
;;
(list 
 (s:div 'class "node"
        ;; (s:p (s:get-err s:strong)) ;; error message
        (s:p "")(s:p (s:get-err s:err-font))
        (if (not (s:session-var-get "email")) ;; setting email defines "logged in"
            (s:form 'action "new_account.create" 'method "post"
                    (s:table 'border "0" 'spacing "0"
                             
                             (s:tr (s:td (s:strong "Email address:")) ;; (s:br)
                                   (s:td (s:input-preserve 'type "text" 'name "email-address" 'size "16" 'maxlength "30"))) ;; (s:br)
                             
                             (s:tr (s:td (s:strong "Email address again:")) ;; (s:br)
                                   (s:td (s:input-preserve 'type "text" 'name "email-address-again" 'size "16" 'maxlength "30"))) ;; (s:br)
                    
                             (s:tr (s:td (s:strong "Password:")) ;; (s:br)
                                   (s:td (s:input 'type "password" 'name "password" 'size "16" 'maxlength "16"))) ;; (s:br)
                             
                             (s:tr (s:td (s:strong "Password again:")) ;; (s:br)
                                   (s:td (s:input 'type "password" 'name "password-again" 'size "16" 'maxlength "16")))); (s:br)
                             
                    (s:input 'type "submit"   'name "form-name"    'value "submit"))
            (s:h1 "Welcome " (s:session-var-get "email") ":" (s:session-var-get "location") "!"))))
 