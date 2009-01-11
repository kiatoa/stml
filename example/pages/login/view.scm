;; Copyright 2007-2008, Matthew Welland. Kiatoa.com All rights reserved.
;; 
;; Login view

(s:div 'class "node"
       ;; (s:p (s:get-err s:strong)) ;; error message
       (if (s:session-var-get "email") 
           (s:a "Log out" 'href (s:link-to (s:current-page) 'action "login.logout"))
           (list 
            (s:center (s:p (s:strong "Log in here!")))
	    (let ((msg (s:get "msg")))
	      (if msg
		  (begin
		    (s:del! "msg")
		    (s:err-font msg))
		  (s:null "")))
            (s:form 'action "login.login" 'method "post"
                    (s:strong "Id: (*)")(s:br)
                    (s:input-preserve 'type "text" 'name "email-address" 'size "14" 'maxlength "30")(s:br)
                    (s:strong "Password:")(s:br)
                    (s:input 'type "password" 'name "password" 'size "14" 'maxlength "30")(s:br)
                    (s:input 'type "submit"   'name "form-name"    'value "login")(s:br)
                    (s:a "Create account" 'href (s:link-to "new_account"))
                    ))))