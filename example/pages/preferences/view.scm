;; Copyright 2007-2008, Matthew Welland. matt@iatoa.com All rights reserved.
;; 
;; preferences/view.scm
;;
(s:div
 'class "node"
 (s:h1 "Register your email address")
 (s:p "Adds 9 pts to your score the first time you do it and enables very occasional email updates. If you change your email address
       you need to re-register to keep your 9 pts.")
 (s:form 'action "preferences.register_email"
	 'method "post"
	  (s:input 'type "submit" 'name "register_email" 'value "Register Email"))) 