;; Copyright 2007-2008, Matthew Welland. Kiatoa.com All rights reserved.
;; 
;; header/control.scm

;; (load (s:model-path "blah"))
(define header:menu-items '(("home" "Home")("learn" "Learn")("action" "Take Action")("discussion" "Discussion")
			    ("preferences" "Preferences")))
(define header:title (let ((t (s:get-param 'section)))
		       (if t t "Home")))
