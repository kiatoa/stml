;; Copyright 2007-2008, Matthew Welland. Kiatoa.com All rights reserved.
;; 
;; leftnav/view.scm

(list
    (s:div 
     'class "node"
     (s:h1 "Navigation")
     (let ((section (slot-ref s:session 'page)))
       (cond
	((or (not section) ;; this is home
	     (string=? section "home"))
	 "Home menu")
	((string=? section "discussions")
	 (list
	  (s:a "Filter"         'href (s:link-to "discussions" 'filter "on"))))
	((string=? section "learn")
	 (list
	  (s:a "Learn"  'href (s:link-to "learn"  'action "learn.teach"))(s:br)
	  (s:a "Test"   'href (s:link-to "learn"  'action "learn.test"))(s:br)
	  ))
	((string=? section "preferences")
	 (list 
	  (s:a "Password"       'href (s:link-to "preferences" 'action "password"))(s:br)
	  (s:a "Messages"       'href (s:link-to "preferences" 'action "messages"))(s:br)
	  (s:a "Preferences"    'href (s:link-to "preferences" 'action "preferences"))(s:br)))
	(else '( "nada" ))))
     (s:br))
    (s:div
     'class "node"
     (s:h1 "About you")
     (let ((email (s:session-var-get "email")))
       (if email
           (list email (s:br))
           "Not logged in")))
    (s:div
     'class "node"
     (s:call "pledge")))
