;; Copyright 2007-2008, Matthew Welland. Kiatoa.com All rights reserved.
;; 
;; maint/view.scm
;;
(if (maint:am-i-maint?)
    (list
     (s:h1 "Hello Maint!")
     (s:p (s:a "Update Tables" 'href (s:link-to (s:current-page) 
					'action "maint.update_tables"))))
    '())

	