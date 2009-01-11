;; Copyright 2007-2008, Matthew Welland. Kiatoa.com All rights reserved.
;; 
;; header/view.scm
;;
(list
 ;; (s:div 'id "titlebar"
	(s:table
	 (s:tr
	  (s:td (s:img 'src "/www/images/approvalvote.png" 
		       'alt "ApprovalVote.com" 
		       'title "Welcome to ApprovalVote.com"))
	  (s:td 'valign "top" 'align "right"
		(s:table 'border "0" 'cellspacing "0"
			 (s:tr 
			  (s:td 'valign "center" ;; 'width "250" ;; 'rowspan "2"
				(s:a (s:small " *      NOW IS A GREAT TIME TO PUSH FOR APPROVAL VOTING!     * "))
				(s:br)))
			 (s:tr 
			  (s:td 'columnspan="3" 
				(s:center "*********")))))) ;;  header:title))))))
	 ;; this is the horizontal menus
	 (s:tr 'columnspan "4"
	       (s:table
		(s:tr
		  (map (lambda (m-item)
			 (s:td (s:small  "[" 
					 (s:a 'href (s:link-to (car m-item))(cadr m-item))
					 "]")))
		       header:menu-items)
		  )))));; )
