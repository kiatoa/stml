;; Copyright 2007-2008, Matthew Welland. Kiatoa.com All rights reserved.
;; 
;; index

(list
 (s:html
  (s:head
   (s:title "Approval Voting Now!")
   (s:link  'rel "stylesheet" 'type "text/css" 'href "/approvalvote/markup.css")
   (s:link  'rel "stylesheet" 'type "text/css" 'href "/approvalvote/layout.css"))
  (s:body
   (s:div 'class "header"       (s:call "header"))
   (s:div 'class "rightcolumn"  (s:call "rightcol"))
   (s:div 'class "leftcolumn"   (s:call "leftnav"))
   (s:div 'class "centercolumn"
          (let ((page    (slot-ref s:session 'page)))
            (if page
                (s:call page)
                (list (s:h2 "Home")
		      (s:call "sys-state")))))
   (s:div 'class "footer" (s:call "footer")))))
