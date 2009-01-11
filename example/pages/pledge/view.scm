;; Copyright 2007-2008, Matthew Welland. matt@iatoa.com All rights reserved.
;; 
(s:if-sessionvar 
 "email"
 (list
  (s:h1 "Pledge now!")
  (s:fieldset 
   "Pledge"
   (s:form 'action "pledge.pledge"
	   'method "post"
	   (s:i " - I will vote" (s:b "ONLY") " for a candidate who supports approval voting!")
	   (s:table
	    (s:tr (s:td "Yes")  (s:td (s:input 'type "radio"    'name "pledge_answer" 'value "yes")))
	    (s:tr (s:td "No")   (s:td (s:input 'type "radio"    'name "pledge_answer" 'value "no")))
	    (s:tr (s:td "Maybe")(s:td (s:input 'type "radio"    'name "pledge_answer" 'value "maybe"))))
	   (s:input 'type "button" 'name "pledge_answer" 'value "Submit")))))