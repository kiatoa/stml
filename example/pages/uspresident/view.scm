;; Copyright 2007-2008, Matthew Welland. Kiatoa.com All rights reserved.
;; 

;; Note: the (list is actually no longer needed. 

(list
 (s:if-sessionvar 
  "email"
  (s:if-sessionvar 
   "voted"
   "We are glad you tried approval voting. Try again to see how the system works. Don't worry about the poll numbers. This poll is for you to play with."))
 (s:fieldset
  "Poll"
  (s:center
   (s:if-param "errmsg"
	       (let ((err (s:get "errmsg")))
		 (s:del! "errmsg")
		 (s:err-font err)))
   (s:form  'action "uspresident.vote"
	    'method "post"
	    (s:table 'border "1" 'cellspacing "0"
		     (s:tr 
		      (s:td "Candidate")(s:td "Party")(s:td "Supports approval?")
		      (s:if-sessionvar 
		       "voted"
		       (list (s:td "Plurality")
			     (s:td "Approval")
			     (s:td "Plurality" (conc "(" candidates:vote-sum-plurality "votes" ")"))
			     (s:td "Approval"  (conc "(" candidates:vote-sum-plurality "votes" ")")))
		       (list (s:td "Plurality (vote for one only)")(s:td "Approval (vote for all which you approve of)"))))
		     ;; map the poll items for each row
		     (map (lambda (candidate)
			    (let ((poll-item-id          (number->string (candidate:get-id candidate)))
				  (poll-item-url         (s:tidy-url (candidate:get-url candidate)))
				  (poll-item-name        (candidate:get-name candidate))
				  (poll-item-description (candidate:get-desc candidate))
				  (poll-item-percent-a   (quotient (* 100 (candidate:get-score candidate)) candidates:vote-sum-plurality))
				  (poll-item-percent-p   (quotient (* 100 (candidate:get-pscore candidate)) candidates:vote-sum-plurality)))
			      (list
			       (s:tr
				(s:td
				 (if poll-item-url
				     (s:a 'href poll-item-url 'target "_blank" poll-item-name)
				     poll-item-name))
				;; (if (poll:poll 'have-description?)
				;;     (s:td 'bgcolor "#f0f0f0" poll-item-description) ;; description
				;;     '())
				(s:td (candidate:get-party       candidate))
				(s:td (candidate:get-supports-av candidate))
				;; (if (not (s:session-var-get "voted")) ;; here are the check buttons for plurality and approval voting
				;;    (list 
				(s:td (s:center
				       (s:input 'type "radio"    'name "plurality" 'value poll-item-id)))
				(s:td (s:center
				       (s:input 'type "checkbox" 'name "approval"  'value poll-item-id)))
				(s:if-sessionvar "voted"
						 (list
						  (s:td (conc poll-item-percent-p "%") 'bgcolor (if (eq? (candidate:get-id candidate)  candidates:top-plurality-id)
												    "cyan"
												    "lightgrey")
							(conc "(" (candidate:get-pscore candidate) ")") 'align "center")
						  (s:td (conc poll-item-percent-a "%")  'bgcolor (if (eq? (candidate:get-id candidate)  candidates:top-approval-id)
												    "cyan"
												    "lightgrey")
							(conc "(" (candidate:get-score candidate) ")")  'align "center"))))))) ;; % votes
			  candidates)
		     (s:tr 
		      (s:td "Write in (name):<br>"
			    (s:input-preserve 'type "text" 'name "poll_name"  'size "15" 'maxlength "40"))
		      (s:td "Party:<br>" (s:input-preserve 'type "text" 'name "poll_party" 'size "10" 'maxlength "40"))
		      (s:td "Supports approval:<br>" (s:input-preserve 'type "text" 'name "poll_supports_av"  'size "10" 'maxlength "40"))
		      (s:td "Url:<br>"   (s:input-preserve 'type "text" 'name "poll_url"   'size "40" 'maxlength "120") 'colspan 4))
		     (s:tr
		      (s:td 'colspan 7
			    (s:center (s:input 'type "submit" 'name "vote" 'value "Vote") 
				      (s:if-sessionvar "email"
						       '()
						       (list
							"Email or nickname:" 
							(s:input-preserve 'type "text" 'name "users_email" 'size 20 'maxlength 40)
							"(required), Country code:"
							(s:input-preserve 'type "text" 'name "users_country_code" 'size 2 'maxlength 2)
							"(optional)"
							))
				       ))))))))