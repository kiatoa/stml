;; Copyright 2007-2008, Matthew Welland. Kiatoa.com All rights reserved.
;; 
;; this gets read for ALL pages. Don't weigh it down excessively!
;;
;; uspresident/control.scm

(s:load-model "candidate")
(s:load-model "voting")
(s:load-model "person")

(define candidates (candidate:get-top 10))
(define candidates:vote-sum-approval  (apply + (map candidate:get-score candidates)))
(define candidates:vote-sum-plurality (apply + (map candidate:get-pscore candidates)))
(define candidates:top-plurality-id   (let ((id       #f)
					    (topscore 0))
					(for-each (lambda (cand)
						    (if (> (candidate:get-pscore cand) topscore)
							(begin 
							  (set! topscore (candidate:get-pscore cand))
							  (set! id       (candidate:get-id cand)))))
						  candidates)
					id))
(define candidates:top-approval-id   (let ((id       #f)
					   (topscore 0))
				       (for-each (lambda (cand)
						   (if (> (candidate:get-score cand) topscore)
						       (begin 
							 (set! topscore (candidate:get-score cand))
							 (set! id       (candidate:get-id cand)))))
						 candidates)
				       id))
							   

(define (uspresident-action action)
  (let ((acsym (string->symbol action)))
    (cond
     ('vote
      (let ((button (s:get-input 'vote)))
	(cond
	 ((equal? button "Vote")
	  (let* ((approval    (s:get-input 'approval))
		 (plurality   (s:get-input 'plurality))
		 (newdat      (make-vector 9 ""))
		 (email       (s:session-var-get "email"))
		 (newcandname (s:get-input 'poll_name))
		 (nick-email  (if email email (s:get-input 'users_email))))
	    (if (not (list? approval))
		(set! approval (list approval)))
	    (if (string-match (regexp "^[a-zA-Z]+") newcandname)
		(let* ((dat (candidate:get-by-name newcandname)))
		  (if dat ;; i.e. this is a new candidate
		      (set! newdat dat)
		      (begin
			(candidate:set-name! newdat newcandname)
			(candidate:set-supports-av! newdat (s:get-input 'poll_supports_av))
			(candidate:set-party! newdat (s:get-input 'poll_party))
			(candidate:set-url! newdat (s:get-input 'poll_url))
			(set! newdat (candidate:update newdat))))
		  (s:log "cid: " (candidate:get-id newdat))
		  (set! approval  (cons (candidate:get-id newdat) approval))
		  (set! plurality (candidate:get-id newdat))))
	    (set! approval (filter (lambda (x)(or (number? x)(string? x))) approval)) ;; clean the approval list
	    (s:log "using email: " nick-email)
	    (s:log "approval: " approval)
	    (s:log "plurality: " plurality)
	    (if (and approval plurality (not (null? approval)))
		(begin
		  (voting:handle-votes nick-email
				       approval
				       plurality)
		  (s:session-var-set! "voted" "yes"))
		(s:set! "errmsg" "Please select one plurality vote and one or more approval votes"))))))))))
