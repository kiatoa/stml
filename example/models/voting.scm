;; Copyright 2007-2008, Matthew Welland. matt@kiatoa.com All rights reserved.
;; 
;; models/voting.scm
;;
;; store the votes!

;; look up the entry to which to add 
(define (voting:get-entry-id candidate-id score type)
  (dbi:get-one (s:db) "SELECT id FROM votes WHERE candidate_id=? AND score=? AND vote_type=? AND vote_date>?;"
	       candidate-id
	       score
	       type
	       (- (current-seconds) 86400))) ;; i.e. since 24 hrs ago
  
(define (voting:apply-vote dat candidate-id vote-type)
  (let* ((score (person:get-score dat))
	 (vote-entry-id (voting:get-entry-id candidate-id score vote-type)))
    (if vote-entry-id
	(dbi:exec (s:db) "UPDATE votes SET votes=votes+1 WHERE id=?;" vote-entry-id)
	(dbi:exec (s:db) "INSERT INTO votes (candidate_id,vote_date,votes,score,vote_type) VALUES(?,?,?,?,?);" 
		  candidate-id
		  (current-seconds)
		  1
		  score
		  vote-type))))

(define (voting:rollup-votes)
  (let ((adat (dbi:get-rows (s:db) 
			    "SELECT candidate_id AS id,SUM(votes*(score+1)) AS score FROM votes WHERE vote_date>? AND vote_type=1 GROUP BY candidate_id;"
			    (- (current-seconds) (* 24 60 60 7))))
	(pdat (dbi:get-rows (s:db) 
			    "SELECT candidate_id AS id,SUM(votes*(score+1)) AS score FROM votes WHERE vote_date>? AND vote_type=0 GROUP BY candidate_id;"
			    (- (current-seconds) (* 24 60 60 7)))))
    (for-each
     (lambda (row)
       (dbi:exec (s:db) "UPDATE candidates SET score=? WHERE id=?;" (vector-ref row 1)(vector-ref row 0)))
     adat)
    (for-each
     (lambda (row)
       (dbi:exec (s:db) "UPDATE candidates SET pscore=? WHERE id=?;" (vector-ref row 1)(vector-ref row 0)))
     pdat)))

;; vote_type: 0=plurality, 1=approval
(define (voting:handle-votes email approval plurality)
  (let* ((pdat (let ((e (s:session-var-get "email")))
		 (if e 
		     (person:get-dat e)
		     (person:create-or-get (if (or (not (string? email)) 
						   (string-match (regexp "^\\s*$") email))
					       "noname" 
					       email)))))) ;; is this really the logic I wanted?
    ;; (s:log "Got here eh!" " pdat: " pdat)
    (if (not pdat)
	(s:set! "errmsg" "Failed to auto log in/register, email or nick already in use. Consider reseting your password")
	(begin
	  (s:session-var-set! "email" (person:get-email pdat))
	  (voting:apply-vote pdat plurality 0)
	  (map (lambda (candidate-id)
		 (voting:apply-vote pdat candidate-id 1))
	       approval)
	  (voting:rollup-votes)))))