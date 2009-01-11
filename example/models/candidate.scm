;; Copyright 2007-2008, Matthew Welland. matt@kiatoa.com All rights reserved.
;; 
;; models/candidates.scm
;;

(define (candidate:get-top n)
  (dbi:get-rows 
   (s:db) 
   "SELECT DISTINCT id,name,url,party,desc,supports_av,date_added,score,pscore FROM candidates AS c ORDER BY score DESC LIMIT ?;" n))

;; HERE !!!! getting vote counts... DONT'USE- SEE VOTED INSTEAD
(define (candidate:get-votes candidates vote_type)
  (let ((ids (map (lambda (c)(candidate:get-id c)) candidates)))
    (dbi:get-rows (s:db)
		  (conc
		   "SELECT id,sum(votes*(1+score)) WHERE vote_date>"
		   (- (current-time) (* 24 60 60 7)) ;; seven days
		   " AND id IN "
		   (apply conc (intersperse ids ","))))))
		   
(define (candidate:get-by-name name)
  (dbi:get-one-row (s:db) "SELECT id,name,url,party,desc,supports_av,date_added,score,pscore FROM candidates WHERE name=?;" name))

;; update an existing candidate or create if new
(define (candidate:update dat)
  (let* ((name   (candidate:get-name dat))
	 (olddat (candidate:get-by-name name)))
    (if olddat
	(begin
	  (dbi:exec (s:db) 
		    "UPDATE candidates SET url=?,party=?,desc=?,supports_av=? WHERE name=?;"
		    (candidate:get-url   dat)
		    (candidate:get-party dat)
		    (candidate:get-desc  dat)
		    (candidate:get-supports-av dat)
		    name)
	  (candidate:get-by-name name))
	(begin
	  (dbi:exec (s:db)
		    "INSERT INTO candidates (name,url,party,desc,supports_av) VALUES(?,?,?,?,?);"
		    name
		    (candidate:get-url   dat)
		    (candidate:get-party dat)
		    (candidate:get-desc  dat)
		    (candidate:get-supports-av dat))
	  (candidate:get-by-name name)))))


(define (candidate:get-id           dat)(vector-ref dat 0)) 
(define (candidate:get-name         dat)(vector-ref dat 1)) 
(define (candidate:get-url          dat)(vector-ref dat 2))
(define (candidate:get-party        dat)(vector-ref dat 3))
(define (candidate:get-desc         dat)(vector-ref dat 4))
(define (candidate:get-supports-av  dat)(vector-ref dat 5))
(define (candidate:get-date-added   dat)(vector-ref dat 6))
(define (candidate:get-score        dat)(vector-ref dat 7))
(define (candidate:get-pscore       dat)(vector-ref dat 8))

(define (candidate:set-id!          dat val)(vector-set! dat 0 val)) 
(define (candidate:set-name!        dat val)(vector-set! dat 1 val)) 
(define (candidate:set-url!         dat val)(vector-set! dat 2 val))
(define (candidate:set-party!       dat val)(vector-set! dat 3 val))
(define (candidate:set-desc!        dat val)(vector-set! dat 4 val))
(define (candidate:set-supports-av! dat val)(vector-set! dat 5 val))
(define (candidate:set-date-added!  dat val)(vector-set! dat 6 val))
(define (candidate:set-score!       dat val)(vector-set! dat 7 val))

