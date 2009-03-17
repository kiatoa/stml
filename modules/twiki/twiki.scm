;; twiki module

(define (twiki:view)
  (s:div 'class "node"
  (s:h1 "Twiki")
  "Title, pictures, etc."
   (let ()
     "blah")))


(define (twiki:wiki . keys)
  (let ((key (conc keys)))
    (twiki:view)))
