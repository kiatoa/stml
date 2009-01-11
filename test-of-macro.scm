;; (require-extension syntax-case)

;; (define-syntax s:if
;;   (syntax-rules ()
;;     [(_ s x)   (if (equal? s "yep") x '())]
;;     [(_ s x y) (if  (equal? s "yep") x y)]))
;; 
;; == (list
;; ==  "foo: " foo "s:if-test "
;; ==  (s:if-test "blah"
;; ==        (begin
;; ==  	 (set! foo #t)
;; ==  	 "yep"))
;; ==  "foo: " foo "s:if-test "
;; ==  (s:if-test "yep"
;; ==        (begin
;; ==  	 (set! foo #t)
;; ==  	 "yep")))
;; == 
;; == (list foo)
(let ((nada 1))
  (inc! nada)
  (list nada
	(s:if-test "nada" (print "fuck") "noi")))