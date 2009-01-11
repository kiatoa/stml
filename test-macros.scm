;; (require-extension syntax-case)
;; (declare (run-time-macros))
;; 
;; (define-syntax s:if
;;   (syntax-rules ()
;;     [(_ s x) (if (equal? s "yep") x '())]
;;     [(_ s x y) (if  (equal? s "yep") x y)]))

;; (define-macro (s:if varname . dat)
;;   (if (null? dat)
;;       '()
;;       (print dat)))
;;       (let ((a   (car dat))
;; 	    (rem (cadr dat)))
;; 	(if (equal? "yep" varname)
;; 	    a
;; 	    (if (null? rem)
;; 		rem
;; 		(car rem))))))

;; (s:if "yep" (print "Yep")(print "nope"))
     ;; this seemed to work, but
;;      (let* ((inps (map open-input-file (list "/home/matt/kiatoa/stml/sugar.scm" view)))
;; 	    (p    (apply make-concatenated-port inps))
;; 	    (res  (map 
;; 		   (lambda (x)
;; 		     (cond
;; 		      ((list? x) x)
;; 		      ((string? x) x)
;; 		      (else '())))
;; 		   (port-map eval (lambda ()(read p))))))
;;        (map close-input-port inps)
;;        res)
(declare (run-time-macros))
(include "sugar")
(define foo #f)
(let* ((inps (map open-input-file (list "test-of-macro.scm"))) ;;"sugar.scm" 
       (p    (apply make-concatenated-port inps)))
  (print ;; (port-map eval (lambda ()(read p))))
   (map 
    (lambda (x)
      (cond
       ((list? x) x)
       ((string? x) x)
       (else '())))
    (port-map eval (lambda ()(read p)))))
  (map close-input-port inps))

;; ==(define (files-read->string . files)
;; ==  (string-intersperse 
;; ==   (apply append (map file-read->string files)) "\n"))
;; ==
;; ==(define (file-read->string f) 
;; ==  (let ((p (open-input-file f)))
;; ==    (let loop ((hed (read-line p))
;; ==	       (res '()))
;; ==      (if (eof-object? hed)
;; ==	  res
;; ==	  (loop (read-line p)(append res (list hed)))))))
;; ==
;; ==(print (let ((inp (open-input-string 
;; ==		   (files-read->string "/home/matt/kiatoa/stml/sugar.scm" 
;; ==				       "test-of-macro.scm"))))
;; ==	 (map 
;; ==	  (lambda (x)
;; ==	    (cond
;; ==	     ((list? x) x)
;; ==	     ((string? x) x)
;; ==	     (else '())))
;; ==	  (port-map eval (lambda ()
;; ==			   (read inp))))))

;; (define files (list "sugar.scm" "test-of-macro.scm"))
;; 
;; (define (file-read->list f)
;;   (let ((p (open-input-file f)))
;;     (let loop ((hed (read p))
;; 	       (res '()))
;;       (if (eof-object? hed)
;; 	  res
;; 	  (loop (read p)(append res (list hed)))))))
;; 
;; (define indat (append (file-read->list "sugar.scm")(file-read->list "test-of-macro.scm")))
;; (eval indat)

;; (make-concatenated-port 