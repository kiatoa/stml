;; Copyright 2007-2008, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;

;;======================================================================
;; Sugar
;;======================================================================
;;
;; (require 'syntax-case)
;; 
;; (define-syntax s:if-param
;;   (syntax-rules ()
;;     [(_ s x)   (if (s:get s) x (s:comment "s:if not"))]
;;     [(_ s x y) (if (s:get s) x y)]))
;; ;; 
;; (define-syntax s:if-test
;;   (syntax-rules ()
;;     [(_ s x) (if   (string=? "yep" s)   x (list "s:if not"))]
;;     [(_ s x y) (if (string=? "yep" s) x y)]))


(define-macro (s:if-param varname . dat)
  (match dat
	 (()    '())
	 ((a)    `(if (s:get ,varname) ,a '()))
	 ((a b)  `(if (s:get ,varname) ,a ,b))))

(define-macro (s:if-sessionvar varname . dat)
  (match dat
	 (()    '())
	 ((a)    `(if (s:session-var-get ,varname) ,a '()))
	 ((a b)  `(if (s:session-var-get ,varname) ,a ,b))))

