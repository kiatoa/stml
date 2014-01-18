;; Copyright 2007-2011, Matthew Welland.
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

;; Some of these routines use:
;;
;;     http://www.cs.toronto.edu/~gfb/scheme/simple-macros.html
;;
;; Syntax for defining macros in a simple style similar to function definiton,
;;  when there is a single pattern for the argument list and there are no keywords.
;;
;; (define-simple-syntax (name arg ...) body ...)
;;

(define-syntax define-simple-syntax
  (syntax-rules ()
    ((_ (name arg ...) body ...)
     (define-syntax name (syntax-rules () ((name arg ...) (begin body ...)))))))

;;======================================================================
;; syntatic sugar items
;;======================================================================

;; We often seem to want to include stuff if a conditional is met
;; otherwise not include it. This routine makes that slightly cleaner
;; since using a pure if results in #<undefined> objects. (admittedly they 
;; should be ignored but this is slightly cleaner I think). 
;;
;; NOTE: This has to be a macro or the true clause will be evaluated 
;; whether "a" is true or false

;; If a is true return b, else return '()
(define-simple-syntax (s:if a b)
  (if a b '()))


;; Using the Simple-Syntax System
;; 
;; The syntax for defining macros in this system is similar to that for defining functions. In fact if the macro has a fixed number of arguments the syntax is identical. For example:
;; 
;;   ; Define a simple macro to add a value to a variable.
;;   ;
;;   (define-simple-syntax (+= variable value)
;;     (set! variable (+ variable value)))
;; 
;;   ; Use it.
;;   ;
;;   (define v 2)
;;   (+= v 7)
;;   v ; => 9
;; 
;; For a fixed number of arguments followed by an unknown number of arguments we use ... after a single argument to represent the unknown number (possibly zero) of arguments. For example, let's revise our definition of += to allow zero or more values to be added:
;; 
;;   ; Define a simple macro to add a zero or more values to a variable
;;   ;
;;   (define-simple-syntax (+= variable value ...)
;;     (set! variable (+ variable value ...)))
;; 
;;   ; Use it
;;   ;
;;   (define v 2)
;;   (+= v 7)
;;   v ; => 9
;;   (+= v 3 4)
;;   v ; => 16
;;   (+= v)
;;   v ; => 16
;; 


;; (define-macro (s:if-param varname . dat)
;;   (match dat
;; 	 (()    '())
;; 	 ((a)    `(if (s:get ,varname) ,a '()))
;; 	 ((a b)  `(if (s:get ,varname) ,a ,b))))
;; 
;; (define-macro (s:if-sessionvar varname . dat)
;;   (match dat
;; 	 (()    '())
;; 	 ((a)    `(if (s:session-var-get ,varname) ,a '()))
;; 	 ((a b)  `(if (s:session-var-get ,varname) ,a ,b))))
;; 
