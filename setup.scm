;; Copyright 2007-2008, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;; 
(define s:session (make <session>))

;; use this for getting data from page to page when scope and evals
;; get in the way
(define s:local-vars (make-hash-table))

(define (s:local-set! k v)
  (hash-table-set! s:local-vars k v))

(define (s:local-get k)
  (hash-table-ref/default s:local-vars k #f))

(session:setup s:session)

(define (s:log . msg)
  (apply session:log s:session msg))

(session:get-vars s:session)

(define (s:set-err . args)
  (slot-set! s:session 'curr-err args))

;; Usage: (s:get-err s:big)
(define (s:get-err wrapperfunc)
  (let ((errmsg (slot-ref s:session 'curr-err)))
    (if errmsg ((if wrapperfunc
                    wrapperfunc
                    s:strong) errmsg) '())))

(define (s:current-page)
  (slot-ref s:session 'page))

(define (s:delete-session)
  (session:delete-session s:session (slot-ref s:session 'session-key)))

(define (s:call page . partsl)
  (if (null? partsl)
      (session:call s:session page)
      (session:call s:session page (car partsl))))

(define (s:link-to page . params)
  (session:link-to s:session page params))

(define (s:get-param key)
  (session:get-param s:session key))

;; these are page local
(define (s:get key) 
  (session:get s:session key))

(define (s:set! key val)
  (session:set! s:session key val))

(define (s:del! key)
  (session:del! s:session key))

(define (s:get-n-del! key)
  (let ((val (session:get s:session key)))
    (session:del! s:session key)
    val))

;; these are session wide
(define (s:session-var-get key) 
  (session:get s:session "*sessionvars*" key))

(define (s:session-var-set! key val)
  (session:set! s:session "*sessionvars*" key val))

(define (s:session-var-get-n-del! key)
  (let ((val (session:get s:session key)))
     (session:del! s:session "*sessionvars*" key)
     val))

(define (s:session-var-del! key)
  (session:del! s:session "*sessionvars*" key))

;;
(define (s:get-input key)
  (session:get-input s:session key))

(define (s:load-model model)
  (session:load-model s:session model))

(define (s:model-path model)
  (session:model-path s:session model))

(define (s:db)
  (slot-ref s:session 'conn))

(define (s:never-called-page? page)
  (session:never-called-page? s:session page))

