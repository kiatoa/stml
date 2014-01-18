;; Copyright 2007-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(declare (unit setup))
(declare (uses session))
(require-extension srfi-69)
(require-extension regex)

;; use this for getting data from page to page when scope and evals
;; get in the way
(define s:local-vars (make-hash-table))

(define (s:local-set! k v)
  (hash-table-set! s:local-vars k v))

(define (s:local-get k)
  (hash-table-ref/default s:local-vars k #f))

(define (s:log . msg)
  (apply session:log s:session msg))

(define (s:set-err . args)
  (sdat-set-curr-err! s:session args))

;; Usage: (s:get-err s:big)
(define (s:get-err wrapperfunc)
  (let ((errmsg (sdat-get-curr-err s:session)))
    (if errmsg ((if wrapperfunc
                    wrapperfunc
                    s:strong) errmsg) '())))

(define (s:current-page)
  (sdat-get-page s:session))

(define (s:delete-session)
  (session:delete-session s:session (sdat-get-session-key s:session)))

(define (s:call page . partsl)
  (if (null? partsl)
      (session:call s:session page #f)
      (session:call s:session page (car partsl))))

(define (s:link-to page . params)
  (session:link-to s:session page params))

(define (s:get-param key)
  (session:get-param s:session key))

;; these are page local
(define (s:get key) 
  (session:page-get s:session key))

(define (s:set! key val)
  (session:curr-page-set! s:session key val))

(define (s:del! key)
  (session:page-var-del! s:session key))

(define (s:get-n-del! key)
  (let ((val (session:page-get s:session key)))
    (session:del! s:session key)
    val))

;; these are session wide
(define (s:session-var-get key) 
  (session:get s:session "*sessionvars*" key))

(define (s:session-var-set! key val)
  (session:set! s:session "*sessionvars*" key val))

(define (s:session-var-get-n-del! key)
  (let ((val (session:page-get s:session key)))
     (session:del! s:session "*sessionvars*" key)
     val))

(define (s:session-var-del! key)
  (session:del! s:session "*sessionvars*" key))

;; utility to get all vars as hash table
(define (s:session-get-sessionvars)
  (sdat-get-sessionvars s:session))

;; inputs
;;
(define (s:get-input key)
  (session:get-input s:session key))

(define (s:load-model model)
  (session:load-model s:session model))

(define (s:model-path model)
  (session:model-path s:session model))

;; share data between pages calls. NOTE: This is not persistent
;; between cgi calls. Use sessionvars for that.
;;
(define (s:shared-hash)
  (sdat-get-shared-hash s:session))

(define (s:shared-set! key val)
  (hash-table-set! (sdat-get-shared-hash s:session) key val))

;; What to return when no value for key?
;;
(define (s:shared-get key)
  (hash-table-ref/default (sdat-get-shared-hash s:session) key #f))

;; http://foo.bar.com/pagename/p1/p2 => '("p1" "p2")
;;
(define (s:get-page-params)
  (sdat-get-page-params s:session))

(define (s:db)
  (sdat-get-conn s:session))

(define (s:never-called-page? page)
  (session:never-called-page? s:session page))

;; find out if we are in debugmode
(define (s:debug-mode?)
  (sdat-get-debugmode s:session))

