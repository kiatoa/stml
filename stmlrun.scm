#!/usr/local/bin/csi -q

;; Copyright 2007-2008, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;; (require-extension syntax-case)
(declare (run-time-macros))

(use dbi)

(include "requirements.scm")
(include "html-filter.scm")
(include "misc-stml.scm")
(include "formdat.scm")
(include "stml.scm")
;; (include "dbi.scm")
(include "session.scm")
(include "setup.scm") ;; s:session gets created here
(include "sqltbl.scm")
(include "keystore.scm")
(include "sugar.scm")

(slot-set! s:session 'log-port ;; (current-error-port))
	   (open-output-file (slot-ref s:session 'logfile) #:append))

;; (s:log "HTTP_COOKIE" (getenv "HTTP_COOKIE"))
;; (s:log "stdin-dat=" (slot-ref s:session 'stdin-dat))

(s:validate-inputs)

(session:run-actions s:session)

(slot-set! s:session 'pagedat 
           (append (slot-ref s:session 'pagedat)
                   (s:call (slot-ref s:session 'toppage))))

(if (eq? (slot-ref s:session 'page-type) 'html) ;; default is html. 
    (session:cgi-out s:session)
    (session:alt-out s:session))

(session:save-vars s:session)

(session:close s:session)

(exit)
