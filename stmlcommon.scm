;; Copyright 2007-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;; (require-extension syntax-case)
;; (declare (run-time-macros))

(include "requirements.scm")
(declare (uses cookie))
(declare (uses html-filter))
(declare (uses misc-stml))
(declare (uses formdat))
(declare (uses stml))
(declare (uses session))
(declare (uses setup)) ;; s:session gets created here
(declare (uses sqltbl))
(declare (uses keystore))

(define (stml:cgi-session session)
  (session:initialize session)
  (session:setup session)
  (session:get-vars session)

  (sdat-set-log-port! session ;; (current-error-port))
		      (open-output-file (sdat-get-logfile session) #:append))
  (s:validate-inputs)
  (session:run-actions session)
  (sdat-set-pagedat! session
		     (append (sdat-get-pagedat session)
			     (s:call (sdat-get-toppage session))))
  (if (eq? (sdat-get-page-type session) 'html) ;; default is html. 
      (session:cgi-out session)
      (session:alt-out session))
  (session:save-vars session)
  (session:close session))

(define (stml:main proc)
  (handle-exceptions
   exn   
   (begin
     (print "Content-type: text/html")
     (print "")
     (print "<html> <head> <title>EXCEPTION</title> </head> <body>")
     (print "   QUERY_STRING is: <b> " (get-environment-variable "QUERY_STRING") " </b> <br>")
     (print "<pre>")
     ;; (print "   EXCEPTION: " ((condition-property-accessor 'exn 'message) exn))
     (print-error-message exn)
     (print-call-chain)
     (print "</pre>")
     (print "<table>")
     (for-each (lambda (var)
		 (print "<tr><td>" (car var) "</td><td>" (cdr var) "</td></tr>"))
	       (get-environment-variables))
     (print "</table>")
     (print "</body></html>"))
   
   (if proc (proc s:session) (stml:cgi-session s:session))
 ;; (raise-error)
 ;; (exit)
   ))
