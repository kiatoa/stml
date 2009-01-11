#!/usr/local/bin/csi -q 

;; This currently requires that the stml code is available in a parallel directory.

(use test)
(if (file-exists? "test.db")
    (begin
      (print "Removing old test.db")
      (system "rm -f test.db")))

(load "../stml/misc-stml.scm")
(load "../stml/formdat.scm")
(load "../stml/stml.scm")
(load "../stml/session.scm")
(load "../stml/sqltbl.scm")
(load "../stml/html-filter.scm") ;; required for s:split-string 
(load "../stml/dbi.scm")
(load "../stml/keystore.scm")
(load "../stml/sugar.scm")

;; create a session to work with")
(setenv "REQUEST_URI" "/stmlrun?action=maint.nada")
(setenv "SCRIPT_NAME" "/cgi-bin/stmlrun")
(setenv "PATH_INFO" "/maint")
(setenv "QUERY_STRING" "action=maint.nada")
(setenv "SERVER_NAME" "localhost")
(setenv "REQUEST_METHOD" "GET")
;; (define session-name "pfNOeqUHkJ26BpU6y49IN") ;; ensure this session already exists
;; (setenv "HTTP_COOKIE" (string-append "session_key=" session-name)) ;; to09ipFJ9_2KXT96b2f9Q")

(load "../stml/setup.scm")
;; (test (string-append "Session set to existing session " session-name)
;;       session-name (slot-ref s:session 'session-key))

(s:validate-inputs)

;; test session variables

;; lazy stuff
(define *conn* (slot-ref s:session 'conn))

;; setup tables
(load "models/maint.scm")
(test "Create tables" #t (> (maint:update-tables)
			    0))

;; test person
(let ((fh (open-input-pipe "ls models/*.scm")))
  (let loop ((l (read-line fh)))
    (if (not (eof-object? l))
        (begin
          (print "loading " l)
          (load l)
          (loop (read-line fh)))))
  (close-input-port fh))

(let ((fh (open-input-pipe "find pages -name control.scm"))) ;; ls pages/*/control.scm")))
  (let loop ((l (read-line fh)))
    (if (not (eof-object? l))
        (begin
          (print "loading " l)
          (load l)
          (loop (read-line fh)))))
  (close-input-port fh))

(let ((fh (open-input-pipe "ls pages/*/view.scm")))
  (let loop ((l (read-line fh)))
    (if (not (eof-object? l))
        (begin
          (print "loading " l)
          (load l)
          (loop (read-line fh)))))
  (close-input-port fh))

;;======================================================================
;; Maint
;;======================================================================
;; 
(load "models/maint.scm")

(test "Update tables"   #t                (> (maint:update-tables))) ;;  *conn* 2 "us") 0))
(test "Add user"        "matt@kiatoa.com" (vector-ref (person:set-password "matt@kiatoa.com" "Password") 2))
(test "Authenticate"    "matt@kiatoa.com" (vector-ref (person:authenticate "matt@kiatoa.com" "Password") 2))
(test "Validate inputs" #t                (new_account:validate-inputs "Password" "Password" "matt@kiatoa.com" "matt@kiatoa.com"))

