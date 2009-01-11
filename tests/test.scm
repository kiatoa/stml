#!/usr/local/bin/csi -q 

;; Copyright 2007-2008, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(use test)

(load "./requirements.scm")
(load "./dbi.scm")
(load "./misc-stml.scm")
(load "./formdat.scm")
(load "./stml.scm")
(load "./session.scm")
(load "./sqltbl.scm")
(load "./html-filter.scm")
(load "./keystore.scm")

;; Test the primitive dbi interface

(system "rm -f tests/test.db")
(define db (dbi:open 'sqlite3 '((dbname . "tests/test.db"))))
(dbi:exec db "CREATE TABLE foo(id INTEGER PRIMARY KEY,name TEXT);")
(dbi:exec db "INSERT INTO foo(name) VALUES(?);" "Matt")
(dbi:for-each-row 
 (lambda (tuple)
   (print (vector-ref tuple 0) " " (vector-ref tuple 1)))
 db "SELECT * FROM foo;")
(test "dbi:get-one" "Matt" (dbi:get-one db "SELECT name FROM foo WHERE name='Matt';"))

;; keystore
(dbi:exec db "CREATE TABLE metadata (id INTEGER PRIMARY KEY,key TEXT,value TEXT);")

(keystore:set! db "SCHEMA-VERSION" 1.2)
(test "Keystore get" "1.2"  (keystore:get  db "SCHEMA-VERSION"))
(keystore:del! db "SCHEMA-VERSION") 
(test "Keystore get deleted" #f (keystore:get db "SCHEMA-VERSION"))

(system "rm -f tests/test.db")

;; create a session to work with")
(setenv "REQUEST_URI" "/stmlrun?action=test.test")
(setenv "SCRIPT_NAME" "/cgi-bin/stmlrun")
(setenv "PATH_INFO" "/test")
(setenv "QUERY_STRING" "action=test.test")
(setenv "SERVER_NAME" "localhost")
(setenv "REQUEST_METHOD" "GET")

(load "./setup.scm")

(s:validate-inputs)

;; test session variables

(session:get-vars s:session)
(define nada "andnndhhshaas")
(s:session-var-set! "nick" nada)
(test "Session var set/get" nada  (s:session-var-get "nick"))
(print "got here")
(session:save-vars s:session)
(session:get-vars  s:session)
(test "Session var set/get after save/get" nada (s:session-var-get "nick"))
(session:del! s:session "*sessionvars*" "nick")
(test "Session var del"                    #f   (s:session-var-get "nick"))
(session:save-vars s:session)
(session:get-vars s:session)
(s:session-var-set! "nick" nada)
(session:save-vars s:session)

;; (test "Session var del"                    #f   (s:session-var-get "nick"))

;; test person

(load "./tests/models/test.scm")

(print "Session key is " (slot-ref s:session 'session-key))

(test "Delete session" #t (s:delete-session))


(let ((fh (open-input-pipe "ls ./tests/pages/*/control.scm")))
  (let loop ((l (read-line fh)))
    (if (not (eof-object? l))
        (begin
          ;; (print "loading " l)
          (load l)
          (loop (read-line fh)))))
  (close-input-port fh))

;; Should have poll:poll defined now.
(test "Make a random string" 2 (string-length (session:make-rand-string 2)))
(test "Create a encrypted password" "abQ9KY.KfrYrc" (s:crypt-passwd "foo" "ab"))

(test "s:any->string on a hash-table" "#<hash-table>" (s:any->string (make-hash-table)))

(define select-list
  '((a b c)(d (e f g)(h i j #t))))
(define result '("<SELECT name=\"efg\">" 
		 ((("<OPTION label=\"a\" value=\"b\">c</OPTION>") 
		   ("<OPTGROUP label=d" 
		    ("<OPTION label=\"e\" value=\"f\">g</OPTION>")
		    ("<OPTION  selected label=\"h\" value=\"i\">j</OPTION>") 
		    "</OPTGROUP>")))
		 "</SELECT>"))

(test "Select list" result (s:select select-list 'name "efg"))
