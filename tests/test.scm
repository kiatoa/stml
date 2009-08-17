#!/usr/local/bin/csi -q 

;; Copyright 2007-2008, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(use test md5)

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

;; Test modules

(test "misc:non-zero-string \"\"" #f (misc:non-zero-string ""))
(test "misc:non-zero-string #f" #f (misc:non-zero-string #f))
(test "misc:non-zero-string 'blah" #f (misc:non-zero-string 'blah))

;; forms
(define form #f)
(test "make <formdat>" #t (let ((f (make <formdat>)))
			    (set! form f)
			    #t))
(test "formdat: set!/get" "Yep!" (begin
				   (formdat:set! form "blah" "Yep!")
				   (formdat:get  form "blah")))

;; The twiki module

;; clean up
(system "rm -rf twikis/*")
(load "modules/twiki/twiki-mod.scm")
(define keys (list "blah" 1 'nada))
(test "twiki:keys->key"  "blah 1 nada" (twiki:keys->key keys))
(define key (twiki:keys->key keys))

(define *tdb* #f)
(test "twiki:open-db"   #t (let ((db (twiki:open-db key)))
			     (set! *tdb* db)
			     (if *tdb* #t #f)))

(test "twiki:dat->html" '("Hello" "<BR>") (twiki:dat->html "Hello"))
(test "twiki:keys->fname" '("twikis/d99a2de9/6808493b/23770f70" "d99a2de96808493b23770f70c76dffe4")
      (twiki:key->fname key))

(test "twiki:key->wid"     1     (twiki:key->wid *tdb* key))
(test "twiki:get-tiddlers-by-num" '() (twiki:get-tiddlers-by-num  *tdb* 0 (list 1 2 3)))
(test "twiki:get-tiddlers-by-name" '() (twiki:get-tiddlers-by-name *tdb* 0 "MainMenu"))
(test "twiki:get-tiddlers"  '()  (twiki:get-tiddlers *tdb* 0 (list "MainMenu")))
(test "twiki:get-tiddlers"  '()  (twiki:get-tiddlers *tdb* 0 (list "MainMenu" "AnotherOne")))
(test "twiki:wiki" "<TABLE>"     (car (twiki:wiki (list "blah" 1 'nada))))
(test "twiki:view"  "<DIV class=\"node\">" (car (twiki:view "" (twiki:tiddler-make))))

(test "s:td"              '("<TD>" (()) "</TD>") (s:td '()))
;; (test "twiki:get-tiddlers-by-name" '() (twiki:get-tiddlers-by-name 1 "fred"))
(test "twiki:tiddler-name->id" 1 (twiki:tiddler-name->id *tdb* "MainMenu"))
(test "s:set! a var to #f"     ""
      (begin (s:set! "BLAH" #f)
	     (s:get "BLAH"))) ;; don't know if this one makes sense. Setting to #f should really delete the value
(test "twiki:save-dat"           2        (twiki:save-dat *tdb* "dat" 0))
(test "twiki:get-dat"            "dat"    (twiki:get-dat *tdb* 2))
(test "twiki:get-dat"            #f       (twiki:get-dat *tdb* 5))
;; (test "twiki:get-dat"      #f    (twiki:get-dat *tdb* #f))
(test "twiki:save-tiddler"       #t       (twiki:save-tiddler *tdb* "heading" "body" "tags" key 0))
(test "twiki:save-curr-tiddler"  #f       (twiki:save-curr-tiddler *tdb* 1))
(test "twiki:edit-twiddler"      #t       (list? (twiki:edit-tiddler *tdb* key 0)))
(test "twiki:maint_area"         "<DIV>"  (car (twiki:maint_area *tdb* 1 key)))
(test "twiki:pic_mgmt"           "<DIV>"  (car (twiki:pic_mgmt *tdb* 1 key)))
(test "twiki:save-pic"           #t       (twiki:save-pic *tdb* (list "mypic.jpg" "image/jpeg" (string->blob "testing eh!")))) 
(test "twiki:save-pic-from-form" #f       (twiki:save-pic-from-form *tdb* 1))