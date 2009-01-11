;; Copyright 2007-2008, Matthew Welland. matt@kiatoa.com All rights reserved.
;; 
;; maint/control.scm
;;

;; evolve your schema here!
;; Add entries and then go to http:/your-url/maint
;;
;; first make maint:db available as a global
;;
(define maint:db (slot-ref s:session 'conn))

;; you can store lambda's or SQL queries to be exectuted
;; be extremely careful - especially with the lambda's!!!
(define maint:schema-updates
  (list (list 1 (lambda ()(keystore:set! maint:db "MAINTPW" "Abc123")))
	(list 2 "CREATE TABLE people (id INTEGER PRIMARY KEY,name TEXT DEFAULT '',nick TEXT DEFAULT '',email TEXT,password TEXT,status INTEGER DEFAULT 0,score INTEGER DEFAULT 0,location_id INTEGER DEFAULT 0);")
	(list 3 "CREATE TABLE candidates (id INTEGER PRIMARY KEY,name TEXT DEFAULT '',url TEXT DEFAULT '',party TEXT DEFAULT '',desc TEXT DEFAULT '',supports_av INTEGER,date_added DATETIME,score INTEGER DEFAULT 0);")
	(list 4 "CREATE TABLE votes (id INTEGER PRIMARY KEY,candidate_id INTEGER,vote_date INTEGER,votes INTEGER,score INTEGER,vote_type INTEGER);")
	(list 5 "CREATE TABLE voted (id INTEGER PRIMARY KEY,user_id INTEGER,vote_date INTEGER,score INTEGER);")
	;; location_type can be: city, town, state, region, county etc
	(list 6 "CREATE TABLE locations (id INTEGER PRIMARY KEY,parent_id INTEGER,codename TEXT,name TEXT,location_type TEXT,desc TEXT,url TEXT);")
	(list 7 "INSERT INTO locations VALUES(0,0,'ea','earth','planet','Home Planet of Humans','');")
	(list 8 "ALTER TABLE candidates ADD column pscore INTEGER DEFAULT 0;")
	))

(define (maint:am-i-maint?)
  ;; Enter a maint password - return #t if good
  #t)

(define (maint:update-tables)
  (let* ((db       (slot-ref s:session 'conn))
	 (curr-ver (s:any->number (keystore:get db "SCHEMA-VERSION"))))
    (if (not curr-ver)
	(begin
	  (keystore:set! (slot-ref s:session 'conn) "SCHEMA-VERSION" 0)
	  (set! curr-ver 0)))
    (if (null? maint:schema-updates)
	(keystore:set! (slot-ref s:session 'conn) "SCHEMA-VERSION" 0)
	(let loop ((hed (car  maint:schema-updates))
		   (tal (cdr maint:schema-updates))
		   (highest-ver 0))
	  (if (< (length hed) 2)
	      (s:log "Malformed maint:schema-updates table in maint/control.scm")
	      (let ((ver (car hed))
		    (act (cadr hed)))
		(if (> ver curr-ver) ;; need to apply this one
		    (begin
		      (if (string? act)
			  (dbi:exec db act)
			  (act))
		      ;; yes, do this for each one, just in case of a crash
		      (keystore:set! db "SCHEMA-VERSION" ver)))
		(if (null? tal)
		    highest-ver
		    (loop (car tal)(cdr tal) ver))))))))

