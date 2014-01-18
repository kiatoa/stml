;; Copyright 2007-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(declare (unit session))
(require-library dbi)
(require-extension regex)
(declare (uses cookie))

;; sessions table
;; id session_id session_key
;; create table sessions (id serial not null,session-key text);

;; session_vars table
;; id session_id page_id key value
;; create table session_vars (id serial not null,session_id integer,page text,key text,value text);

;; TODO
;;  Concept of order num incremented with each page access
;;     if a branch is taken then a new session would need to be created
;;

;; make-vector-record session session dbtype dbinit conn params path-params session-key session-id domain toppage page curr-page content-type page-type sroot twikidir pagedat alt-page-dat pagevars pagevars-before sessionvars sessionvars-before globalvars globalvars-before logpt formdat request-method session-cookie curr-err log-port logfile seen-pages page-dir-style debugmode
(define (make-sdat)(make-vector 34))
(define (sdat-get-dbtype               vec)    (vector-ref  vec 0))
(define (sdat-get-dbinit               vec)    (vector-ref  vec 1))
(define (sdat-get-conn                 vec)    (vector-ref  vec 2))
(define (sdat-get-pgconn               vec)    (vector-ref (vector-ref vec 2) 1))
(define (sdat-get-params               vec)    (vector-ref  vec 3))
(define (sdat-get-path-params          vec)    (vector-ref  vec 4))
(define (sdat-get-session-key          vec)    (vector-ref  vec 5))
(define (sdat-get-session-id           vec)    (vector-ref  vec 6))
(define (sdat-get-domain               vec)    (vector-ref  vec 7))
(define (sdat-get-toppage              vec)    (vector-ref  vec 8))
(define (sdat-get-page                 vec)    (vector-ref  vec 9))
(define (sdat-get-curr-page            vec)    (vector-ref  vec 10))
(define (sdat-get-content-type         vec)    (vector-ref  vec 11))
(define (sdat-get-page-type            vec)    (vector-ref  vec 12))
(define (sdat-get-sroot                vec)    (vector-ref  vec 13))
(define (sdat-get-twikidir             vec)    (vector-ref  vec 14))
(define (sdat-get-pagedat              vec)    (vector-ref  vec 15))
(define (sdat-get-alt-page-dat         vec)    (vector-ref  vec 16))
(define (sdat-get-pagevars             vec)    (vector-ref  vec 17))
(define (sdat-get-pagevars-before      vec)    (vector-ref  vec 18))
(define (sdat-get-sessionvars          vec)    (vector-ref  vec 19))
(define (sdat-get-sessionvars-before   vec)    (vector-ref  vec 20))
(define (sdat-get-globalvars           vec)    (vector-ref  vec 21))
(define (sdat-get-globalvars-before    vec)    (vector-ref  vec 22))
(define (sdat-get-logpt                vec)    (vector-ref  vec 23))
(define (sdat-get-formdat              vec)    (vector-ref  vec 24))
(define (sdat-get-request-method       vec)    (vector-ref  vec 25))
(define (sdat-get-session-cookie       vec)    (vector-ref  vec 26))
(define (sdat-get-curr-err             vec)    (vector-ref  vec 27))
(define (sdat-get-log-port             vec)    (vector-ref  vec 28))
(define (sdat-get-logfile              vec)    (vector-ref  vec 29))
(define (sdat-get-seen-pages           vec)    (vector-ref  vec 30))
(define (sdat-get-page-dir-style       vec)    (vector-ref  vec 31))
(define (sdat-get-debugmode            vec)    (vector-ref  vec 32))
(define (sdat-get-shared-hash          vec)    (vector-ref  vec 33))

(define (session:get-shared vec varname)
  (hash-table-ref/default (vector-ref vec 33) varname #f))

(define (sdat-set-dbtype!              vec val)(vector-set! vec 0 val))
(define (sdat-set-dbinit!              vec val)(vector-set! vec 1 val))
(define (sdat-set-conn!                vec val)(vector-set! vec 2 val))
(define (sdat-set-params!              vec val)(vector-set! vec 3 val))
(define (sdat-set-path-params!         vec val)(vector-set! vec 4 val))
(define (sdat-set-session-key!         vec val)(vector-set! vec 5 val))
(define (sdat-set-session-id!          vec val)(vector-set! vec 6 val))
(define (sdat-set-domain!              vec val)(vector-set! vec 7 val))
(define (sdat-set-toppage!             vec val)(vector-set! vec 8 val))
(define (sdat-set-page!                vec val)(vector-set! vec 9 val))
(define (sdat-set-curr-page!           vec val)(vector-set! vec 10 val))
(define (sdat-set-content-type!        vec val)(vector-set! vec 11 val))
(define (sdat-set-page-type!           vec val)(vector-set! vec 12 val))
(define (sdat-set-sroot!               vec val)(vector-set! vec 13 val))
(define (sdat-set-twikidir!            vec val)(vector-set! vec 14 val))
(define (sdat-set-pagedat!             vec val)(vector-set! vec 15 val))
(define (sdat-set-alt-page-dat!        vec val)(vector-set! vec 16 val))
(define (sdat-set-pagevars!            vec val)(vector-set! vec 17 val))
(define (sdat-set-pagevars-before!     vec val)(vector-set! vec 18 val))
(define (sdat-set-sessionvars!         vec val)(vector-set! vec 19 val))
(define (sdat-set-sessionvars-before!  vec val)(vector-set! vec 20 val))
(define (sdat-set-globalvars!          vec val)(vector-set! vec 21 val))
(define (sdat-set-globalvars-before!   vec val)(vector-set! vec 22 val))
(define (sdat-set-logpt!               vec val)(vector-set! vec 23 val))
(define (sdat-set-formdat!             vec val)(vector-set! vec 24 val))
(define (sdat-set-request-method!      vec val)(vector-set! vec 25 val))
(define (sdat-set-session-cookie!      vec val)(vector-set! vec 26 val))
(define (sdat-set-curr-err!            vec val)(vector-set! vec 27 val))
(define (sdat-set-log-port!            vec val)(vector-set! vec 28 val))
(define (sdat-set-logfile!             vec val)(vector-set! vec 29 val))
(define (sdat-set-seen-pages!          vec val)(vector-set! vec 30 val))
(define (sdat-set-page-dir-style!      vec val)(vector-set! vec 31 val))
(define (sdat-set-debugmode!           vec val)(vector-set! vec 32 val))
(define (sdat-set-shared-hash!         vec val)(vector-set! vec 33 val))

(define (session:set-shared! vec varname val)
  (hash-table-set! (vector-ref vec 33) varname val))

;; The global session
(define s:session (make-sdat))

;; SPLIT INTO STRAIGHT FORWARD INIT AND COMPLEX INIT
(define (session:initialize self)
  (sdat-set-dbtype! self      'pg)
  (sdat-set-page! self        "home")        ;; these are defaults
  (sdat-set-curr-page! self   "home")
  (sdat-set-content-type! self "Content-type: text/html; charset=iso-8859-1\n\n")
  (sdat-set-page-type! self   'html)
  (sdat-set-toppage! self     "index")
  (sdat-set-params! self      '())           ;;
  (sdat-set-path-params! self '())
  (sdat-set-session-key! self #f)
  (sdat-set-pagedat! self     '())
  (sdat-set-alt-page-dat! self #f)
  (sdat-set-sroot! self       "./")
  (sdat-set-session-cookie! self #f)
  (sdat-set-curr-err! self #f)
  (sdat-set-log-port! self (current-error-port))
  (sdat-set-seen-pages! self '())
  (sdat-set-page-dir-style! self #t) ;; #t : pages/<pagename>_(view|cntl).scm
                                      ;; #f : pages/<pagename>/(view|control).scm 
  (sdat-set-debugmode!          self #f)
  			     
  (sdat-set-pagevars!           self (make-hash-table))
  (sdat-set-sessionvars!        self (make-hash-table))
  (sdat-set-globalvars!         self (make-hash-table))
  (sdat-set-pagevars-before!    self (make-hash-table))
  (sdat-set-sessionvars-before! self (make-hash-table))
  (sdat-set-globalvars-before!  self (make-hash-table))
  (sdat-set-domain!             self "locahost")   ;; end of defaults
  (let* ((rawconfigdat (session:read-config self))
	 (configdat (if rawconfigdat (eval rawconfigdat) '()))
	 (sroot     (s:find-param 'sroot    configdat))
	 (logfile   (s:find-param 'logfile  configdat))
	 (dbtype    (s:find-param 'dbtype   configdat))
	 (dbinit    (s:find-param 'dbinit   configdat))
	 (domain    (s:find-param 'domain   configdat))
	 (twikidir  (s:find-param 'twikidir configdat))
	 (page-dir  (s:find-param 'page-dir-style configdat))
	 (debugmode (s:find-param 'debugmode configdat)))
    (if sroot    (sdat-set-sroot!    self sroot))
    (if logfile  (sdat-set-logfile!  self logfile))
    (if dbtype   (sdat-set-dbtype!   self dbtype))
    (if dbinit   (sdat-set-dbinit!   self dbinit))
    (if domain   (sdat-set-domain!   self domain))
    (if twikidir (sdat-set-twikidir! self twikidir))
    (if debugmode (sdat-set-debugmode! self debugmode))
    (sdat-set-page-dir-style! self page-dir)
    ;; (print "configdat: ")(pp configdat)
    (if debugmode
	(session:log self "sroot: " sroot " logfile: " logfile " dbtype: " dbtype 
		     " dbinit: " dbinit " domain: " domain " page-dir-style: " page-dir))
    )
  (sdat-set-shared-hash! self (make-hash-table))
  )

;; Used for the strangely inconsistent handling of the config file. A better way is needed.
;;
;;   (let ((dbtype (sdat-get-dbtype self)))
;;     (print "dbtype: " dbtype)
;;     (sdat-set-dbtype! self (eval dbtype))))

(define (session:setup self)
  (let ((dbtype    (sdat-get-dbtype self))
	(debugmode (sdat-get-debugmode self))
	(dbinit    (eval (sdat-get-dbinit self)))
	(dbexists  #f))
    (let ((dbfname (alist-ref 'dbname dbinit)))
      (if debugmode (session:log self "session:setup dbfname=" dbfname ", dbtype=" dbtype ", dbinit=" dbinit))
      (if (eq? dbtype 'sqlite3)
	  ;; The 'auto method will distribute dbs across the disk using hash
	  ;; of user host and user. TODO
	  ;; (if (eq? dbfname 'auto) ;; This is the auto assignment of a db based on hash of IP
	  (let ((dbpath (pathname-directory dbfname)))  ;; do a couple sanity checks here to make setting up easier
	    (if debugmode (session:log self "INFO: setting up for sqlite3 db access to " dbfname))
	    (if (not (file-write-access? dbpath))
		(session:log self "WARNING: Cannot write to " dbpath)
		(if debugmode (session:log self "INFO: " dbpath " is writeable")))
	    (if (file-exists? dbfname)
		(begin
		  ;; (session:log self "setting dbexists to #t")
		  (set! dbexists #t))))
	  (if debugmode (session:log self "INFO: setting up for pg db access to account info " dbinit)))
      (if debugmode (session:log self "dbtype: " dbtype " dbfname: " dbfname " dbexists: " dbexists)))
    (sdat-set-conn! self (dbi:open dbtype dbinit))
    (set! *db* (sdat-get-conn self))
    (if (and (not dbexists)(eq? dbtype 'sqlite3))
 	(begin
	  (print "WARNING: Setting up session db with sqlite3")
	  (session:setup-db self)))
    (session:process-url-path self)
    (session:setup-session-key self)
    ;; capture stdin if this is a POST
    (sdat-set-request-method! self (get-environment-variable "REQUEST_METHOD"))
    (sdat-set-formdat! self (formdat:load-all))))

;; setup the db with session tables, works for sqlite only right now
(define (session:setup-db self)
  (let ((conn (sdat-get-conn self)))
    (for-each 
     (lambda (stmt)
       (dbi:exec conn stmt))
     (list "CREATE TABLE session_vars (id INTEGER PRIMARY KEY,session_id INTEGER,page TEXT,key TEXT,value TEXT);"
	   "CREATE TABLE sessions (id INTEGER PRIMARY KEY,session_key TEXT,last_used TIMESTAMP);"
           "CREATE TABLE metadata (id INTEGER PRIMARY KEY,key TEXT,value TEXT);"))))
;;  ;; if we have a session_key look up the session-id and store it
;;  (sdat-set-session-id! self (session:get-id self)))

;; only set session-cookie when a new session is created
(define (session:setup-session-key self)  
  (let* ((sk  (session:extract-session-key self))
         (sid (if sk (session:get-id self sk) #f)))
    (if (not sid) ;; need a new key
        (let* ((new-key (session:get-new-key self))
               (new-sid (session:get-id self new-key)))
          (sdat-set-session-key! self new-key)
          (sdat-set-session-id! self new-sid)
          (sdat-set-session-cookie! self (session:make-cookie self)))
        (sdat-set-session-id! self sid))))

(define (session:make-cookie self)
  ;; (list (conc "session_key=" (sdat-get-session-key self) "; Path=/; Domain=." (sdat-get-domain self) "; Max-Age=" (* 86400 14) "; Version=1"))) 
  ;; According to 
  ;;    http://www.codemarvels.com/2010/11/apache-rewriterule-set-a-cookie-on-localhost/

  ;;  Here are the 2 (often left out) requirements to set a cookie using
  ;;  httpd-F¢s rewrite rule (mod_rewrite), while working on localhost:-A
  ;;
  ;;  Use the IP 127.0.0.1 instead of localhost/machine-name as the
  ;;  domain; e.g. [CO=someCookie:someValue:127.0.0.1:2:/], which says
  ;;  create a cookie -Y´someCookie¡ with value ´someValue¡ for the
  ;;  domain ´127.0.0.1$B!m(B having a life time of 2 mins, for any path in
  ;;  the domain (path=/). (Obviously you will have to run the
  ;;  application with this value in the URL)
  ;;
  ;;  To make a session cookie, limit the flag statement to just three
  ;;  attributes: name, value and domain. e.g
  ;;  [CO=someCookie:someValue:127.0.0.1] %Gâ€“%@ Any further
  ;;  settings, apache writes an¡ expires¡ attribute for the set-cookie
  ;;  header, which makes the cookie a persistent one (not really
  ;;  persistent, as the expires value set is the current server time
  ;;  %Gâ€“%@ so you don-F-F¢t even get to see your cookie!)-A
  (list (string-substitute 
	 ";" "; " 
	 (car (construct-cookie-string 
	       ;; warning! messing up this itty bitty bit of code will cost much time!
	       `(("session_key" ,(sdat-get-session-key self)
		  expires: ,(+ (current-seconds) (* 14 86400)) 
		  ;; max-age: (* 14 86400)
		  path: "/" ;; 
		  domain: ,(string-append "." (sdat-get-domain self))
		  version: 1)) 0)))))

;; look up a given session key and return the id if found, #f if not found
(define (session:get-id self session-key)
  ;; (let ((session-key (sdat-get-session-key self)))
  (if session-key
      (let ((query (string-append "SELECT id FROM sessions WHERE session_key='" session-key "'"))
            (conn (sdat-get-conn self))
            (result #f))
	(dbi:for-each-row 
	 (lambda (tuple)
	   (set! result (vector-ref tuple 0)))
	 conn query)
	(if result (dbi:exec conn (conc "UPDATE sessions SET last_used=" (dbi:now conn) " WHERE session_key=?;") session-key))
        result)
      #f))

;; 
(define (session:process-url-path self)
  (let ((path-info    (get-environment-variable "PATH_INFO"))
	(query-string (get-environment-variable "QUERY_STRING")))
    ;; (session:log self "path-info=" path-info " query-string=" query-string)
    (if path-info
	(let* ((parts    (string-split path-info "/"))
	       (numparts (length parts)))
	  (if (> numparts 0)
	      (sdat-set-page! self (car parts)))
	  ;; (session:log self "url-path=" url-path " parts=" parts)
	  (if (> numparts 1)
	      (sdat-set-path-params! self (cdr parts)))
          (if query-string
              (sdat-set-params! self (string-split query-string "&")))))))

;; BUGGY!
(define (session:get-new-key self)
  (let ((conn   (sdat-get-conn self))
        (tmpkey (session:make-rand-string 20))
        (status #f))
    (dbi:for-each-row (lambda (tuple)
			(set! status #t))
		      conn (string-append "INSERT INTO sessions (session_key) VALUES ('" tmpkey "')"))
    tmpkey))

;; returns session key IFF it is in the HTTP_COOKIE 
(define (session:extract-session-key self)
  (let ((http-cookie (get-environment-variable "HTTP_COOKIE")))
    ;; (err:log "http-cookie: " http-cookie)
    (if http-cookie
        (session:extract-key-from-param self (string-split-fields  ";\\s+" http-cookie infix:) "session_key")
        #f)))

(define (session:get-session-id self session-key)
  (let ((query "SELECT id FROM sessions WHERE session_key=?;")
        (result #f))
    ;;     (pg:query-for-each (lambda (tuple)
    ;;                          (set! result (vector-ref tuple 0))) ;; (vector-ref tuple 0)))
    ;;                        (s:sqlparam query session-key)
    ;;                        (sdat-get-conn self))
    ;;                        conn)
    (dbi:for-each-row (lambda (tuple)
			(set! result (vector-ref tuple 0))) ;; (vector-ref tuple 0)))
		      (sdat-get-conn self)
		      (s:sqlparam query session-key))
    result))

;; delete all records for a session
;; 
;; NEEDS TO BE TRANSACTIONIZED!
;;
(define (session:delete-session self session-key)
  (let ((session-id (session:get-session-id self session-key))
        (qry1        ;; (conc "BEGIN;"
			  "DELETE FROM session_vars WHERE session_id=?;")
	(qry2             "DELETE FROM sessions WHERE id=?;")
		     ;;  "COMMIT;"))
        (conn              (sdat-get-conn self)))
    (if session-id
        (begin
          (dbi:exec conn qry1 session-id) ;; session-id)
	  (dbi:exec conn qry2 session-id)
	  (session:initialize self)
	  (session:setup self)))
    (not (session:get-session-id self session-key))))

;; (define (session:delete-session self session-key)
;;   (let ((session-id (session:get-session-id self session-key))
;;         (queries    (list "BEGIN;"
;; 			  "DELETE FROM session_vars WHERE session_id=?;"
;;                           "DELETE FROM sessions WHERE id=?;"
;; 			  "COMMIT;"))
;;         (conn              (sdat-get-conn self)))
;;     (if session-id
;;         (begin
;;           (for-each
;;            (lambda (query)
;;              (dbi:exec conn query session-id))
;; 	   queries)
;; 	  (initialize self '())
;; 	  (session:setup self)))
;;     (not (session:get-session-id self session-key))))

(define (session:extract-key self key)
  (let ((params (sdat-get-params self)))
    (session:extract-key-from-param self params key)))

(define (session:extract-key-from-param self params key)
  (let ((r1     (regexp (string-append "^" key "=([^=]+)$"))))
    (err:log "INFO: Looking for " key " in " params)
    (if (< (length params) 1) #f
	(let loop ((head   (car params))
		   (tail   (cdr params)))
	  (let ((match (string-match r1 head)))
	    (cond
	     (match
	      (let ((session-key (list-ref match 1)))
		(err:log "INFO: Found session key=" session-key)
		(sdat-set-session-key! self (list-ref match 1))
		session-key))
	     ((null? tail)
	      #f)
	     (else
	      (loop (car tail)
		    (cdr tail)))))))))

(define (session:set-page! self page_name)
  (sdat-set-page! self page_name))

(define (session:close self)
  (dbi:close (sdat-get-conn self)))
;; (close-output-port (sdat-get-logpt self))

(define (session:err-msg self msg)
  (hash-table-set! (sdat-get-sessionvars self) "ERROR_MSG"
		   (string-intersperse (map s:any->string msg) " ")))

(define (session:prev-err self)
  (let ((prev-err (hash-table-ref/default (sdat-get-sessionvars-before self) "ERROR_MSG" #f))
	(curr-err (hash-table-ref/default (sdat-get-sessionvars self) "ERROR_MSG" #f)))
    (if prev-err prev-err
	(if curr-err curr-err #f))))

;; session vars
;; 1. keys are always a string NOT a symbol
;; 2. values are always a string conversion is the responsibility of the 
;;    consuming function (at least for now, I'd like to change this)

;; set a session var for the current page
;;
(define (session:curr-page-set! self key value)
  (hash-table-set! (sdat-get-pagevars self) (s:any->string key) (s:any->string value)))

;; del a var for the current page
;;
(define (session:page-var-del! self key)
  (hash-table-delete! (sdat-get-pagevars self) (s:any->string key)))

;; get the appropriate hash given a page "*sessionvars*, *globalvars* or page
;;
(define (session:get-page-hash self page)
  (if (string=? page "*sessionvars*")
      (sdat-get-sessionvars self)
      (if (string=? page "*globalvars*")
	  (sdat-get-globalvars self)
	  (sdat-get-pagevars self))))

;; set a session var for a given page
;;
(define (session:set! self page key value)
  (let ((ht (session:get-page-hash self page)))
    (hash-table-set! ht (s:any->string key) (s:any->string value))))

;; get session vars for the current page
;;
(define (session:page-get self key)
  (hash-table-ref/default (sdat-get-pagevars self) key #f))

;; get session vars for a specified page
;;
(define (session:get self page key)
  (let ((ht (session:get-page-hash self page)))
    (hash-table-ref/default ht (s:any->string key) #f)))

;; delete a session var for a specified page
;;
(define (session:del! self page key)
  (let ((ht (session:get-page-hash self page)))
    (hash-table-delete! ht (s:any->string key))))

;; get ALL keys for this page and store in the session pagevars hash
;;
(define (session:get-vars self)
  (let ((session-id  (sdat-get-session-id self)))
    (if (not session-id)
	(err:log "ERROR: No session id in session object! session:get-vars")
	(let* ((result             #f)
	       (conn               (sdat-get-conn self))
	       (pagevars-before    (sdat-get-pagevars-before self))
	       (sessionvars-before (sdat-get-sessionvars-before self))
	       (globalvars-before  (sdat-get-globalvars-before self))
	       (pagevars           (sdat-get-pagevars self))
	       (sessionvars        (sdat-get-sessionvars self))
	       (globalvars         (sdat-get-globalvars self))
	       (page-name          (sdat-get-page self))
	       (session-key        (sdat-get-session-key self))
	       (query              (string-append
				    "SELECT key,value FROM session_vars INNER JOIN sessions ON session_vars.session_id=sessions.id "
				    "WHERE session_key=? AND page=?;")))
	  ;; first the page specific vars
	  (dbi:for-each-row (lambda (tuple)
			      (let ((k (vector-ref tuple 0))
				    (v (vector-ref tuple 1)))
				(hash-table-set! pagevars-before k v)
				(hash-table-set! pagevars        k v)))
			    conn
			    (s:sqlparam query session-key page-name))
	  ;; then the session specific vars
	  (dbi:for-each-row (lambda (tuple)
			      (let ((k (vector-ref tuple 0))
				    (v (vector-ref tuple 1)))
				(hash-table-set! sessionvars-before k v)
				(hash-table-set! sessionvars        k v)))
			    conn
			    (s:sqlparam query session-key "*sessionvars*"))
	  ;; and finally the global vars
	  (dbi:for-each-row (lambda (tuple)
			      (let ((k (vector-ref tuple 0))
				    (v (vector-ref tuple 1)))
				(hash-table-set! globalvars-before k v)
				(hash-table-set! globalvars        k v)))
			    conn
			    (s:sqlparam query session-key "*globalvars"))
	  ))))

(define (session:save-vars self)
  (let ((session-id  (sdat-get-session-id self)))
    (if (not session-id)
	(err:log "ERROR: No session id in session object! session:get-vars")
	(let* ((status      #f)
	       (conn        (sdat-get-conn self))
	       (page-name   (sdat-get-page self))
	       (del-query   "DELETE FROM session_vars WHERE session_id=? AND page=? AND key=?;")
	       (ins-query   "INSERT INTO session_vars (session_id,page,key,value) VALUES(?,?,?,?);")
	       (upd-query   "UPDATE session_vars set value=? WHERE key=? AND session_id=? AND page=?;")
	       (changed-count 0))
	  ;; save the delta only
	  (for-each
	   (lambda (page) ;; page is: "*globalvars*" "*sessionvars*" or otherstring
	     (let* ((before-after-ht (cond
				      ((string=? page "*sessionvars*")
				       (vector (sdat-get-sessionvars self)
					       (sdat-get-sessionvars-before self)))
				       ((string=? page "*globalvars*")
					(vector (sdat-get-globalvars self)
						(sdat-get-globalvars-before self)))
				       (else 
					(vector (sdat-get-pagevars self)
						(sdat-get-pagevars-before self)))))
		    (master-ht   (vector-ref before-after-ht 0))
		    (before-ht   (vector-ref before-after-ht 1))
		    (master-keys (hash-table-keys master-ht))
		    (before-keys (hash-table-keys before-ht))
		    (all-keys (delete-duplicates (append master-keys before-keys))))
	       (for-each 
		(lambda (key)
		  (let ((master-value (hash-table-ref/default master-ht key #f))
			(before-value (hash-table-ref/default before-ht key #f)))
		    (cond
		     ;; before and after exist and value unchanged - do nothing
		     ((and master-value before-value (equal? master-value before-value)))
		     ;; before and after exist but are changed
		     ((and master-value before-value)
		      (dbi:for-each-row (lambda (tuple)
					  (set! changed-count (+ changed-count 1)))
					conn
					(s:sqlparam upd-query master-value key session-id page)))
		     ;; master-value no longer exists (i.e. #f) - remove item
		     ((not master-value)
		      (dbi:for-each-row (lambda (tuple)
					  (set! changed-count (+ changed-count 1)))
					conn
					(s:sqlparam del-query session-id page key)))
		     ;; before-value doesn't exist - insert a new value
		     ((not before-value)
		      (dbi:for-each-row (lambda (tuple)
					  (set! changed-count (+ changed-count 1)))
					conn
					(s:sqlparam ins-query session-id page key master-value)))
		     (else (err:log "Shouldn't get here")))))
		all-keys))) ;; process all keys
	   (list "*sessionvars*" "*globalvars*" page-name))))))

;; (pg:sql-null-object? element)
(define (session:read-config self)
  (let ((name (string-append "." (pathname-file (car (argv))) ".config")))
    (if (not (file-exists? name))
	(print name " not found at " (current-directory))
	(let* ((fp (open-input-file name))
	       (initargs (read fp)))
	  (close-input-port fp)
	  initargs))))

;; call the controller if it exists
;; 
;; WARNING - this code needs a defence agains recursive calling!!!!!
;;
;;   I suggest a limit of 100 calls. Plenty for allowing multiple instances
;;   of a page inside another page. 
;;
;; parts = 'both | 'control | 'view
;;

(define (files-read->string . files)
  (string-intersperse 
   (apply append (map file-read->string files)) "\n"))

(define (file-read->string f) 
  (let ((p (open-input-file f)))
    (let loop ((hed (read-line p))
	       (res '()))
      (if (eof-object? hed)
	  res
	  (loop (read-line p)(append res (list hed)))))))

(define (process-port p)
  (let ((e (interaction-environment)))
    (map 
     (lambda (x)
       (cond
	((list? x) x)
	((string? x) x)
	(else '())))
     (port-map (lambda (s)
		 (eval s e))
	       (lambda ()(read p))))))

(define (session:process-file f)
  (let* ((p    (open-input-file f))
	 (dat  (process-port p)))
    (close-input-port p)
    dat))

;; May 2011, putting all pages into one directory for the following reasons:
;;   1. want filename to reflect page name (emacs limitation)
;;   2. that's it! no other reason. could make it configurable ...
;; page-dir-style is:
;;  'stored   => stored in executable
;;  'flat     => pages flat directory
;;  'dir      => directory tree pages/<pagename>/{view,control}.scm
;; parts:
;;  'both     => load control and view (anything other than view or control
;;  'view     => load view only
;;  'control  => load control only
(define (session:call-parts self page #!key (parts 'both))
  (sdat-set-curr-page! self page)
  (let* ((dir-style    (sdat-get-page-dir-style self));; (equal? (sdat-get-page-dir-style self) "onedir")) ;; flag #t for onedir, #f for old style
	 (dir          (string-append (sdat-get-sroot self) 
				      (if dir-style 
					  (conc "/pages/")
					  (conc "/pages/" page)))))
    (case dir-style
      ;; NB// Stored always loads both control and view
      ((stored)
       ((eval (string->symbol (conc "pages:" page))) 
	self                         ;; the session
	(sdat-get-conn self)         ;; the db connection
	(sdat-get-shared-hash self)  ;; a shared hash table for passing data to/from page calls
	))
      ((flat)   
       (let* ((so-file  (conc dir page ".so"))
	      (scm-file (conc dir page ".scm"))
	      (src-file (or (file-exists? so-file)
			    (file-exists? scm-file))))
	 (if src-file
	     (begin
	       (load src-file)
	       ((eval (string->symbol (conc "pages:" page))) 
		self                         ;; the session
		(sdat-get-conn self)         ;; the db connection
		(sdat-get-shared-hash self)  ;; a shared hash table for passing data to/from page calls
		))
	     (list "<p>Page not found " page " </p>"))))
       ;; first the control
       ;; (let ((control-file (conc "pages/" page "_ctrl.scm"))
       ;;       (view-file    (conc "pages/" page "_view.scm")))
       ;;   (if (and (file-exists? control-file)
       ;;  	  (not (eq? parts 'view)))
       ;;       (begin
       ;;         (session:set-called! self page)
       ;;         (load control-file)))
       ;;   (if (file-exists? view-file)
       ;;       (if (not (eq? parts 'control))
       ;;  	 (session:process-file view-file))
       ;;       (list "<p>Page not found " page " </p>")))
      ((dir) "ERROR:  dir style not yet re-implemented")
      (else
       (list "ERROR: page-dir-style must be stored, dir or flat, got " dir-style)))))

(define (session:call self page parts)
  (session:call-parts self page 'both))

;; (define (session:load-model self model)
;;   (let ((model.scm (string-append (sdat-get-sroot self) "/models/" model ".scm"))
;; 	(model.so  (string-append (sdat-get-sroot self) "/models/" model ".so")))
;;     (if (file-exists? model.so)
;; 	(load model.so)
;; 	(if (file-exists? model.scm)
;; 	    (load model.scm)
;; 	    (s:log "ERROR: model " model.scm " not found")))))

;; (define (session:model-path self model)
;;   (string-append (sdat-get-sroot self) "/models/" model ".scm"))

(define (session:pp-formdat self)
  (let ((dat (formdat:all->strings (sdat-get-formdat self))))
    (string-intersperse dat "<br> ")))

(define (session:param->string params)
  ;; (err:log "params=" params)
  (if (< (length params) 1)
      ""
      (let loop ((key (car params))
		 (val (cadr params))
		 (tail (cddr params))
		 (result '()))
	(let ((newresult (cons (string-append (s:any->string key) "=" (s:any->string val))
			       result)))
	  (if (< (length tail) 1) ;; true if done
	      (string-intersperse newresult "&")
	      (loop (car tail)(cadr tail)(cddr tail) newresult))))))

(define (session:link-to self page params)
  (let* ((server    (if (get-environment-variable "HTTP_HOST")
			(get-environment-variable "HTTP_HOST")
			(get-environment-variable "SERVER_NAME")))
	 (script (let ((script-name (string-split (get-environment-variable "SCRIPT_NAME") "/")))
		   (if (> (length script-name) 1)
		       (string-append (car script-name) "/" (cadr script-name))
		       (get-environment-variable "SCRIPT_NAME")))) ;; build script name from first two elements. This is a hangover from before I used ? in the URL.
	 (session-key (sdat-get-session-key self))
	 (paramstr (session:param->string params)))
    ;; (session:log self "server=" server " script=" script " page=" page)
    (string-append "http://" server "/" script "/" page "?" paramstr))) ;; "/sn=" session-key)))

(define (session:cgi-out self)
  (let* ((content  (list (sdat-get-content-type self))) ;; '("Content-type: text/html; charset=iso-8859-1\n\n"))
	 (header   (let ((cookie (sdat-get-session-cookie self)))
		     (if cookie
			 (cons (string-append "Set-Cookie: " (car cookie))
			       content)
			 content)))
	 (pagedat  (sdat-get-pagedat self)))
    (s:cgi-out 
     (cons header pagedat))))

(define (session:log self . msg)
  (with-output-to-port (sdat-get-log-port self) ;; (sdat-get-logpt self)
    (lambda () 
      (apply print msg))))

(define (session:get-param self key)
  ;; (session:log s:session "params=" (slot-ref s:session 'params))
  (let ((params (sdat-get-params self)))
    (session:get-param-from params key)))

;; This one will get the first value found regardless of form
(define (session:get-input self key)
  (let* ((formdat (sdat-get-formdat self)))
    (if (not formdat) #f
	(if (or (string? key)(number? key)(symbol? key))
	    (if (and (vector? formdat)(eq? (vector-length formdat) 1)(hash-table? (vector-ref formdat 0)))
		(formdat:get formdat key)
		(begin
		  (session:log self "ERROR: formdat: " formdat " is not of class <formdat>")
		  #f))
	    (session:log self "ERROR: bad key " key)))))

(define (session:run-actions self)
  (let* ((action    (session:get-param self 'action))
	 (page      (sdat-get-page self)))
    ;; (print "action=" action " page=" page)
    (if action
	(let ((action-lst  (string-split action ".")))
	  ;; (print "action-lst=" action-lst)
	  (if (not (= (length action-lst) 2)) 
	      (err:log "Action should be of form: module.action")
	      (let* ((targ-page   (car action-lst))
		     (proc-name   (string-append targ-page "-action"))
		     (targ-action (cadr action-lst)))
		;; (err:log "targ-page=" targ-page " proc-name=" proc-name " targ-action=" targ-action)

		;; call here only if never called before
		(if (session:never-called-page? self targ-page)
		    (session:call-parts self targ-page 'control))
		;;                    proc                         action    

		(if #t ;; set to #t to see better error messages during debuggin :-)
		    ((eval (string->symbol proc-name)) targ-action) ;; unsafe execution
		    (condition-case ((eval (string->symbol proc-name)) targ-action)
				    ((exn file) (s:log "file error"))
				    ((exn i/o)  (s:log "i/o error"))
				    ((exn )     (s:log "Action not implemented: " proc-name " action: " targ-action))
				    (var ()     (s:log "Unknown Error"))))))))))

(define (session:never-called-page? self page)
  (session:log self "Checking for page: " page)
  (not (member page (sdat-get-seen-pages self))))

(define (session:set-called! self page)
  (sdat-set-seen-pages! self (cons page (sdat-get-seen-pages self))))

;;======================================================================
;; Alternative data type delivery
;;======================================================================

(define (session:alt-out self)
  (let ((dat (sdat-get-alt-page-dat self)))
    ;; (s:log "dat is: " dat)
    ;; (print "HTTP/1.1 200 OK")
    (print "Date: " (time->string (seconds->utc-time (current-seconds))))
    (print "Content-Type: " (sdat-get-content-type self))
    (print "Accept-Ranges: bytes")
    (print "Content-Length: " (if (blob? dat)
				  (blob-size dat)
				  0))
    (print "Keep-Alive: timeout=15, max=100")
    (print "Connection: Keep-Alive")
    (print "")
    (write-string (blob->string dat) #f (current-output-port))))
