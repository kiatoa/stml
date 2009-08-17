;; Copyright 2007-2008, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(include "requirements.scm")

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

(define-class <session> ()
  (dbtype       ;; 'pg or 'sqlite3
   dbinit
   conn
   params       ;; params from the key=val&key1=val2 string
   path-params  ;; remaining params from the path
   session-key
   session-id
   domain
   toppage      ;; defaults to "index" - override in .stml.config if desired
   page         ;; the page name - defaults to home
   curr-page    ;; the current page being evaluated
   content-type ;; the default content type is text/html, override to deliver other stuff
   page-type    ;; use in conjunction with content-type to deliver other payloads
   sroot
   pagedat
   pagevars     ;; session vars specific to this page
   pagevars-before
   sessionvars  ;; session vars visible to all pages
   sessionvars-before
   globalvars   ;; global vars visible to all sessions
   globalvars-before
   logpt
   formdat
   request-method
   session-cookie
   curr-err
   log-port
   logfile
   seen-pages))

;; SPLIT INTO STRAIGHT FORWARD INIT AND COMPLEX INIT
(define-method (initialize (self <session>) initargs)
  (call-next-method)
  (slot-set! self 'dbtype      'pg)
  (slot-set! self 'page        "home")        ;; these are defaults
  (slot-set! self 'curr-page   "home")
  (slot-set! self 'content-type "Content-type: text/html; charset=iso-8859-1\n\n")
  (slot-set! self 'page-type   'html)
  (slot-set! self 'toppage     "index")
  (slot-set! self 'params      '())           ;;
  (slot-set! self 'path-params '())
  (slot-set! self 'session-key #f)
  (slot-set! self 'pagedat     '())
  (slot-set! self 'sroot       "./")
  (slot-set! self 'session-cookie #f)
  (slot-set! self 'curr-err #f)
  (slot-set! self 'log-port (current-error-port))
  (slot-set! self 'seen-pages '())
  (for-each (lambda (slot-name)
              (slot-set! self slot-name (make-hash-table)))
            (list 'pagevars 'sessionvars 'globalvars 'pagevars-before 
		  'sessionvars-before 'globalvars-before))
  (slot-set! self 'domain "locahost")   ;; end of defaults
  (initialize-slots self (session:read-config self))
  ;; some values read in from the config file need to be evaled
  (for-each (lambda (slot-name)
	      (slot-set! self slot-name (eval (slot-ref self slot-name))))
	    (list 'dbtype))
  (initialize-slots self initargs))

(define-method (session:setup (self <session>))
  (let ((dbtype (slot-ref self 'dbtype))
	(dbinit (eval (slot-ref self 'dbinit)))
	(dbexists #f))
    (let ((dbfname (alist-ref 'dbname dbinit)))
      (if (eq? dbtype 'sqlite3)
	  (if (file-exists? dbfname)
	      (begin
		;; (session:log self "setting dbexists to #t")
		(set! dbexists #t))))
      ;; (session:log self "dbtype: " dbtype " dbfname: " dbfname " dbexists: " dbexists))
      )
    (slot-set! self 'conn (dbi:open dbtype dbinit))
    (if (and (not dbexists)(eq? dbtype 'sqlite3))
 	(begin
	  (print "WARNING: Setting up session db with sqlite3")
	  (session:setup-db self)))
    (session:process-url-path self)
    (session:setup-session-key self)
    ;; capture stdin if this is a POST
    (slot-set! self 'request-method (getenv "REQUEST_METHOD"))
    (slot-set! self 'formdat (formdat:load-all))))

;; setup the db with session tables, works for sqlite only right now
(define-method (session:setup-db (self <session>))
  (let ((conn (slot-ref self 'conn)))
    (for-each 
     (lambda (stmt)
       (dbi:exec conn stmt))
     (list "CREATE TABLE session_vars (id integer PRIMARY KEY,session_id integer,page text,key text,value text);"
	   "CREATE TABLE sessions (id integer PRIMARY KEY,session_key text);"
           "CREATE TABLE metadata (id integer PRIMARY KEY,key TEXT,value TEXT);"))))
;;  ;; if we have a session_key look up the session-id and store it
;;  (slot-set! self 'session-id (session:get-id self)))

;; only set session-cookie when a new session is created
(define-method (session:setup-session-key (self <session>))  
  (let* ((sk  (session:extract-session-key self))
         (sid (if sk (session:get-id self sk) #f)))
    (if (not sid) ;; need a new key
        (let* ((new-key (session:get-new-key self))
               (new-sid (session:get-id self new-key)))
          (slot-set! self 'session-key new-key)
          (slot-set! self 'session-id new-sid)
          (slot-set! self 'session-cookie (session:make-cookie self)))
        (slot-set! self 'session-id sid))))

(define-method (session:make-cookie (self <session>))
  ;; (list (conc "session_key=" (slot-ref self 'session-key) "; Path=/; Domain=." (slot-ref self 'domain) "; Max-Age=" (* 86400 14) "; Version=1"))) 
  (list (string-substitute 
	 ";" "; " 
	 (car (construct-cookie-string 
	       ;; warning! messing up this itty bitty bit of code will cost much time!
	       `(("session_key" ,(slot-ref self 'session-key)
		  expires: ,(+ (current-seconds) (* 14 86400)) 
		  max-age: (* 14 86400)
		  path: "/" ;; 
		  domain: ,(string-append "." (slot-ref self 'domain))
		  version: 1)) 0)))))

;; look up a given session key and return the id if found, #f if not found
(define-method (session:get-id (self <session>) session-key)
  ;; (let ((session-key (slot-ref self 'session-key)))
  (if session-key
      (let ((query (string-append "SELECT id FROM sessions WHERE session_key='" session-key "'"))
            (conn (slot-ref self 'conn))
            (result #f))
	(dbi:for-each-row 
	 (lambda (tuple)
	   (set! result (vector-ref tuple 0)))
	 conn query)
        result)
      #f))

;; 
(define-method (session:process-url-path (self <session>))
  (let ((path-info    (getenv "PATH_INFO"))
	(query-string (getenv "QUERY_STRING")))
    ;; (session:log self "path-info=" path-info " query-string=" query-string)
    (if path-info
	(let* ((parts    (string-split path-info "/"))
	       (numparts (length parts)))
	  (if (> numparts 0)
	      (slot-set! self 'page (car parts)))
	  ;; (session:log self "url-path=" url-path " parts=" parts)
	  (if (> numparts 1)
	      (slot-set! self 'path-params (cdr parts)))
          (if query-string
              (slot-set! self 'params (string-split query-string "&")))))))

;; BUGGY!
(define-method (session:get-new-key (self <session>))
  (let ((conn   (slot-ref self 'conn))
        (tmpkey (session:make-rand-string 20))
        (status #f))
    (dbi:for-each-row (lambda (tuple)
			(set! status #t))
		      conn (string-append "INSERT INTO sessions (session_key) VALUES ('" tmpkey "')"))
    tmpkey))

;; returns session key IFF it is in the HTTP_COOKIE 
(define-method (session:extract-session-key (self <session>))
  (let ((http-session (getenv "HTTP_COOKIE")))
    (if http-session 
        (session:extract-key-from-param self (list http-session) "session_key")
        #f)))

(define-method (session:get-session-id (self <session>) session-key)
  (let ((query "SELECT id FROM sessions WHERE session_key=?;")
        (result #f))
    ;;     (pg:query-for-each (lambda (tuple)
    ;;                          (set! result (vector-ref tuple 0))) ;; (vector-ref tuple 0)))
    ;;                        (s:sqlparam query session-key)
    ;;                        (slot-ref self 'conn))
    ;;                        conn)
    (dbi:for-each-row (lambda (tuple)
			(set! result (vector-ref tuple 0))) ;; (vector-ref tuple 0)))
		      (slot-ref self 'conn)
		      (s:sqlparam query session-key))
    result))

;; delete all records for a session
;;
(define-method (session:delete-session (self <session>) session-key)
  (let ((session-id (session:get-session-id self session-key))
        (queries    (list "DELETE FROM session_vars WHERE session_id=?;"
                          "DELETE FROM sessions WHERE id=?;"))
        (conn              (slot-ref self 'conn)))
    (if session-id
        (begin
          (for-each
           (lambda (query)
             (dbi:exec conn query session-id))
	   queries)
	  (initialize self '())
	  (session:setup self)))
    (not (session:get-session-id self session-key))))

(define-method (session:extract-key (self <session>) key)
  (let ((params (slot-ref self 'params)))
    (session:extract-key-from-param self params key)))

(define-method (session:extract-key-from-param (self <session>) params key)
  (let ((r1     (regexp (string-append "^" key "=([^=]+)$"))))
    (if (< (length params) 1) #f
	(let loop ((head   (car params))
		   (tail   (cdr params)))
	  (let ((match (string-match r1 head)))
	    (cond
	     (match
	      (let ((session-key (list-ref match 1)))
		(slot-set! self 'session-key (list-ref match 1))
		session-key))
	     ((null? tail)
	      #f)
	     (else
	      (loop (car tail)
		    (cdr tail)))))))))

(define-method (session:set-page! (self <session>) page_name)
  (slot-set! self 'page page_name))

(define-method (session:close (self <session>))
  (dbi:close (slot-ref self 'conn)))
;; (close-output-port (slot-ref self 'logpt))

(define-method (session:err-msg (self <session>) msg)
  (hash-table-set! (slot-ref self 'sessionvars) "ERROR_MSG"
		   (string-intersperse (map s:any->string msg) " ")))

(define-method (session:prev-err (self <session>))
  (let ((prev-err (hash-table-ref/default (slot-ref self 'sessionvars-before) "ERROR_MSG" #f))
	(curr-err (hash-table-ref/default (slot-ref self 'sessionvars) "ERROR_MSG" #f)))
    (if prev-err prev-err
	(if curr-err curr-err #f))))

;; session vars
;; 1. keys are always a string NOT a symbol
;; 2. values are always a string conversion is the responsibility of the 
;;    consuming function (at least for now, I'd like to change this)

;; set a session var for the current page
;;
(define-method (session:set! (self <session>) key value)
  (hash-table-set! (slot-ref self 'pagevars) (s:any->string key) (s:any->string value)))

;; del a var for the current page
;;
(define-method (session:del! (self <session>) key)
  (hash-table-delete! (slot-ref self 'pagevars) (s:any->string key)))

;; get the appropriate hash given a page "*sessionvars*, *globalvars* or page
;;
(define-method (session:get-page-hash (self <session>) page)
  (if (string=? page "*sessionvars*")
      (slot-ref self 'sessionvars)
      (if (string=? page "*globalvars*")
	  (slot-ref self 'globalvars)
	  (slot-ref self 'pagevars))))

;; set a session var for a given page
;;
(define-method (session:set! (self <session>) page key value)
  (let ((ht (session:get-page-hash self page)))
    (hash-table-set! ht (s:any->string key) (s:any->string value))))

;; get session vars for the current page
;;
(define-method (session:get (self <session>) key)
  (hash-table-ref/default (slot-ref self 'pagevars) key #f))

;; get session vars for a specified page
;;
(define-method (session:get (self <session>) page key)
  (let ((ht (session:get-page-hash self page)))
    (hash-table-ref/default ht key #f)))

;; delete a session var for a specified page
;;
(define-method (session:del! (self <session>) page key)
  (let ((ht (session:get-page-hash self page)))
    (hash-table-delete! ht key)))

;; get ALL keys for this page and store in the session pagevars hash
;;
(define-method (session:get-vars (self <session>))
  (let ((session-id  (slot-ref self 'session-id)))
    (if (not session-id)
	(err:log "ERROR: No session id in session object! session:get-vars")
	(let* ((result             #f)
	       (conn               (slot-ref self 'conn))
	       (pagevars-before    (slot-ref self 'pagevars-before))
	       (sessionvars-before (slot-ref self 'sessionvars-before))
	       (globalvars-before  (slot-ref self 'globalvars-before))
	       (pagevars           (slot-ref self 'pagevars))
	       (sessionvars        (slot-ref self 'sessionvars))
	       (globalvars         (slot-ref self 'globalvars))
	       (page-name          (slot-ref self 'page))
	       (session-key        (slot-ref self 'session-key))
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

(define-method (session:save-vars (self <session>))
  (let ((session-id  (slot-ref self 'session-id)))
    (if (not session-id)
	(err:log "ERROR: No session id in session object! session:get-vars")
	(let* ((status      #f)
	       (conn        (slot-ref self 'conn))
	       (page-name   (slot-ref self 'page))
	       (del-query   "DELETE FROM session_vars WHERE session_id=? AND page=? AND key=?;")
	       (ins-query   "INSERT INTO session_vars (session_id,page,key,value) VALUES(?,?,?,?);")
	       (upd-query   "UPDATE session_vars set value=? WHERE key=? AND session_id=? AND page=?;")
	       (changed-count 0))
	  ;; save the delta only
	  (for-each
	   (lambda (page) ;; page is: "*globalvars*" "*sessionvars*" or otherstring
	     (let* ((master-slot-name (cond
				       ((string=? page "*sessionvars*") 'sessionvars)
				       ((string=? page "*globalvars*")  'globalvars)
				       (else 'pagevars)))
		    (before-slot-name (string->symbol (string-append (symbol->string master-slot-name)
								     "-before")))
		    (master-ht   (slot-ref self master-slot-name))
		    (before-ht   (slot-ref self before-slot-name))
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

;; 	  ;; (print del-query)
;;           (for-each
;;            (lambda (page)
;;              (pg:query-for-each (lambda (tuple)
;;                                   (set! status #t))
;;                                 (s:sqlparam del-query session-id page-name)
;;                                 conn))
;;            (list page-name "*sessionvars"))
;;           ;; NOTE: The following approach is inefficient and a little dangerous. Need to keep
;;           ;;       two hashes, before and after and use the delta to drive updating the db OR
;;           ;;       even better move to using rpc with a central process for maintaining state
;;           ;; write the session page specific vars to the db
;; 	  (for-each (lambda (key)
;; 		      (pg:query-for-each (lambda (tuple)
;; 					   (set! status #t))
;; 					 (s:sqlparam ins-query session-id page-name
;;                                                      (s:any->string key) ;; just in case it is a symbol
;;                                                      (hash-table-ref pagevars key))
;; 					 conn))
;; 		    (hash-table-keys pagevars))
;;           ;; write the session specific vars to the db
;;           ;; BUG!!! THIS IS LAZY AND WILL BREAK FOR SOMEONE ACCESSING THE SAME SESSION FROM TWO WINDOWS!!!
;; 	  (for-each (lambda (key)
;; 		      (pg:query-for-each (lambda (tuple)
;; 					   (set! status #t))
;; 					 (s:sqlparam ins-query session-id "*sessionvars*"
;;                                                      (s:any->string key) ;; just in case it is a symbol
;;                                                      (hash-table-ref sessionvars key))
;; 					 conn))
;; 		    (hash-table-keys sessionvars))
;;           ;; global vars will require a little more care - delaying for now.
;;           ))))

;; (pg:sql-null-object? element)
(define-method (session:read-config (self <session>))
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

(define-method (session:call-parts (self <session>) page parts)
  (slot-set! self 'curr-page page)
  (let* ((dir     (string-append (slot-ref self 'sroot) "/pages/" page))
	 (control (string-append dir "/control.scm"))
	 (view    (string-append dir "/view.scm"))
	 (load-view    (and (file-exists? view)
			    (or (eq? parts 'both)(eq? parts 'view))))
	 (load-control (and (file-exists? control)
			    (or (eq? parts 'both)(eq? parts 'control))))
	 (view-dat   '())
	 (sugar "/home/matt/kiatoa/stml/sugar.scm" ))
    ;; (print "dir=" dir " control=" control " view=" view " load-view=" load-view " load=control=" load-control)
    (if load-control
	(begin
	  (load control)
	  (session:set-called! self page)))
    ;; move this to where it gets exectuted only once
    ;;
    (if load-view
	;; option one.:
	;;
	;; (let ((inp (open-input-string 
	;; 	    (files-read->string "/home/matt/kiatoa/stml/sugar.scm" 
	;; 				view))))
	;;   (map 
	;;    (lambda (x)
	;;      (cond
	;;       ((list? x) x)
	;;       ((string? x) x)
	;;       (else '())))
	;;    (port-map eval (lambda ()
	;; 		 (read inp)))))
	;;
	;; option two:
	;;
	(let* (;; (inps (map open-input-file (list view))) ;; sugar view)))
	       (p    (open-input-file view)) ;; (apply make-concatenated-port inps))
	       (dat  (map 
		      (lambda (x)
			(cond
			 ((list? x) x)
			 ((string? x) x)
			 (else '())))
		      (port-map eval (lambda ()(read p))))))
	  ;; (map close-input-port inps)
	  (close-input-port p)
	  dat)
	(list "<p>Page not found " page " </p>"))))

(define-method (session:call (self <session>) page)
  (session:call-parts self page 'both))

(define-method (session:call (self <session>) page parts)
  (session:call-parts self page 'both))

(define-method (session:load-model (self <session>) model)
  (let ((model.scm (string-append (slot-ref self 'sroot) "/models/" model ".scm"))
	(model.so  (string-append (slot-ref self 'sroot) "/models/" model ".so")))
    (if (file-exists? model.so)
	(load model.so)
	(if (file-exists? model.scm)
	    (load model.scm)
	    (s:log "ERROR: model " model.scm " not found")))))

(define-method (session:model-path (self <session>) model)
  (string-append (slot-ref self 'sroot) "/models/" model ".scm"))

(define-method (session:pp-formdat (self <session>))
  (let ((dat (formdat:all->strings (slot-ref self 'formdat))))
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

(define-method (session:link-to (self <session>) page params)
  (let* ((server    (if (getenv "HTTP_HOST")
			(getenv "HTTP_HOST")
			(getenv "SERVER_NAME")))
	 (script (let ((script-name (string-split (getenv "SCRIPT_NAME") "/")))
		   (if (> (length script-name) 1)
		       (string-append (car script-name) "/" (cadr script-name))
		       (getenv "SCRIPT_NAME")))) ;; build script name from first two elements. This is a hangover from before I used ? in the URL.
	 (session-key (slot-ref self 'session-key))
	 (paramstr (session:param->string params)))
    ;; (session:log self "server=" server " script=" script " page=" page)
    (string-append "http://" server "/" script "/" page "?" paramstr))) ;; "/sn=" session-key)))

(define-method (session:cgi-out (self <session>))
  (let* ((content  (list (slot-ref self 'content-type))) ;; '("Content-type: text/html; charset=iso-8859-1\n\n"))
	 (header   (let ((cookie (slot-ref self 'session-cookie)))
		     (if cookie
			 (cons (string-append "Set-Cookie: " (car cookie))
			       content)
			 content)))
	 (pagedat  (slot-ref self 'pagedat)))
    (s:cgi-out 
     (cons header pagedat))))

(define-method (session:log (self <session>) . msg)
  (with-output-to-port (slot-ref self 'log-port) ;; (slot-ref self 'logpt)
    (lambda () 
      (apply print msg))))

(define-method (session:get-param (self <session>) key)
  ;; (session:log s:session "params=" (slot-ref s:session 'params))
  (let ((params (slot-ref self 'params)))
    (session:get-param-from params key)))

;; This one will get the first value found regardless of form
(define-method (session:get-input (self <session>) key)
  (let* ((formdat (slot-ref self 'formdat)))
    (if (not formdat) #f
	(formdat:get formdat key))))

(define-method (session:run-actions (self <session>))
  (let* ((action    (session:get-param self 'action))
	 (page      (slot-ref self 'page)))
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

(define-method (session:never-called-page? (self <session>) page)
  (session:log self "Checking for page: " page)
  (not (member page (slot-ref self 'seen-pages))))

(define-method (session:set-called! (self <session>) page)
  (slot-set! self 'seen-pages (cons page (slot-ref self 'seen-pages))))

;;======================================================================
;; Alternative data type delivery
;;======================================================================

(define-method (session:alt-out (self <session>))
  '())