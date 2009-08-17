;; twiki module
(require-extension silex sqlite3 regex posix)

(include "twiki.l.scm")

(define (twiki:open-db keys)
  (let* ((fname   (twiki:keys->fname keys))
	 (fexists (file-exists? fname))
	 (db (dbi:open 'sqlite3 '((dbname . fname)))))
    (if (not fexists)
	(for-each 
	 (lambda (sqry)
	   (dbi:exec db sqry))
	 '("CREATE TABLE dats     (id INTEGER PRIMARY KEY,md5sum TEXT,dat BLOB,type INTEGER);"
	   "CREATE TABLE tiddlers (id INTEGER PRIMARY KEY,wiki_id INTEGER,name TEXT,rev INTEGER,dat_id INTEGER,created_on INTEGER,changed_on INTEGER,owner_id INTEGER);"
	   "CREATE TABLE revs     (id INTEGER PRIMARY KEY,tag TEXT);"
	   "CREATE TABLE wikis    (id INTEGER PRIMARY KEY,key_name TEXT,title TEXT,created_on INTEGER);")))
    (sqlite3:set-busy-timeout!(dbi:db-conn db) 1000000)
    db))
	
(define (twiki:view)
  (s:div 'class "node"
  (s:h1 "Twiki")
  "Title, pictures, etc."
   (let ()
     "blah")))


(define (twiki:wiki . keys)
  (let ((key (conc keys)))
    (twiki:view)))

(define (twiki:extract-tiddlers dat)
  (let* ((inp (open-input-string dat))
	 (prev-state #f)
	 (stack      (list 'start))
	 (links      '())
	 (currlnk    #f))
    (lexer-init 'port inp)
    (let loop ((token          (lexer)))
      (let ((token-type (car token))
	    (token-val  (cadr token))
	    (state      (car  stack)))
	(if (not (eq? prev-state state))
	    (begin
	      (print "state: " state)
	      (set! prev-state state)))
	(case token-type
	  ('end-of-input       (print "Done")(close-input-port inp))
	  ('twikilink-start
	   (set! stack (cons 'twikilink-start stack))
	   (loop (lexer)))
	  ('twikilink-end
	   (set! links (cons currlnk links))
	   (set! stack (cdr stack))
	   (loop (lexer)))
	  ('twikitext
	   (if (eq? state 'twikilink-start)
	       (set! currlnk (cadr token))
	       (print "Got " token))
	   (loop (lexer)))
	  ('anydat
	   (loop (lexer)))
	  (else
	   (print "ERROR: unknown token " token " on line " (lexer-get-line))
	   (loop (lexer))))))
    links))
     
