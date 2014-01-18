
(require-extension sqlite3 regex posix eformat silex stack regex)

(define help "
Usage: nldb [options]


General
  -h                      : this help

Netlist data queries

  -findpath start,end     : find path from start to end. % is a wildcard

Managing netlist data

  -load /path/to/netlist  : load a model into the db
  -d dbname               : name of the .db file
  -dump fname             : dump the netlist in to verilog file

")

(include "/nfs/an/home/mrwellan/stuff/tools/lnkmkr/args.scm")
(include "verilog.l.scm")

;; process args
(define remargs (get-args (argv)
			  (list "-load"
				"-d"          "-dump" 
				"-findpath")
			  
			  (list "-h"
				)
			  arg-hash
			  0)) ;;

(define dbpaths (list "testing.db"))

(define dbpath #f)

(if (get-arg "-d")
    (set! dbpath (get-arg "-d"))
    (for-each
     (lambda (path)
       (if (file-exists? path)
	   (set! dbpath path)))
     dbpaths))

(if (and (not dbpath) (get-arg "-d"))
    (begin
      (print "Can't find db. " (get-arg "-d") " Try again or contact Matt!")
      (exit 1)))

(define dbexists (file-exists? dbpath))

(define realuser (getenv "USER"))
(define user realuser)

(define db (sqlite3:open dbpath))
(sqlite3:set-busy-timeout! db 1000000)

(define (mk-tables)
  (for-each
   (lambda (sqlstmt)
     (sqlite3:exec db sqlstmt))
   (list "CREATE TABLE modules(id INTEGER PRIMARY KEY,name_id INTEGER);"
	 "CREATE TABLE nets   (id INTEGER PRIMARY KEY,name_id INTEGER,module_id INTEGER);"
	 "CREATE TABLE insts  (id INTEGER PRIMARY KEY,name_id INTEGER,module_id INTEGER,parent_id INTEGER);"
	 "CREATE TABLE pins   (id INTEGER PRIMARY KEY,name_id INTEGER,module_id INTEGER,net_id INTEGER,type_id INTEGER);"
	 "CREATE TABLE conns  (id INTEGER PRIMARY KEY,net_id  INTEGER,inst_id INTEGER,pin_id INTEGER);"
	 "CREATE TABLE names  (id INTEGER PRIMARY KEY,name TEXT);"
	 "CREATE TABLE types(id INTEGER PRIMARY KEY,type TEXT);"
	 "INSERT INTO types VALUES(1, 'undef');"
	 "INSERT INTO types VALUES(2, 'input');"
	 "INSERT INTO types VALUES(3, 'output');"
	 "INSERT INTO types VALUES(4, 'inout');"
	 "INSERT INTO types VALUES(5, 'pwr');"
	 "PRAGMA synchronous=OFF;")))

(if (not dbexists)(mk-tables))

;;======================================================================
;; NETLIST READING
;;======================================================================

;; Use a stack to tracking state
;;
(define nldb:*stack* (make-stack))

(define (nldb:read-files fnames) ;; read in a list of files
  (for-each 
   (lambda (fname)
     (if (file-exists? fname)
	 (nldb:read-file fname)))
   fnames))

;;======================================================================
;; PRECOMPILED REGEXS
;;======================================================================

(define nldb:escaped-name     (regexp "^\\s*\\\\([^\\s]+)\\s*"))
(define nldb:trailing-garbage (regexp "^\\s*([^\\s,;]+)[,;\\s]*$"))
(define nldb:module-pin       (regexp "^\\s*([^\\s]+)\\s*([,\\s\\)]*)"))
(define nldb:pins-end         (regexp "\\)\\s*;"))
(define nldb:input-output     (regexp "\\s*(input|output)\\s+([^\\s]+)[\\s;,]"))

;;                                           modname instname( .\pinname[35] (\netname ),
(define nldb:instance         (regexp "^\\s*([^\\s]+)\\s+([^\\s]+)\\s*\\(\\s*\\.([^\\s]+)\\s*\\(\\s*([^\\s]+)\\s*\\)\\s*,"))
(define nldb:inst-conn        (regexp "^\\s*\\.([^\\s]+)\\s*\\(\\s*([^\\s])+\\s+\\)\\s*([\\),;]+)"))

;;                                                 module_name         netname (opt)
(define nldb:module-regex (regexp "^\\s*module\\s+([^\\s]+)\\s*\\(\\s*([^\\s,]+\\s*,|)$"))

;;======================================================================
;; MISC
;;======================================================================

;; apply regex and set nldb:match-val
(define nldb:match-val #f)
(define (nldb:regex-match r l)
  (let ((m (string-match r l)))
    (set! nldb:match-val m) m))

;; stmt can only return *one* value!!
(define (nldb:sqlite3:get-one stmt . params)
  (let ((sqlstmt (sqlite3:prepare db stmt))
	(result  #f))
    (apply sqlite3:for-each-row
	   (lambda (x)
	     (set! result x)) sqlstmt params)
    (sqlite3:finalize! sqlstmt)
    result))

;;======================================================================
;; CACHE
;;======================================================================

(define *cache*             (make-hash-table))
(define *module-name-cache* (make-hash-table))

(define (cache-get-module-hash module)
  (sub-hash-create-get *cache* module))

(define (sub-hash-create-get subhash key)
  (let ((shash (hash-table-get/default subhash key)))
    (if shash shash
	(let ((newh (make-hash-table)))
	  (hash-table-set! subhash key newh)
	  newh))))

;; (cache-set! "abc_adder" 'pin "addrin" 0)
(define (cache-set! module objtype objname value)
  (let* ((mhash (cache-get-module-hash module))
	 (thash (sub-hash-create-get mhash objtype)))
    (hash-table-set! thash objname value)))

(define (cache-ref module objtype objname)
  (let ((mhash (hash-table-ref/default *cache* module)))
    (if mhash
	(let ((ohash (hash-table-ref/default mhash objtype)))
	  (if ohash
	      (hash-table-ref/default ohash objname)
	      #f))
	#f)))
    
;;======================================================================
;; NAMES
;;======================================================================

(define nldb:names-hash (make-hash-table))

;; always sucessful. inserts name if not found
(define (nldb:get-name-id name)
  (let ((cached-id (hash-table-ref/default nldb:names-hash name #f)))
    (if cached-id cached-id
	(let ((id (nldb:sqlite3:get-one "SELECT id FROM names WHERE name=?;" name)))
	  (if id
	      (begin
		(hash-table-set! nldb:names-hash name id )
		id)
	      (begin
		(sqlite3:exec db "INSERT INTO names (name) VALUES (?);" name)
		(nldb:get-name-id name)))))))

(define (nldb:clean-name name)
  (if (nldb:regex-match nldb:escaped-name name) ;; process escaped identifiers
      (list-ref nldb:match-val 1)
      (if (nldb:regex-match nldb:trailing-garbage name)
	  (list-ref nldb:match-val 1)
	  name)))

;;======================================================================
;; MODULES
;;======================================================================

;; add a module and return its id.
(define (nldb:get-module-id name-id)
  (let ((id  (nldb:sqlite3:get-one 
	      "SELECT id FROM modules WHERE name_id=?;" name-id)))
    (if id id
	(begin
	  (nldb:insert-module name-id)
	  (nldb:get-module-id name-id))))) ;; now retrieve and return the id

;; not safe to use outside of get-module-id - could add duplicates
(define (nldb:insert-module name-id)
  (sqlite3:exec db "INSERT INTO modules (name_id) VALUES (?);" name-id))

;; module namespace is unique so this is ok, should check for redefining though.
(define (nldb:get-module-by-name name)
  (let ((module-id (hash-table-ref *module-name-cache* name)))
    (if module-id module-id
	(let ((mid (nldb:get-module-id (nldb:get-name-id name))))
	  (hash-table-set! *module-name-cache* name mid)))))

;;======================================================================
;; PINS
;;======================================================================

(define (nldb:get-pin-id module-id name-id)
  (nldb:sqlite3:get-one 
   (string-append "SELECT id FROM pins WHERE module_id=? AND name_id=?;") 
   module-id name-id))

(define (nldb:add-pin module-id name-id type-id)
  (let ((pin-id (nldb:get-pin-id module-id name-id)))
    (if pin-id pin-id
	(begin	
	  (nldb:insert-pin module-id name-id type-id)
	  (nldb:get-pin-id module-id name-id)))))

(define (nldb:insert-pin module-id name-id type-id)
  (sqlite3:exec db "INSERT INTO pins (module_id,name_id,type_id) VALUES (?,?,?);"
		module-id name-id (if type-id type-id 0)))

(define (nldb:set-pin-direction pin-id direction)
  (sqlite3:exec db "UPDATE pins SET type_id=(SELECT id FROM types WHERE type=?) WHERE id=?;" direction pin-id))

(define (nldb:set-pin-net pin-id net-id)
  (sqlite3:exec db "UPDATE pins SET net_id=? WHERE id=?;" net-id pin-id))

;;====================================================================
;; CONNS
;;======================================================================

(define (nldb:get-conn-id inst-id pin-id)
  ;; (if (not (and inst-id pin-id))(print "ERROR: nldb:get-conn-id called with bad params: inst-id " inst-id " pin-id " pin-id)
  (nldb:sqlite3:get-one  "SELECT id FROM conns WHERE inst_id=? AND pin_id=?;" inst-id pin-id))

(define (nldb:add-conn inst-id pin-id net-id)
  ;;  (if (not (and inst-id pin-id net-id))(print "ERROR: nldb:add-conn called with bad params: inst-id " inst-id " pin-id " pin-id " net-id " net-id)
  (let ((conn-id (nldb:get-conn-id inst-id pin-id)))
    (if conn-id conn-id
	(begin	
	  (nldb:insert-conn inst-id pin-id net-id)
	  (nldb:get-conn-id inst-id pin-id)))))

(define (nldb:insert-conn inst-id pin-id net-id)
  ;;  (if (not (and inst-id pin-id net-id))(print "ERROR: nldb:insert-conn called with bad params: inst-id " inst-id " pin-id " pin-id " net-id " net-id)
  (sqlite3:exec db "INSERT INTO conns (inst_id,pin_id,net_id) VALUES (?,?,?);"
		inst-id pin-id net-id ))

;;======================================================================
;; NET
;;======================================================================

(define (nldb:get-net-id module-id name-id)
  (nldb:sqlite3:get-one "SELECT id FROM nets WHERE name_id=?;" name-id))

(define (nldb:add-net module-id name-id)
  (let ((net-id (nldb:get-net-id module-id name-id)))
    (if net-id net-id
	(begin
	  (nldb:insert-net module-id name-id)
	  (nldb:get-net-id module-id name-id)))))

(define (nldb:insert-net module-id name-id)
  (sqlite3:exec db "INSERT INTO nets (module_id,name_id) VALUES(?,?);" module-id name-id))

;;======================================================================
;; INSTANCES
;;======================================================================

(define (nldb:get-inst-id parent-id name-id)
  (nldb:sqlite3:get-one "SELECT id FROM insts WHERE parent_id=? AND name_id=?;" parent-id name-id))

;; sub-mod-id = type of instance, parent-id = where instantiated
(define (nldb:add-inst module-id parent-id name-id)
  (let ((inst-id (nldb:get-inst-id parent-id name-id))) ;; parent and name are enough to identify it
    (if inst-id inst-id
	(begin
	  (nldb:insert-inst module-id parent-id name-id)
	  (nldb:get-inst-id parent-id name-id)))))

(define (nldb:insert-inst module-id parent-id name-id)
  (sqlite3:exec db "INSERT INTO insts (module_id,parent_id,name_id) VALUES(?,?,?);" module-id parent-id name-id))

;;======================================================================
;; RECORD FOR STATE
;;======================================================================

(define *statevec* (make-vector 5))

(define-inline (curr-pin-id)           (vector-ref  *statevec* 0))
(define-inline (curr-inst-id)          (vector-ref  *statevec* 1))
(define-inline (curr-module-id)        (vector-ref  *statevec* 2))
(define-inline (curr-inst-module-id)   (vector-ref  *statevec* 3))

(define-inline (set-curr-pin-id!         id)(vector-set! *statevec* 0 id))
(define-inline (set-curr-inst-id!        id)(vector-set! *statevec* 1 id))
(define-inline (set-curr-module-id!      id)(vector-set! *statevec* 2 id))
(define-inline (set-curr-inst-module-id! id)(vector-set! *statevec* 3 id))

;;======================================================================
;; FILE I/O
;;======================================================================

;; Initialization and support routines for nldb:read-file
(stack-push! nldb:*stack* 'start)
(define nldb:esc-regex  (regexp "^\\\\([^\\s]*)\\s*$") )
(define (nldb:clean-identifier token)
  (let* ((t   (car token))
	 (v   (cadr token))
	 (ctm (string-match nldb:esc-regex v)))
    (list 'identifier (list-ref ctm 1))))


(define (nldb:read-file fname)
  (let* ((inp (open-input-file fname))
	 (prev-state #f))
    (lexer-init 'port inp)
    (let loop ((token          (lexer)))
      (let ((token-type (car token))
	    (token-val  (cadr token))
	    (state      (stack-peek herc:*stack*)))
	(if (not (eq? prev-state state))
	    (begin
	      (print "state: " state)
	      (set! prev-state state)))
	(case token-type
	  ('end-of-input       (print "Done")(close-input-port inp))
	  ('whitespace         (loop (lexer)))  ;; skip whitespace
	  ('comment-begin      
	   (stack-push! herc:*stack* 'comment )
	   (loop (lexer)))
	  ('comment-end        (stack-pop! herc:*stack*)(loop (lexer)))
	  ('begin              (stack-push! herc:*stack* 'begin)(loop (lexer)))
	  ('end                (stack-pop! herc:*stack*)(loop (lexer)))
	  ('cell
	   (case state
	     ('begin
	       (stack-push! herc:*stack* 'cell-name)
	       (loop (lexer)))
	     (else
	      (loop (lexer)))))
	  ('plainidentifier
	   (case state
	     ('cell-name

	  ('statementend       (stack-pop! nldb:*stack*)(loop (lexer)))
	  ('endparen           (stack-pop! nldb:*stack*)(loop (lexer)))
	  ('endmodule          (stack-pop! nldb:*stack*)(loop (lexer)))

	  ('startparen 
	   (case state
	     ('module-pins     (loop (lexer)))
	     ('inst-def        (loop (lexer)))
	     ('inst-conn-def   (loop (lexer)))
	     ('pin-net         (loop (lexer)))
	     (else             (print "ERROR: Didn't expect an open paren here! Line " (lexer-get-line)))))

	  ('comma
	   (case state
	     ('module-pins     (loop (lexer)))
	     ('input-pin       (loop (lexer)))
	     ('output-pin      (loop (lexer)))
	     ('wire            (loop (lexer)))
	     ('inst-conn-def   (loop (lexer))) ;; (stack-pop! nldb:*stack*) (loop (lexer)))
	     (else             (print "ERROR: Didn't expect a comma here! Line " (lexer-get-line)))))

	  ('module 
	   (case state
	     ('start 
	      (stack-push! nldb:*stack* 'module)      ;; we will be in a module
	      (stack-push! nldb:*stack* 'module-def)) ;; starting in the def
	     (else
	      (print "ERROR: Didn't expect module declaration here! Line " (lexer-get-line))))
	   (loop (lexer)))

	  ('input 
	   (case state
	     ('module      (stack-push! nldb:*stack* 'input-pin))
	     (else         (print "ERROR: Didn't expect \"input\" statement here! Linenum " (lexer-get-line))))
	   (loop (lexer)))

	  ('output
	   (case state
	     ('module      (stack-push! nldb:*stack* 'output-pin))
	     (else         (print "ERROR: Didn't expect \"output\" statement here! Linenum " (lexer-get-line))))
	   (loop (lexer)))
	  
	  ('inout
	   (case state
	     ('module      (stack-push! nldb:*stack* 'inout-pin))
	     (else         (print "ERROR: Didn't expect \"inout\" statement here! Linenum " (lexer-get-line))))
	   (loop (lexer)))

	  ('pin 
	   (case state
	     ('inst-conn-def
	      (let* ((pin-name    (substring token-val 1 (string-length token-val)))
		     (pin-name-id (nldb:get-name-id pin-name))
		     (pin-id      (nldb:add-pin (curr-module-id) pin-name-id #f)))
		(stack-push! nldb:*stack* 'pin-net)
		(set-curr-pin-id! pin-id)
		(loop (lexer))))
	     (else  (print "ERROR: Didn't expect pin here " token-val " Linenum: " (lexer-get-line)))))

	  ('identifier
	   (case state
	     ('module  ;; this must be an instance, an identifier at the top level
	      (let* ((inst-mod-id (nldb:get-module-by-name token-val)))
		(set-curr-inst-module-id! inst-mod-id)
		(stack-push! nldb:*stack* 'inst-def))
	      (loop (lexer)))
	     ('inst-def                  ;;     inst-module type  parent-id    inst-name-id
	      (let* ((inst-id (nldb:add-inst (curr-inst-module-id)(curr-module-id)(nldb:get-name-id token-val))))
		(set-curr-inst-id! inst-id))
	      (stack-push! nldb:*stack* 'inst-conn-def)
	      (loop (lexer)))
	     ('module-def
	      (let* ((m-id (nldb:get-module-by-name token-val)))
		(set-curr-module-id! m-id))
	      (stack-push! nldb:*stack* 'module-pins))
	     ('module-pins
	      (nldb:add-pin (curr-module-id) (nldb:get-name-id token-val) #f))
	     ('input-pin
	      (let ((pin-id (nldb:get-pin-id (curr-module-id) (nldb:get-name-id token-val))))
		(nldb:set-pin-direction pin-id "input")))
	     ('output-pin
	      (let ((pin-id (nldb:get-pin-id (curr-module-id) (nldb:get-name-id token-val))))
		(nldb:set-pin-direction pin-id "output")))
	     ('inout-pin
	      (let ((pin-id (nldb:get-pin-id (curr-module-id) (nldb:get-name-id token-val))))
		(nldb:set-pin-direction pin-id "inout")))
	     ('pin-net
	      (let* ((net-name-id (nldb:get-name-id token-val))
		     (net-id      (nldb:add-net (curr-inst-module-id) net-name-id)))
		(nldb:add-conn (curr-inst-id) (curr-pin-id) net-id)))
	     (else
	      (print "ERROR: Didn't expect an identifier here! Token " token-val " Line " (lexer-get-line))))
	   (loop (lexer)))

	  (else
	   (print "ERROR: unknown token " token " on line " (lexer-get-line))
	   (loop (lexer))))))))
     
