;; twiki module
(require-extension sqlite3 regex posix md5 base64)

;; This is the currently supported mechanism. Postgres will be added later -mrw- 7/26/2009
;;
(define (twiki:open-db key)
  ;; (s:log "Got to twiki:open-db with key: " key)
  (let* ((fdat    (twiki:key->fname key))
	 (fpath   (car fdat))
	 (fname   (cadr fdat))
	 (fullname (conc fpath "/" fname))
	 (fexists (file-exists? fullname))
	 (db      (if fexists (dbi:open 'sqlite3 (list (cons 'dbname fullname))) #f)))
    (if (not fexists)
	(begin
	  ;; (print "fullname: " fullname)
	  (system (conc "mkdir -p " fpath)) ;; create the path
	  (set! db (dbi:open 'sqlite3 (list (cons 'dbname fullname))))
	  (for-each 
	   (lambda (sqry)
	     ;; (print sqry)
	     (dbi:exec db sqry))
	   ;; types: 0 text, 1 jpg, 2 png, 3 svg, 4 spreadsheet, 5 audio, 6 video :: better specs to come...
	   (list
	    "CREATE TABLE pics     (id INTEGER PRIMARY KEY,name TEXT,dat_id INTEGER,thumb_dat_id INTEGER,created_on INTEGER,owner_id INTEGER);"
	    "CREATE TABLE dats     (id INTEGER PRIMARY KEY,md5sum TEXT,dat BLOB,type INTEGER);"
	     ;; on every modification a new tiddlers entry is created. When displaying the tiddlers do:
	     ;;    select where created_on < somedate order by created_on desc limit 1
	     "CREATE TABLE tiddlers (id INTEGER PRIMARY KEY,wiki_id INTEGER,name TEXT,rev INTEGER,dat_id INTEGER,created_on INTEGER,owner_id INTEGER);"
	     ;; rev and tag only utilized when user sets a tag. All results from a select as above for tiddlers are set to the tag
	     "CREATE TABLE revs     (id INTEGER PRIMARY KEY,tag TEXT);"
	     ;; wikis is here for when postgresql support is added or if a sub wiki is created. 
	     "CREATE TABLE wikis    (id INTEGER PRIMARY KEY,key_name TEXT,title TEXT,created_on INTEGER);"
	     ;; need to create an entry for *this* twiki
	     (conc "INSERT INTO wikis (id,key_name,title,created_on) VALUES (1,'" key "','main'," (current-seconds) ");")))
	  ;;     (conc "INSERT INTO tiddlers (wiki_id,name,created_on) VALUES(1,'MainMenu'," (current-seconds) ");")))))
	  (twiki:save-tiddler db "MainMenu" "[[FirstTiddler]]" "" 1 1)))
    (sqlite3:set-busy-timeout!(dbi:db-conn db) 1000000)
    db))

;;======================================================================
;; twikis (db naming, sqlite vs postgresql, keys etc.
;;======================================================================

;; A wiki is specified by a list of keys, here we convert that list to a single string
(define (twiki:keys->key keys)
  (if (not (null? keys))
      (string-intersperse (map conc keys) " ")
      " "))

(define (twiki:key->fname key)
  (let* ((md5keypath (md5:digest key)) ;; (twiki:keys->key keys)))
	 ;; (keypath    (twiki:web64enc key))
	 (p1         (substring md5keypath  0  8))
	 (p2         (substring md5keypath  8 16))
	 (p3         (substring md5keypath 16 24))
	 (p4         (substring md5keypath 24 32)))
    (list (string-intersperse (list "twikis" p1 p2 p3) "/") md5keypath)))

;; look up the wid based on the keys, this is used for sub wikis only. I.e. a wiki instantiated inside another wiki 
;; giving a separate namespace to all the tiddlers
(define (twiki:key->wid db key) ;; (slot-ref s:session 'conn)
  (dbi:get-one db "SELECT id FROM wikis WHERE key_name=?;" key))

;;======================================================================
;; tiddlers
;;======================================================================

(define twiki:tiddler-selector "SELECT t.id,t.name,t.rev,t.dat_id,t.created_on,t.owner_id FROM tiddlers AS t INNER JOIN dats AS d ON t.dat_id=d.id")
(define (twiki:tiddler-make)(make-vector 8 #f))
(define-inline (twiki:tiddler-get-id           vec)    (vector-ref  vec 0))
(define-inline (twiki:tiddler-get-name         vec)    (vector-ref  vec 1))
(define-inline (twiki:tiddler-get-rev          vec)    (vector-ref  vec 2))
(define-inline (twiki:tiddler-get-dat-id       vec)    (vector-ref  vec 3))
(define-inline (twiki:tiddler-get-created_on   vec)    (vector-ref  vec 4))
(define-inline (twiki:tiddler-get-owner_id     vec)    (vector-ref  vec 5))
;; (define-inline (twiki:tiddler-get-dat-type     vec)    (vector-ref  vec 6))

(define-inline (twiki:tiddler-set-id!          vec val)(vector-set! vec 0 val) vec)
(define-inline (twiki:tiddler-set-name!        vec val)(vector-set! vec 1 val) vec)
(define-inline (twiki:tiddler-set-rev!         vec val)(vector-set! vec 2 val) vec)
(define-inline (twiki:tiddler-set-dat-id!      vec val)(vector-set! vec 3 val) vec)
(define-inline (twiki:tiddler-set-created_on!  vec val)(vector-set! vec 4 val) vec)
;; (define-inline (twiki:tiddler-set-owner_id!    vec val)(vector-set! vec 5 val))


;;======================================================================
;; Routines for displaying, editing, browsing etc. tiddlers
;;======================================================================	

;; should change this to take a tiddler structure?
(define (twiki:view dat tiddler) ;; close, close others, edit, more
  (let ((is-not-main (not (equal? "MainMenu" (twiki:tiddler-get-name tiddler)))))
    (s:div 'class "node"
	   (s:hr  " ")
	   (s:p 'class (if is-not-main "float-left" "float-left-menu")
		 (if is-not-main
		     (s:big (s:b (twiki:tiddler-get-name tiddler)))
		     (list (s:big (s:b "Menu") 
				  (s:small "("
					   (s:a "edit" 'href
						(s:link-to (twiki:get-link-back-to-current)
							   'edit_tiddler (twiki:tiddler-get-id tiddler))) ")"))))
		 )
	    (s:p 'class "tiddlercommands"
		 (if (not (equal? "MainMenu" (twiki:tiddler-get-name tiddler)))
		     (list 
		      (s:a "close" 'href (s:link-to (twiki:get-link-back-to-current) 'close_tiddler (twiki:tiddler-get-id tiddler))) "."
		      (s:a "close others" 'href (s:link-to (twiki:get-link-back-to-current) 'close_other_tiddlers (twiki:tiddler-get-id tiddler))) "."
		      (s:a "edit"  'href (s:link-to (twiki:get-link-back-to-current) 'edit_tiddler (twiki:tiddler-get-id tiddler))))
		     '())
		 )
	   (s:p 'class "tiddler-skip-par" "-")
	   ;; (s:p ".") 
	   (twiki:dat->html dat))))

(define (twiki:view-tiddler db tiddler)
  (let* ((dat-id (twiki:tiddler-get-dat-id tiddler))
	 (dat    (twiki:get-dat db dat-id))
	 (tnum   (twiki:tiddler-get-id tiddler)))
    ;; (s:log "twid: " dat-id " dat: " dat)
    (twiki:view dat tiddler)))

;; this one is called when an edit form is submitted (i.e. POST)
(define (twiki:action params)
  (if (and (list? params)
	   (> (length params) 0))
      (let* ((cmdln (string-split (car params) "_"))
	     (cmd   (string->symbol (car cmdln)))
	     (tkey  (twiki:web64dec (cadr cmdln)))
	     (tdb   (twiki:open-db tkey))
	     (twid  (twiki:key->wid tdb tkey)))
	(s:log "cmdln: " cmdln " cmd: " cmd " tkey: " tkey)
	(case cmd
	  ((save)
	   (twiki:save-curr-tiddler tdb twid))
	  ((savepic)
	   (s:log "twiki:action got to savepic")
	   (twiki:save-pic-from-form tdb twid))
	  ((cancel) ;; deprecated. Use a link for this (i.e in the twiki:twiki proc
	   (s:del! (conc "CURRENT_TWIDLER_UNDER_EDIT:" twid))
	   )))))

;; generate a form for editing a twiddler tnum
(define (twiki:edit-tiddler db tkey tnum)
  (s:log "twiki:edit-tiddler: tkey=" tkey)
  (let* ((enc-key (twiki:web64enc tkey))
	 (twid    (twiki:key->wid db tkey))
	 (trimmed (if (substring-index "=" enc-key)
		      (substring enc-key 0 (- (string-length enc-key) 1))
		      enc-key))
	 (tiddats (twiki:get-tiddlers-by-num db twid (list tnum))))
    (if (not (null? tiddats))
	(let* ((tid    (car tiddats))
	       (dat-id (twiki:tiddler-get-dat-id tid)))
	  (s:log "tid: " tid " dat-id: " dat-id)
	  (s:set! "twiki_title" (twiki:tiddler-get-name tid))
	  (s:set! "twiki_body"  (twiki:get-dat db dat-id))))
    (s:form 'action (s:link-to (twiki:get-link-back-to-current)
			       'action (conc "twiki.save_" trimmed))
	    'method "post" ;; 'twikiname tkey ;; done, cancel, delete
	    (s:input 'type "submit"   'name "form-name" 'value "save" 'twikiname tkey)
	    ;; (s:a "done" 'href (s:link-to (twiki:get-link-back-to-current) 'save_tmenu tnum))
	    (s:a "cancel" 'href (s:link-to (twiki:get-link-back-to-current) 'cancel_tedit tnum)) "."
	    (s:a "delete" 'href (s:link-to (twiki:get-link-back-to-current) 'delete_tiddler tnum))(s:br)
	    (s:input-preserve 'type "text" 'name "twiki_title" 'size "58" 'maxlength "150")
	    (s:textarea-preserve 'type "textarea" 'name "twiki_body" 'rows "10" 'cols "65")
	    (s:p "Tags" (s:input-preserve 'type "text" 'name "twiki_tags" 'size "55" 'maxlength "150")))))

;; save a tiddler to the db for the twiki twik, getting data from the INPUT
(define (twiki:save-curr-tiddler tdb twid)
  (formdat:printall (slot-ref s:session 'formdat) s:log)
  (let* ((heading (s:get-input 'twiki_title))
	 (body    (s:get-input 'twiki_body))
	 (tags    (s:get-input 'twiki_tags))
	 (uid     (twiki:get-id)))
    (s:log "twiki:save-curr-tiddler heading: " heading " body: " body " tags: " tags)
    (s:set! 'twiki_title heading)
    (if body
	(begin
	  (set! body (string-chomp body))
	  (s:set! 'twiki_body  body)))
    (s:set! 'twiki_tags  tags)
    (s:del! (conc "CURRENT_TWIDLER_UNDER_EDIT:" twid))
    (let ((res (twiki:save-tiddler tdb heading body tags twid uid)))
      ;; Now, replace this twiddler number in the view list with 
      ;; the new number from the db
      (twiki:normalize-current-twiddlers tdb twid)
      (s:del! 'twiki_title)
      (s:del! 'twiki_body)
      (s:del! 'twiki_tags)
      res)
    ))

(define (twiki:normalize-current-twiddlers tdb twid)
  (let* ((cvar      (conc "CURRENT_TWIDLERS:" twid))
	 (curr-slst (s:get cvar))
	 (curr-lst  (map string->number (string-split curr-slst ",")))
	 (tdlrs     (twiki:get-tiddlers-by-num tdb twid curr-lst))
	 (names     (remove (lambda (t)(string=? "MainMenu" t))
			    (map twiki:tiddler-get-name tdlrs)))
	 (newnums   (map twiki:tiddler-get-id 
			 (map (lambda (tn)
				(twiki:get-tiddler-by-name tdb twid tn))
			      names))))
    (s:set! cvar (string-intersperse (map number->string newnums)
				     ","))))
    
;; generic save tiddler
(define (twiki:save-tiddler tdb heading body tags twid uid)
    (if (misc:non-zero-string heading)
	(let* ((prev-tid (twiki:get-tiddler-by-name tdb twid heading))
	       (prev-dat-id (if prev-tid 
				(twiki:tiddler-get-dat-id prev-tid)
				-1))
	       (dat-id (twiki:save-dat tdb body 0))) ;; 0=text
	  ;; (s:log "twiki:save-tiddler dat-id: " dat-id " body: " body)
	  (if (equal? prev-dat-id dat-id) ;; no need to insert a new record if the dat didn't change
	      #t
	      (dbi:exec tdb 
			"INSERT INTO tiddlers (wiki_id,name,dat_id,created_on,owner_id) VALUES(?,?,?,?,?);"
			twid heading dat-id (current-seconds) uid))
	  #t) ;; success
	#f))  ;; non-success

;; text=0, jpg=1, png=2
(define (twiki:save-dat db dat type)
  (let* ((md5sum (md5:digest dat))
	 (datid  (twiki:dat-exists? db md5sum type))
	 (datblob (if (string? dat)
		      (string->blob dat)
		      dat)))
    (if datid
	datid
	(begin
	  (case type
	    ((0)   (dbi:exec db "INSERT INTO dats (md5sum,dat,type) VALUES(?,?,?);" md5sum datblob 0))
	    ((1)   (dbi:exec db "INSERT INTO dats (md5sum,dat,type) VALUES(?,?,?);" md5sum datblob 1))
	    (else  (dbi:exec db "INSERT INTO dats (md5sum,dat,type) VALUES(?,?,?);" md5sum datblob type)))
	  (twiki:dat-exists? db md5sum type)))))
       
(define (twiki:dat-exists? db md5sum type)
  (dbi:get-one db "SELECT id FROM dats WHERE md5sum=? AND type=?;" md5sum type))

(define (twiki:get-dat db id)
  (if (and id (number? id))
      (let ((res (dbi:get-one-row db "SELECT dat,type FROM dats WHERE id=?;" id)))
	(if res
	    (case (vector-ref res 1)
	      ((0)(blob->string (vector-ref res 0)))
	      (else (vector-ref res 0)))
	    #f))
      #f))

(define (twiki:maint_area tdb twid tkey)
  (let ((maint (s:get-param 'twiki_maint)))
    (s:div ;; 'class "node" 
     (s:a "Orphans"  'href (s:link-to (twiki:get-link-back-to-current) 'twiki_maint 1))(s:br)
     (s:a "Pics"     'href (s:link-to (twiki:get-link-back-to-current) 'twiki_maint 2))(s:br)
     (s:a "Search"   'href (s:link-to (twiki:get-link-back-to-current) 'twiki_maint 3))(s:br)
     (case maint
       ((1)
	(twiki:list-orphans tdb))
       (else
	 '())))))

;;======================================================================
;; Orphans
;;======================================================================
(define (twiki:make-tiddler-list tdlrs . tnums)
  (conc (string-intersperse 
	 (map conc (delete-duplicates
		    (append (map twiki:tiddler-get-id tdlrs) tnums)))
	 ",")))

(define (twiki:get-orphans tdb)
  '())

(define (twiki:list-orphans tdb)
  '())

;;======================================================================
;; Pictures
;;======================================================================
(define (twiki:pic_mgmt tdb twid tkey)
  (s:div 
   (s:a "Add pic" 'href (s:link-to (twiki:get-link-back-to-current) 'twiki_maint 2 'twiki_maint_add_pics 1))(s:br)
   (if (s:get-param "twiki_maint_add_pics")
       (s:form 'enctype "multipart/form-data" ;; 'name "does-a-form-have-a-name"
	       (s:input 'type "file" 'name "input-picture" 'value "Upload pic")
	       (s:input 'type "submit" 'name "submit-picture" 'value "Submit")
	       'method "post" 
	       'action (s:link-to (twiki:get-link-back-to-current) 'action (conc "twiki.savepic_" (twiki:web64enc tkey)))
	       (s:input 'type "text" 'name "picture-name" 'value "nada"))
       '())
   (let ((pics (dbi:get-rows tdb "SELECT id,name,dat_id,thumb_dat_id FROM pics;")))
     (map (lambda (pic)
	    (s:div 'class "tiddlerthumb"
		    (s:img 'title (vector-ref pic 1) 'alt (vector-ref pic 1)
		 	  'src (s:link-to (conc "twiki/" twid "/thumbs/" (vector-ref pic 0))))
		   (vector-ref pic 0) (vector-ref pic 1)))
	  pics))))

(define  (twiki:save-pic-from-form tdb twid)
  (let* ((pic-dat  (s:get-input 'input-picture)))
    (if pic-dat
	(begin
	  (s:log "twiki:save-pic-from-form with pic-dat=" pic-dat)
	  (twiki:save-pic tdb pic-dat))
	#f)))

(define (twiki:save-pic tdb pic-dat)
  (let ((pic-name (car pic-dat))
	(pic-type (cadr pic-dat))
	(pic-data (caddr pic-dat)))
    (if pic-data
	(let ((dat-id (twiki:save-dat tdb pic-data (twiki:mime->twiki-type pic-type))))
	  (dbi:exec tdb 
		    "INSERT INTO pics (name,dat_id,created_on,owner_id) VALUES(?,?,?,?);"
		    pic-name dat-id (current-seconds) (twiki:get-id))
	  #t)
	#f)))

(define (twiki:get-pic-dat tdb pic-id)
  (dbi:exec tdb "SELECT dat FROM pics INNER JOIN dats ON pics.dat_id=dats.id WHERE pics.id=?;" pic-id))

;; this one sets up the Content type, puts the data into page-dat and is done
(define (twiki:return-image-dat tdb dat-id)
  (slot-set! s:session 'page-type 'image)
  (slot-set! s:session 'content-type "image/jpeg")
  (slot-set! s:session 'alt-page-dat (twiki:get-dat tdb dat-id)))
  
(define (twiki:make-thumbnail tdb pic-id)
  (let ((indat  (twiki:get-pic-dat tdb pic-id))
	(outdat (make-string-port)))
    (let-values ((inp oup pid)
	       (process "convert" (list "-size" "500x180" "-" "-thumbnail" "250x90" "-unsharp" "0x.5" "-")))
	      (format inp "~A" (blob->string indat))
	      (let loop ((l (read-line oup)))
		(if (not (eof-object? l))
		    (begin
		      (format outdat "~A\n" l)
		      (loop (read-line oup)))
		    ;; we are done with the conversion, now put it back in the db
		    (let* ((newdat (string->blob (get-output-string outdat)))
			   (dat-id (twiki:save-dat tdb 2)))
		      (dbi:exec tdb "UPDATE pics SET thumb_id=? WHERE id=?;" dat-id pic-id)
		      dat-id))))))

(define (twiki:mime->twiki-type mime-type)
  (case (string->symbol mime-type)
    ((image/jpeg) 1)
    ((image/png)  2)
    (else 0)))

;;======================================================================
;; Wiki stuff
;;======================================================================

;; curr-tiddlers is a list of the names of the current tiddlers displayed
;; tiddler-under-edit is the tiddler being edited (or #f for none).
(define (twiki:wiki keys)
  (let* ((tkey     (twiki:keys->key keys))
	 (tdb      (twiki:open-db tkey))
	 (twid     (twiki:key->wid tdb tkey))
	 (cvar     (conc "CURRENT_TWIDLERS:" twid)) ;; page var to store current twiddlers being viewed
	 (cvar-ed  (conc "CURRENT_TWIDLER_UNDER_EDIT:" twid))
	 (tnumedit (if (s:get cvar-ed) 
		       (string->number (s:get cvar-ed))
		       #f)) ;; #f => nothing to edit, -1 create a new tiddler
	 (tnumview #f)
	 (lmenu    (twiki:get-tiddlers tdb twid (list "MainMenu")))
	 ;; store tiddlers for this page/twiki in cvar (i.e. CURRENT_TWIDLERS:<wid>
	 (tdlnums  (if (s:get cvar)
		       (map string->number (string-split (s:get cvar) ","))
		       '())) ;; list of tiddler numbers
	 (tdlrs    '())
	 (tedited  #f)
	 (edit-tmenu-id (if (s:get-param "edit_tmenu")
			    (string->number (s:get-param "edit_tmenu"))
			    #f))
	 (edit-tiddler (if (s:get-param "edit_tiddler") ;; this handles the "edit" link in the tiddler control bar
			   (let ((t (twiki:get-tiddlers-by-num tdb twid (list (string->number (s:get-param "edit_tiddler"))))))
			     (s:log "t: " t)
			     (if t
				 (car t ) ;; should be a list of one
				 (twiki:tiddler-set-name!
				  (twiki:tiddler-set-id! (twiki:tiddler-make) -1) "NewTiddler")))
			   #f))
	 (view-tiddler (if (s:get-param "view_tiddler")
			   (let* ((tname (twiki:web64dec (s:get-param "view_tiddler")))
				  (t     (twiki:get-tiddler-by-name tdb twid tname)))
			     (s:log "t: " t)
			     (if t
				 t 
				 (begin
				   (twiki:save-tiddler tdb tname "" "" twid (twiki:get-id))
				   (twiki:get-tiddler-by-name tdb twid tname))))

			   #f))
	 (image        (s:get-param "image"))) ;; image is the dat_id, keep it simple silly.
    ;; (thumb        (s:get-param "thumb")))

    ;; handle returning pictures
    (if image
	(twiki:return-image-dat tdb twid (string-number image))) ;; do not return from twiki:return-image

    (s:log "edit-tmenu-id: " edit-tmenu-id " edit-tiddler: " edit-tiddler)

    ;; Handle other URI commands here
    (if (s:get-param "cancel_tedit") ;; doesn't matter which tiddler - just use this to cancel any edit
	(begin
	  (s:del! (conc "CURRENT_TWIDLER_UNDER_EDIT:" twid))
	  (set! edit-tiddler #f)
	  (set! tnumedit #f)
	  (set! view-tiddler #f)
	  (twiki:normalize-current-twiddlers tdb twid)
	  (if (s:get cvar)
	      (set! tdlnums (map string->number (string-split (s:get cvar) ","))))))
    (if (s:get-param "delete_tiddler") '())
    ;; (twiki:delete_tiddler tdb twid (string->number (s:get-param "delete_tiddler"))))

    (s:set! "TWIKI_KEY" tkey) ;; this mechanism will fail for hierarchial twikis
    ;; override the twiddler to edit when editing MainMenu
    (if edit-tiddler
	(begin
	  (set! tnumedit (twiki:tiddler-get-id edit-tiddler))
	  (s:set! 'twiki_title (twiki:tiddler-get-name edit-tiddler))
	  (s:set! 'twiki_body  (twiki:get-dat tdb (twiki:tiddler-get-dat-id edit-tiddler)))))
    (if view-tiddler
	(begin
	  (set! tnumview (twiki:tiddler-get-id view-tiddler))))
    
    ;; NOW WHAT FOR VIEW - fix the links, add to tdlst


    (if edit-tmenu-id   (set! tnumedit edit-tmenu-id))
    (if tnumedit (set! tdlnums (cons tnumedit tdlnums)))
    (if tnumview (set! tdlnums (cons tnumview tdlnums)))
    (set! tdlrs (twiki:get-tiddlers-by-num tdb twid tdlnums))

    ;; remove tdlrs from the list if close_tiddler called
    (if (s:get-param "close_tiddler")
	(set! tdlrs (let ((tnum (string->number (s:get-param "close_tiddler"))))
		      (remove (lambda (t)
				(equal? (twiki:tiddler-get-id t) tnum))
			      tdlrs))))

    ;; remove all others if close_other_tiddlers called
    (if (s:get-param "close_other_tiddlers")
	(set! tdlrs (let ((tnum (string->number (s:get-param "close_other_tiddlers"))))
		      (remove (lambda (t)
				(not (equal? (twiki:tiddler-get-id t) tnum)))
			      tdlrs))))
    
    (s:set! cvar (twiki:make-tiddler-list tdlrs))
    (if tnumedit 
	(s:set! cvar-ed tnumedit)
	(s:del! cvar-ed))

    ;; get the tiddlers from the db now
    (set! result
	  (s:table
	   ;; ;; A header row MainMenu   WikiName    Other
	   ;; (s:tr
	   ;;  (s:td
	   ;;   (let ((main-menu-tnum  (twiki:tiddler-name->id tdb "MainMenu")))
	   ;;     (if edit-tmenu-id
	   ;;         "MainMenu"
	   ;;         (s:a "MainMenu" 'href (s:link-to (twiki:get-link-back-to-current) 'edit_tmenu main-menu-tnum)))))
	   ;;  (s:td (twiki:key->fname tkey))    
	   ;;  (s:td ""))
	   (s:tr
	    ;; The menu column (must do all this with css some time)
	    (s:td 'class "col1-tiddler" (if (not (null? lmenu))
					   (twiki:view-tiddler tdb (car lmenu))
					   ""))
	    (s:td 'class "col2-tiddler" 
		  ;; this is probably not needed as there is no reason to create tiddlers this way
		  (if (eq? tnumedit -1)(twiki:edit-tiddler tdb tkey tnumedit) '())
		  ;; insert the picture editor window if enabled
		  (if (equal? (s:get-param "twiki_maint") "2")(twiki:pic_mgmt tdb twid tkey) '())
		  (if (not (null? tdlrs))
		      (map (lambda (tdlr)
			     (let ((tnum  (twiki:tiddler-get-id tdlr)))
			       (s:log "tnum: " tnum " tnumedit: " tnumedit)
			       (if (and tnumedit (not tedited) (equal? tnumedit tnum))
				   (begin
				     (set! tedited #t) ;; only allow editing one tiddler at a time
				     (twiki:edit-tiddler tdb tkey tnum))
				   (twiki:view-tiddler tdb tdlr))))
			   tdlrs)
		      '()))
	    (s:td  'class "tiddler-top" (twiki:maint_area tdb twid tkey)))))
    (dbi:close tdb)
    result))

;; should do a single more efficient query but this is good enough
(define (twiki:get-tiddlers db twid tnames)
  (apply twiki:get-tiddlers-by-name db twid tnames))
;;   (let* ((tdlrs '())
;; 	 ;; (conn   (slot-ref s:session 'conn))
;; 	 (namelst (conc "('" (string-intersperse (map conc tnames) "','") "')"))
;; 	 (qry     (conc twiki:tiddler-selector " WHERE t.wiki_id=? AND t.id IN " namelst ";")))
;;     ;; (print qry)
;;     (dbi:for-each-row
;;      (lambda (row)
;;        (set! tdlrs (cons row tdlrs)))
;;      db qry twid)
;;     (reverse tdlrs))) ;; !Twiki\

;; tlst is a list of tiddler nums
(define (twiki:get-tiddlers-by-num db twid tlst)
  ;; (s:log "Got to twiki:get-tiddlers with keys: " tlst " and twid: " twid)
  ;; select where created_on < somedate order by created_on desc limit 1
  (let* ((tdlrs '())
	 (tlststr (string-intersperse (map number->string tlst) ","))
	 (already-got (make-hash-table))
	 (qry    (conc twiki:tiddler-selector " WHERE t.wiki_id=? AND t.id IN (" tlststr ") ORDER BY created_on DESC;")))
	;; (conn   (slot-ref s:session 'conn))
    ;; (print "qry: " qry)
    (dbi:for-each-row
     (lambda (row)
       (let ((tname (twiki:tiddler-get-name row)))
	 (if (not (hash-table-ref/default already-got tname #f))
	     (begin
	       (set! tdlrs (cons row tdlrs))
	       (hash-table-set! already-got tname #t)))))
     db qry twid)
    (reverse tdlrs))) ;; !Twiki\nTitle, pictures, etc.\n{{{\nCode\n}}}\n[[links]]\n|table|of|stuff|\n|more|stuff|here|\n"))

;; wid = wiki id
;; returns a list of twiki:tiddlers
(define (twiki:get-tiddlers-by-name tdb wid . names)
  (let ((tdlrs '()))
    (for-each (lambda (name)
		(let ((tdlr (twiki:get-tiddler-by-name tdb wid name)))
		  (if tdlr (set! tdlrs (cons tdlr tdlrs)))))
	      names)
    (reverse tdlrs)))
;; with the right query it should be possible to do this much faster approach for twiki:get-tiddlers-by-name
;;   (let ((tdlrs '())
;; 	(namelst (conc "('" (string-intersperse names "','") "')")))
;;     (dbi:for-each-row
;;      (lambda (row)
;;        (set! tdlrs (cons row tdlrs)))
;;      tdb
;;      (conc twiki:tiddler-selector " WHERE t.wiki_id=? AND t.name IN " namelst) wid)
;;     (reverse tdlrs)))

;; get the tiddler with the given name and the max date
(define (twiki:get-tiddler-by-name tdb wid name)
  (dbi:get-one-row tdb (conc twiki:tiddler-selector " WHERE t.wiki_id=? AND t.name=? ORDER BY created_on DESC LIMIT 1;") wid name))

(define (twiki:tiddler-name->id db tname)
  (dbi:get-one db "SELECT id FROM tiddlers WHERE name=?;" tname))

;;======================================================================
;; twiki text formating, parsing and display
;;======================================================================

;; twiki formating routines (override these to change your look and feel
(define twiki:twiki-tag  s:b)
(define twiki:h3         s:h3)
(define twiki:h2         s:h2)
(define twiki:h1         s:h1)
;; (define twiki:make-tlink s:i)
(define twiki:ul         s:ul)
(define twiki:ol         s:ol)
(define twiki:li         s:li)
(define twiki:pre        s:pre)
(define twiki:p          s:p)
(define twiki:u          s:u)
(define twiki:td         s:td)
(define twiki:tr         s:tr)
(define twiki:table      s:table)

(define (twiki:web64enc str)
  (string-substitute "=" "_" (base64:encode str) #t))

(define (twiki:web64dec str)
  (base64:decode (string-substitute "_" "=" str #t)))
    
(define (twiki:make-tlink text tiddlername)
  (s:a text 'href (s:link-to (twiki:get-link-back-to-current) 'view_tiddler (twiki:web64enc tiddlername))))

;; override these also
(define (twiki:get-id)
  (s:session-var-get "id"))
;; override this to set links inside wiki's
(define (twiki:get-link-back-to-current)
  (s:current-page))


;; regexes are listed in the order in which they should be checked

(define twiki:h3-patt (regexp "^!!!(.*)$"))
(define twiki:h2-patt (regexp "^!!(.*)$"))
(define twiki:h1-patt (regexp "^!(.*)$"))

(define twiki:tlink-patt     (regexp "^(.*)\\[\\[([^\\[\\]]*)\\]\\](.*)$"))
(define twiki:underline-patt (regexp "^(.*)__(.*)__(.*)$"))
(define twiki:table-patt     (regexp "^\\|(.*)\\|$"))

;; these are for multi-line formating
(define twiki:list-patt    (regexp "^(\\*+|\\#+)(.*)$"))
(define twiki:bullet-patt  (regexp "^(\\*+)(.*)$"))
(define twiki:number-patt  (regexp "^(\\#+)(.*)$"))
(define twiki:prefor-patt  (regexp "^\\{\\{\\{$"))
(define twiki:prefor-end-patt (regexp "^\\}\\}\\}$"))

;; regex
(define t:match  #f)
(define (t-match r s)
  (let ((res (string-match r s)))
    (set! t:match res)
    res))

;; should switch to recursively processing by block?
;; (process-block dat)
;;   ...
;;   (process-block remdat)
(define (twiki:dat->html dat)
  (let* ((inp        (open-input-string dat))
	 (nest-depth 0) ;; depth of nested lists
	 ;; token (i.e. line) handling stuff
	 (next-line  #f)
	 (peek-line  (lambda ()
		       next-line))
	 (get-line   (lambda ()
		       (let ((res next-line))
			 (set! next-line (read-line inp))
			 ;; (print "get-line: prev=" res " next=" next-line "\n")
			 res)))
	 (l          (get-line))) ;; discard the #f in next-line
    (twiki:read-block peek-line get-line nest-depth #f)))

;; blk-type is #f for not in a block (i.e. at top level), 'pre for preformated, 'ul or 'ol
;; call with first line as legit data
;; i.e. for preform - skip the {{{ line then call read-block
;;      for # or * call with first line
(define (twiki:read-block peek-line get-line nest-depth blk-type)
  (let loop ((res '())
	     (l   (peek-line))) ;; should this be a peek-line? yes!!
    ;; (print "twiki:read-block loop nest-depth="nest-depth " blk-type=" blk-type " l=" l "\n  res=" res)
    (if (eof-object? l)
	;; we are done! return the list
	res
	;; process it!
	(cond
	 ;; handle preformated text
	 ((eq? blk-type 'pre)
	  (if (t-match  twiki:prefor-end-patt l)
	      (begin
		(get-line) ;; discard the }}}
		res)       ;; end of preformatted
	      (begin
		;; (get-line) ;; discard the {{{
		(loop (append res (list (get-line)))
		      (peek-line)))))
	 ;; handle tables
	 ((eq? blk-type 'table)
	  (if (t-match twiki:table-patt l)
	      (let ((cels  (string-split (cadr t:match) "|")))
		(get-line)
		(loop (append res (twiki:tr (map twiki:td 
						 (map (lambda (x)(twiki:line->html x #f)) cels))))
		      (get-line)))
	      res))
	 ;; handle lists
	 ((or (t-match twiki:bullet-patt l) ;; have *
	      (t-match twiki:number-patt l))
	  (let* ((directive (cadr t:match))
		 (levelnum (string-length directive))
		 (text     (twiki:line->html (caddr t:match) #t))
		 (btype    (if (string=? "#" (substring directive 0 1))
			       'ul
			       'ol))
		 (func     (if (eq? btype 'ul)
			       twiki:ul
			       twiki:ol)))
	    ;; (print "handling " btype ": levelnum=" levelnum " text=" text " nest-depth=" nest-depth " blk-type=" blk-type)
	    (cond
	     ((not blk-type) ;; i.e first member of the list!
	      (loop (append res (func (twiki:read-block peek-line get-line levelnum btype)))
		    (get-line)))
	     ((> levelnum nest-depth)
	      (loop (append res (func (twiki:read-block peek-line get-line (+ nest-depth 1) btype)))
		    (peek-line)))
	     ((< levelnum nest-depth)
	      (append res (twiki:li text))) ;; return the bulleted item, don't get the next line??
	     (else
	      (get-line)
	      (loop (append res (twiki:li text))
		    (peek-line))))))
	 ((t-match twiki:prefor-patt l)
	  (get-line) ;; discard the {{{
	  (loop (append res (twiki:pre (twiki:read-block peek-line get-line nest-depth 'pre)))
		(peek-line)))
	 ((t-match twiki:table-patt l)
	  (get-line)
	  (loop (append res (twiki:table 'border 1 'cellspacing 0 (twiki:read-block peek-line get-line 0 'table)))
		(peek-line)))
	 (else
	  (get-line)
	  (loop (append res (twiki:line->html l #t))
		(peek-line)))))))

(define (twiki:line->html dat firstcall)
  (if firstcall 
      ;; process the patterns that test for beginning of line only on the first call
      (cond
       ((t-match twiki:h3-patt dat)
	(twiki:h3 (twiki:line->html (cadr t:match) #f)))
       ((t-match twiki:h2-patt dat)
	(twiki:h2 (twiki:line->html (cadr t:match) #f)))
       ((t-match twiki:h1-patt dat)
	(twiki:h1 (twiki:line->html (cadr t:match) #f)))
       (else  (append (twiki:line->html dat #f)(list (s:br)))));; (s:p 'class "tiddlerpar"
      ;; not firstcall so process other patterns
      (cond
       ((t-match twiki:tlink-patt dat)
	(let ((pre  (cadr   t:match))
	      (lnk  (caddr  t:match))
	      (post (cadddr t:match)))
	  (list (twiki:line->html pre #f)
		(twiki:make-tlink (twiki:line->html lnk #f) lnk) ;; special handling
		(twiki:line->html post #f))))
       ((t-match twiki:underline-patt dat)
	(let ((pre  (cadr   t:match))
	      (lnk  (caddr  t:match))
	      (post (cadddr t:match)))
	  (list (twiki:line->html pre #f)
		(twiki:u (twiki:line->html lnk #f))
		(twiki:line->html post #f))))
       ((t-match twiki:table-patt dat)
	(let ((cels  (string-split (cadr t:match) "|")))
	  (twiki:tr (map twiki:td (map twiki:line->html cels)))))
       (else (list dat)))))


#|
(twiki:dat->html "a\n{{{\nb\nc\nd\n}}}\n!e\n[[f]]\n[[g]]\n*h")
(s:output (current-output-port) (twiki:dat->html "!Testing [[my first link]]\n* Test\n* Foo\nblah"))   
(s:output (current-output-port) (twiki:dat->html "[[a]]\n{{{\nb\n  c\n   d\n}}}\n*x\n[[f]]\n[[g]]\n*h"))
(s:output (current-output-port)
|#

