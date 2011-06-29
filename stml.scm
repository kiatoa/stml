;; Copyright 2007-2008, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;; stml is a list of html strings

;; extract various tokens from the parameter list
;;   'key val => put in the params list
;;   strings  => maintain order and add to the datalist <<== IMPORTANT
(define (s:extract inlst)
  (if (null? inlst) inlst
      (let loop ((data '())
                 (params '())
                 (head (car inlst))
                 (tail (cdr inlst)))
        ;; (print "head=" head " tail=" tail)
        (cond 
         ((null? tail)
          (if (symbol? head) ;; the last item is a param - borked
              (s:log "ERROR: param with no value"))
          (list (append data (list (s:any->string head))) params))
         ((or (string? head)(list? head)(number? head))
          (loop (append data (list  (s:any->string head))) params (car tail)   (cdr tail)))
         ((symbol? head)
          (let ((new-params (cons (list head (car tail)) params))
                (new-tail  (cdr tail)))
            (if (null? new-tail) ;; we are done, no more params etc.
                (list data new-params)
                (loop data new-params (car new-tail)(cdr new-tail)))))
         (else
          (s:log "WARNING: Malformed input, you have broken stml, probably in a form: head=" head 
	          "\ntail=" tail 
                  "\ninlst=" inlst 
                  "\nparams=" params)
	  (if (null? tail)
	      (list data params)
	      (loop data params (car tail)(cdr tail))))))))

;; most tags can be handled by this routine
(define (s:common-tag tagname args)
  (let* ((inputs (s:extract args))
         (data   (car inputs))
         (params (s:process-params (cadr inputs))))
    (list (conc "<" tagname params ">")
          data
          (conc "</" tagname ">"))))

;; Suggestion: order these alphabetically
(define (s:a      . args) (s:common-tag "A"      args))
(define (s:b      . args) (s:common-tag "B"      args))
(define (s:u      . args) (s:common-tag "U"      args))
(define (s:big    . args) (s:common-tag "BIG"    args))
(define (s:body   . args) (s:common-tag "BODY"   args))
(define (s:center . args) (s:common-tag "CENTER" args))
(define (s:code   . args) (s:common-tag "CODE"   args))
(define (s:div    . args) (s:common-tag "DIV"    args))
(define (s:h1     . args) (s:common-tag "H1"     args))
(define (s:h2     . args) (s:common-tag "H2"     args))
(define (s:h3     . args) (s:common-tag "H3"     args))
(define (s:head   . args) (s:common-tag "HEAD"   args))
(define (s:html   . args) (s:common-tag "HTML"   args))
(define (s:i      . args) (s:common-tag "I"      args))
(define (s:img    . args) (s:common-tag "IMG"    args))
(define (s:input  . args) (s:common-tag "INPUT"  args))
(define (s:link   . args) (s:common-tag "LINK"   args))
(define (s:p      . args) (s:common-tag "P"      args))
(define (s:strong . args) (s:common-tag "STRONG" args))
(define (s:table  . args) (s:common-tag "TABLE"  args))
(define (s:td     . args) (s:common-tag "TD"     args))
(define (s:title  . args) (s:common-tag "TITLE"  args))
(define (s:tr     . args) (s:common-tag "TR"     args))
(define (s:small  . args) (s:common-tag "SMALL"  args))
(define (s:quote  . args) (s:common-tag "QUOTE"  args))
(define (s:hr     . args) (s:common-tag "HR"     args))
(define (s:li     . args) (s:common-tag "LI"     args))
(define (s:ul     . args) (s:common-tag "UL"     args))
(define (s:ol     . args) (s:common-tag "OL"     args))
(define (s:dl     . args) (s:common-tag "DL"     args))
(define (s:dt     . args) (s:common-tag "DT"     args))
(define (s:dd     . args) (s:common-tag "DD"     args))
(define (s:pre    . args) (s:common-tag "PRE"    args))
(define (s:span   . args) (s:common-tag "SPAN"   args))

(define (s:dblquote  . args)
  (let* ((inputs (s:extract args))
         (data   (caar inputs))
         (params (s:process-params (cadr inputs))))
    (conc "&quot;" data "&quot;")))

(define (s:br     . args) "<BR>") ;;  THIS MAY NOT WORK!!!! BR CAN (MISTAKENLY) GET PARAM TEXT
;; (define (s:br     . args) (s:common-tag "BR"     args))
(define (s:font   . args) (s:common-tag "FONT"   args))
(define (s:err-font . args)
  (s:b (s:font 'color "red" args)))

(define (s:comment . args)
  (let* ((inputs (s:extract args))
         (data   (car inputs))
         (params (s:process-params (cadr inputs))))
    (list "<!--" data "-->")))

(define (s:null   . args) ;; nop
  (let* ((inputs (s:extract args))
         (data   (car inputs))
         (params (s:process-params (cadr inputs))))
    (list data)))

;; puts a nice box around a chunk of stuff
(define (s:fieldset legend . args)
  (list "<FIELDSET><LEGEND>" legend "</LEGEND>" args "</FIELDSET>"))

;; given a string return the string if it is non-white space or &nbsp; otherwise
(define (s:nbsp str)
  (if (string-match "^\\s*$" str)
      "&nbsp;"
      str))

;; USE 'page_override to override a linkto page from a button
(define (s:form   . args)
  ;; create a link for calling back into the current page and calling a specified 
  ;; function
  (let* ((action     (let ((v (s:find-param 'action args)))
                       (if v v "default")))
	 (id         (let ((i (s:find-param 'id args)))
		       (if i i #f)))
         (page       (let ((p (slot-ref s:session 'page)))
                       (if p p "home")))
	 ;; (link       (session:link-to s:session page (if id
         ;;                                                 (list 'action action 'id id)
         ;;                                                 (list 'action action)))))
	 (link       (if (string=? (substring action 0 5) "http:") ;; if first part of string is http:
	        	 action
	        	 (session:link-to s:session 
	        			  page 
	        			  (if id
	        			      (list 'action action 'id id)
	        			      (list 'action action))))))
    ;; (script     (slot-ref s:session 'script))
    ;; (action-str (string-append script "/" page "?action=" action)))
    (s:common-tag "FORM" (append (s:remove-param-matching (s:remove-param-matching args 'action) 'id)
                                 (list 'action link)))))

;; look up the variable name (via the 'name tag) then inject the value from the session var
;; replacing the 'value value if it is already there, adding it if it is not.
(define (s:preserve tag args)
  (let* ((var-name (s:find-param 'name args)) ;; name='varname'
	 (value    (let ((v (s:get var-name)))
		     (if v v #f)))
	 (newargs  (append (s:remove-param-matching args 'value) (if value (list 'value value) '()))))
    (s:common-tag tag newargs)))

(define (s:input-preserve  . args)
  (s:preserve "INPUT" args))

;; text areas are done a little differently. The value is stored between the tags <textarea ...>the value goes here</textarea>
(define (s:textarea-preserve . args)
  (let* ((var-name (s:find-param 'name args))
	 (value    (let ((v (s:get var-name)))
		     (if v v #f))))
    (s:common-tag "TEXTAREA" (if value (cons value args) args))))

(define (s:option dat)
  (let ((len      (length dat)))
    (cond
     ((eq? len 1)
      (let ((item (car dat)))
	(s:option (list item item item))))
     ((eq? len 2)
      (s:option (append dat (list (car dat)))))
     (else
      (let ((label    (car dat))
	    (value    (cadr dat))
	    (dispval  (caddr dat))
	    (selected (if (> len 3)(cadddr dat) #f)))
	(list (conc "<OPTION " 
		    (if selected " selected " "")
		    "label=\"" label
		    "\" value=\"" value
		    "\">" dispval "</OPTION>")))))))

;; call only with (label (label value dispval [#t]) ...)
;; NB// sadly this block is redundantly almost identical to the s:select
;; fix that later ...
(define (s:optgroup dat)
  (let ((label (car dat))
	(rem   (cdr dat)))
    (if (null? rem)
	(s:common-tag "OPTGROUP" 'label label)
	(let loop ((hed (car rem))
		   (tal (cdr rem))
		   (res (list (conc "<OPTGROUP label=" label))))
	  ;; (print "hed: " hed " tal: " tal " res: " res)
	  (let ((new (append res (list (if (list? (cadr hed))
					   (s:optgroup hed)
					   (s:option hed))))))
	    (if (null? tal)
		(append new (list "</OPTGROUP>"))
		(loop (car tal)(cdr tal) new)))))))
    
;; items is a hierarchial alist
;; ( (label1 value1 dispval1 #t) ;; <== this one is selected
;;   (label2 (label3 value2 dispval2)
;;           (label4 value3 dispval3)))
;;     
;;  required arg is 'name
(define (s:select items . args)
  (if (null? items)
      (s:common-tag "SELECT" args)
      (let loop ((hed (car items))
		 (tal (cdr items))
		 (res '()))
	;; (print "hed: " hed " tal: " tal " res: " res)
	(let ((new (append res (list (if (and (> (length hed) 1)
					      (list? (cadr hed)))
					 (s:optgroup hed)
					 (s:option hed))))))
	  (if (null? tal)
	      (s:common-tag "SELECT" (cons new args))
	      (loop (car tal)(cdr tal) new))))))

(define (s:color  . args)
  "#00ff00")

(define (s:print indent inlst)
  (map (lambda (x)
         (cond 
          ((or (string? x)(symbol? x))
           (print (conc (make-string (* indent 2) #\ ) (any->string x))))
          ((list? x)
           (s:print (+ indent 1) x))
          (else
           ;; (print "ERROR: Bad input 01") ;; why do anything with junk?
           )))
       inlst))

(define (s:cgi-out inlst)
  (s:output (current-output-port) inlst))

(define (s:output port inlst)
  (map (lambda (x)
	 (cond 
	  ((string? x) (print x)) ;; (print x))
	  ((symbol? x) (print x)) ;; (print x))
	  ((list? x)   (s:output port x))
	  (else
	   ;; (print "ERROR: Bad input 02") ;; why do anything? don't output junk.
	   )))
       inlst))
;  (if (> (length inlst) 2)
;      (print)))

(define (s:output-new port inlst)
  (with-output-to-port port
      (lambda ()
	(map (lambda (x)
	       (cond 
		((string? x) (print x))
		((symbol? x) (print x))
		((list? x)   (s:output port x))
		(else
		 ;; (print "ERROR: Bad input 03")
     )))
	     inlst))))
           
