;; Copyright 2007-2008, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;; 
(define (s:split-string strng delim)
  (if (eq? (string-length strng) 0) (list strng)
      (let loop ((head (make-string 1 (car (string->list strng))))
		 (tail (cdr (string->list strng)))
		 (dest '())
		 (temp ""))
	(cond ((equal? head delim)
	       (set! dest (append dest (list temp)))
	       (set! temp ""))
	      ((null? head) 
	       (set! dest (append dest (list temp))))
	      (else (set! temp (string-append temp head)))) ;; end if
	(cond ((null? tail)
	       (set! dest (append dest (list temp))) dest)
	      (else (loop (make-string 1 (car tail)) (cdr tail) dest temp))))))

;; allowed-tags is a list of tags as symbols:
;;   '(a b center p a)
;; parsing is simplistic and the response conservative
;; if a < is found without the tag and closing > then
;; the < or > is replaced with &lt; or &gt; without 
;; even trying hard to figure out if there is a legit tag 
;; buried in the text somewhere.
;; a list of strings is returned.
;;
;; NOTES
;; 1. case is important in the allowed-tags list!
;; 2. only "solid" tags are supported i.e. <a href="foo"> will not work?
;;

;; (s:cgi-out (eval (s:output (s:html-filter "hello<b>goodbye</b><b> eh" '(a b i))))

;; strategy
;; 1. convert \n to <linefeed>
;; 2. Split on "<"
;; 3. Split on ">"
;; 4. Fix
(define (s:html-filter input-text allowed-tags)
  (let* ((toks   (s:str->toks input-text))
	 (tmp    (s:toks->stml '(s:null) #f toks allowed-tags))
	 (res    (car tmp))
	 (nxttag (cadr tmp))
	 (rem    (caddr tmp)))
    res))

(define (s:html-filter->string input-text allowed-tags)
  (let ((ostr (open-output-string)))
    ;;; (s:output-new ostr (s:html-filter input-text allowed-tags))
    (s:output-new ostr (car (eval (s:html-filter input-text allowed-tags))))
    (get-output-string ostr)))
	
;;     (if (null? rem)
;; 	res '())
;; 	(s:toks->stml (if (list? res) res '()) #f rem allowed-tags))))

(define (s:str->toks str)
  (apply append (map (lambda (tok)
		       (intersperse (s:split-string tok ">") ">")) 
		     (intersperse (s:split-string str "<") "<"))))

(define (s:tag->stml tag)
  (string->symbol (string-append "s:" (symbol->string tag))))


(define (s:toks->stml res tag rem allowed)
  ;; (print "tag: " tag " rem: " rem)
  (if (null? rem)
      (list (append res (if tag
			    (list (s:tag->stml tag))
				'())) #f '() allowed) ;; the case of a lone tag 
      ;; handle a starting tag
      (let* ((tmp       (s:upto-tag rem allowed))
	     (txt       (car tmp))      ;; this txt goes with tag!!!
	     (nexttag   (cadr tmp))     ;; this is the NEXT DAMN tag!
	     (begin-tag (caddr tmp))
	     (newrem    (cadddr tmp)))
	;; (print "txt:        " txt "\nnexttag:    " nexttag "\nbegin-tag:  " begin-tag "\nnewrem:     " newrem "\nres:        " res "\n")
	(if begin-tag ;; nest the following stuff
	    (let* ((childdat (s:toks->stml '() nexttag newrem allowed))
		   (child    (car childdat))
		   (newtag   (cadr childdat))
		   (newrem2  (caddr childdat))
		   (allowed  (cadddr childdat))) ;; ya, it shouldn't have changed
	      (if tag 
		  (s:toks->stml (append res (list (append (list (s:tag->stml tag)) child (list txt))))
				newtag newrem2 allowed)
		  (s:toks->stml (append res (list txt) child)
				newtag newrem2 allowed)))
	    ;; it must have been an end tag
	    (list (append res (list 
			       (if tag
				   (list (s:tag->stml tag) txt)
				   txt)))
		  #f
		  newrem
		  allowed)))))


;; "<" "b" ">"  => "<b>"
;; "<"
;; (define (s:rebuild-tags input-list)

;; ("blah blah" "<" "b" ">" "more stuff" "<" "i" ">" ) 
;;     => ("blah blah" b #t ( "more stuff" "<" "i" ">" ))
;; ("blah blah" "<" "/b" ">" "more stuff" "<" "i" ">" ) 
;;     => ("blah blah" b #f ( "more stuff" "<" "i" ">" ))
(define (s:upto-tag inlst allowed-tags)
  (if (null? inlst) inlst
      (let loop ((tok  (car inlst))
		 (tail (cdr inlst))
		 (prel "")) ;; create a string or a list of string parts?
	(if (string=? tok "<") ;; might have a tag
	    (if (> (length tail) 1) ;; to be a tag, need tag and closing ">"
		(let ((tag (car tail))
		      (end (cadr tail))
		      (rem (cddr tail))) 
		  (if (string=? end ">") ;; yep, it is probably a tag
		      (let* ((trim-tag (if  (string=? "/" (substring tag 0 1))
					    (substring tag 1 (string-length tag)) #f))
			     (tag-sym  (string->symbol (if trim-tag trim-tag tag))))
			(if (member tag-sym allowed-tags)
			    ;; have a valid tag, rebuild it and return the result
			    (list prel tag-sym (if trim-tag #f #t) rem)
			    ;; not a valid tag, convert "<" and ">" and add all to prel
			    (let ((newprel (string-append prel "&lt;" tag "&gt;")))
			      (if (null? rem)(list newprel #f #f '()) ;; return newprel - add #f #f ???
				  (loop (car rem)(cdr rem) newprel)))))
		      ;; so, it wasn't a tag
		      (let ((newprel (string-append prel "&lt;" tag)))
			(if (null? tail)
			    (list newprel #f #f '())
			    (loop (car rem)(cdr rem) newprel)))))
		;; too short to be a tag
		(list (apply string-append prel "&lt;" tail) #f #f '()))
	    (if (null? tail) 
		;; we're done
		(list (string-append prel tok) #f #f '())
		(loop (car tail)(cdr tail)(string-append prel tok)))))))


(define (s:divy-up-cgi-str instr)
  (map (lambda (x) (string-split x "=")) (string-split instr "&")))

(define (s:decode-str instr)
  (let* ((abc (string-substitute "\\+" " " instr #t))
	 (toks (s:split-string abc "%")))
    (if (< (length toks) 2) abc
	(let loop ((head (cadr toks))
		   (tail (cddr toks))
		   (result (car toks)))
	  (if (string=? head "")
	      (if (null? tail)
		  result
		  (loop (car tail)(cdr tail) result))
	      (let* ((key (substring head 0 2))
		     (rem (substring head 2 (string-length head)))
		     (num (string->number key 16))
		     (ch  (if (exact? num)
			      (integer->char num)
			      #f)) ;; this is an error. I will probably regret this some day
		     (chstr  (if ch (make-string 1 ch) ""))
		     (newres (if ch
				 (string-append result chstr rem)
				 (string-append result head))))
		;; (print "head: " head " num: " num " ch: |" ch "| chstr: " chstr)
		(if (null? tail)
		    newres
		    (loop (car tail)(cdr tail) newres))))))))

(define (s:process-cgi-input instr)
  (map (lambda (xy)
         (list (string->symbol (s:decode-str (car xy)))
               (if (eq? (length xy) 1) 
                   ""
                   (s:decode-str (cadr xy)))))
         (s:divy-up-cgi-str instr)))

;; for testing -- deletme
;; (define blah "post_title=%2B%2B%2B%2B%2B%2B%2B%2B%2B%2B%2Bhello-------------+++++++++++%26%26%26%26%26%26%26%26%26%40%40%40%40%40%40%40%40%40&post_body=%2B%2B%2B%2B%2B%2B%2B%2B%2B%2B%2Bhello-------------+++++++++++%26%26%26%26%26%26%26%26%26%40%40%40%40%40%40%40%40%40%0D%0A%0D%0A%2B%2B%2B%2B%2B%2B%2B%2B%2B%2B%2Bhello-------------+++++++++++%26%26%26%26%26%26%26%26%26%40%40%40%40%40%40%40%40%40%0D%0A%0D%0A%0D%0A%2B%2B%2B%2B%2B%2B%2B%2B%2B%2B%2Bhello-------------+++++++++++%26%26%26%26%26%26%26%26%26%40%40%40%40%40%40%40%40%40&new_post=Submit")
;; (define blah2 "post_title=5%25&post_body=and+10%25&new_post=Submit")