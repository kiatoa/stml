(use regex posix srfi-69 srfi-1)

(define extract-rx (regexp "pages\\/(.*)_(view|ctrl).scm"))

(define (print-page-wrapper lookup page)
  (print "(define (pages:" page " session db shared)")
  (if (hash-table-ref/default lookup (conc page "_ctrl") #f)
      (print "(include \"pages/" page "_ctrl.scm\")"))
  (if (hash-table-ref/default lookup (conc page "_view") #f)
      (print "(include \"pages/" page "_view.scm\")"))
  (print ")\n"))

(let* ((views  (glob "pages/*_view.scm"))
       (ctrls  (glob "pages/*_ctrl.scm"))
       (all    (append views ctrls))
       (lookup (make-hash-table))
       (pages  (delete-duplicates
		(map (lambda (x)
		       (let* ((res  (string-match extract-rx x))
			      (page (cadr res))
			      (type (caddr res)))
			 (hash-table-set! lookup (conc page "_" type) #t)
			 (cadr res)))
		     all))))
  (if (null? all)(begin (print "No page files matching pages/*_(view|ctrl).scm")(exit)))
  (print "Pages: " pages)
  ;; first the individual rollup wrappers (used by the dynamic load)
  (for-each 
   (lambda (page)
     (let ((pagefile  (conc "pages/" page ".scm")))
       (print "page " page " ")
       (if (not (file-exists? pagefile))
	   (begin
	     (with-output-to-file pagefile
	       (lambda ()
		 (print-page-wrapper lookup page)))
	     (print " created"))
	   (print " already created"))))
   pages)
  ;; then the monolithic rollup wrapper (used in compiling the single-executable)
  (with-output-to-file "all_pages.scm"
    (lambda ()
      (for-each
       (lambda (page)
	 (print-page-wrapper lookup page))
       pages))))


  