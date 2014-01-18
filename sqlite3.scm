;; Copyright 2007-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;

;; I used this to get a simple interactive sqlite editor on the nokia n800
;; since I couldn't get sqlite3 to install (for reasons I can't remember).

(use sqlite3)

(define args (argv))
(define num-args (length args))

(define dbname #f)
(define cmd    #f)

(if (> num-args 1)
  (set! dbname (cadr args))
  (exit 0))

(if (> num-args 2)
  (set! cmd (caddr args)))

(define db (sqlite3:open dbname))

(define (interactive db)
  (let ((prompt " > "))
    (display prompt)
  (let loop ((cmd (read-line)))
    (cond 
      ((> (string-length cmd) 0)
       (process-cmd db cmd)
       (display prompt)
       (loop (read-line)))
      (else
	(loop (read-line)))))))
 
(define (process-cmd db cmd)
  (sqlite3:for-each-row
    (lambda (a . b)
      (print a " " (string-intersperse b " ")))
    db cmd))

(if cmd
  (process-cmd db cmd)
  (interactive db))

(sqlite3:finalize! db)    

