;; Copyright 2007-2008, Matthew Welland. Kiatoa.com All rights reserved.
;; 

;; sys-state

(list (let ((p (open-input-pipe "env")))
	(let loop ((l (read-line p))
		   (res '()))
	  (if (not (eof-object? l))
	      (loop (read-line p)(cons (list l "<BR>") res))
	      res)))
      ;; "USER=" (user-information (current-user-id))

      (s:h2 "Form data")
      (session:pp-formdat s:session)
      "argv=" (argv))
