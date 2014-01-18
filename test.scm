(use test md5)

(require-extension sqlite3)
(import (prefix sqlite3 sqlite3:))

(require-library dbi)

;; (declare (uses stml))

(include "requirements.scm")
(include "cookie.scm")
(include "misc-stml.scm")
(include "formdat.scm")
(include "stml.scm")
(include "session.scm")
(include "sqltbl.scm")
(include "html-filter.scm")
(include "keystore.scm")

(define p (open-input-file "test.stml"))
(print (process-port p))
(close-input-port p)
