;; Copyright 2007-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;;======================================================================
;; The meta data key store, just a general dumping ground for values
;; only used occasionally
;;======================================================================

(declare (unit keystore))

(define (keystore:get db key)
  (dbi:get-one db "SELECT value FROM metadata WHERE key=?;" key))

(define (keystore:set! db key value)
  (let ((curr-val (keystore:get db key)))
    (if curr-val
	(dbi:exec db "UPDATE metadata SET value=? WHERE key=?;" value key)
	(dbi:exec db "INSERT INTO metadata (key,value) VALUES (?,?);" key value))))

(define (keystore:del! db key)
  (dbi:exec db "DELETE FROM metadata WHERE key=?;" key))
