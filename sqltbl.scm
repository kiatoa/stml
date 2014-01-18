;; Copyright 2007-2011, Matthew Welland. Kiatoa.com All rights reserved.
;; 

;; DON'T USE THIS!!!! It was a bad idea :-(

;; (require-extension tinyclos)

;; (define-class <sqltbl> ()
;;   (rows
;;    fields       ;; list of field
;;    fields-hash  ;; hash of fields -> number
;;    query        ;; query string using ?'s
;;    query-params ;; list of params for the query
;;    conn         ;; connection to db
;;    num-rows     ;; whatever 
;;    curr-row-ptr ;; number of the current row
;;    curr-row     ;; the current row vector (?? do we really want this ??)
;;    ))

(declare (unit sqltbl))

(define (make-sqltbl:tbl)(make-vector 9))
(define (sqltbl:tbl-get-rows           vec)    (vector-ref  vec 0))
(define (sqltbl:tbl-get-fields         vec)    (vector-ref  vec 1))
(define (sqltbl:tbl-get-fields-hash    vec)    (vector-ref  vec 2))
(define (sqltbl:tbl-get-query          vec)    (vector-ref  vec 3))
(define (sqltbl:tbl-get-query-params   vec)    (vector-ref  vec 4))
(define (sqltbl:tbl-get-conn           vec)    (vector-ref  vec 5))
(define (sqltbl:tbl-get-num-rows       vec)    (vector-ref  vec 6))
(define (sqltbl:tbl-get-curr-row-ptr   vec)    (vector-ref  vec 7))
(define (sqltbl:tbl-get-curr-row       vec)    (vector-ref  vec 8))
(define (sqltbl:tbl-set-rows!          vec val)(vector-set! vec 0 val))
(define (sqltbl:tbl-set-fields!        vec val)(vector-set! vec 1 val))
(define (sqltbl:tbl-set-fields-hash!   vec val)(vector-set! vec 2 val))
(define (sqltbl:tbl-set-query!         vec val)(vector-set! vec 3 val))
(define (sqltbl:tbl-set-query-params!  vec val)(vector-set! vec 4 val))
(define (sqltbl:tbl-set-conn!          vec val)(vector-set! vec 5 val))
(define (sqltbl:tbl-set-num-rows!      vec val)(vector-set! vec 6 val))
(define (sqltbl:tbl-set-curr-row-ptr!  vec val)(vector-set! vec 7 val))
(define (sqltbl:tbl-set-curr-row!      vec val)(vector-set! vec 8 val))

(define  (sqltbl:initialize self);; initargs)
  (sqltbl:tbl-set-num-rows! self 0)
  (sqltbl:tbl-set-curr-row-ptr! self 0)
  (sqltbl:tbl-set-fields! self '())
  (sqltbl:tbl-set-fields-hash! self (make-hash-table)))
  ;; (initialize-slots self initargs))
;;  (if (> (length (sqltbl:tbl-get-rows self) 0))
;;      (sqltbl:tbl-set-curr-row! self (car rows))))

(define (sqltbl:next-row self)
  (let ((curr-row-ptr (+ (sqltbl:tbl-get-curr-row-ptr self) 1))
        (num-rows     (sqltbl:tbl-get-num-rows self))
        (rows         (sqltbl:tbl-get-rows self)))
    (if (> curr-row-prt (sqltbl:tbl-get-num-rows self)) #f ;; there is no next row
        (let ((new-curr-row (list-ref rows curr-row-ptr)))
          (sqltbl:tbl-set-curr-row! self new-curr-row)
          (sqltbl:tbl-set-curr-row-prt! self curr-row-prt)
          new-curr-row))))

;; run the query and fill the rows list 
(define (sqltbl:run-query self . params)
  (let* ((query  (sqltbl:tbl-get-query self))
         (fields (sqltbl:tbl-get-fields self))
         (rows (let ((result '())
                     (actual-query (apply s:sqlparam query fields params)))
                 ;; (s:log "actual-query=" actual-query)
                 (dbi:for-each-row (lambda (tuple)
                                      (set! result (cons tuple result)))
                                    (sqltbl:tbl-get-conn self)
				actual-query)
                 (sqltbl:tbl-set-query-params! self params)
                 (sqltbl:tbl-set-num-rows! self (length result))
                 (sqltbl:setup-fields self)  ;; update the fields lookup hash
                 (reverse result))))
    (sqltbl:tbl-set-rows! self rows)
    (if (not (null? rows))
        (sqltbl:tbl-set-curr-row! self (car rows)))
    (sqltbl:tbl-set-curr-row-ptr! self 0)
    rows))

(define (sqltbl:setup-fields self)
  (let ((fields-hash (sqltbl:tbl-get-fields-hash self))
        (fields-list (sqltbl:tbl-get-fields self)))
    (let loop ((head (car fields-list))
               (tail (cdr fields-list))
               (fnum 0))
      (hash-table-set! fields-hash head fnum)
      (if (null? tail) fnum
          (loop (car tail)(cdr tail)(+ fnum 1))))))

;; get a value from the current row
(define (sqltbl:get-field-value-curr self field)
  (let ((curr-row  (sqltbl:tbl-get-curr-row self))
        (field-num (hash-table-ref/default (sqltbl:tbl-get-fields-hash self) field #f)))
    (if field-num 
        (vector-ref curr-row field-num)
        #f))) ;; not found -> #f

(define (sqltbl:vector->hash self vec)
  (let ((h      (make-hash-table))
        (fields (sqltbl:tbl-get-fields self)))
    (do ((i 0 (+ i 1)))
        ((>= i (length fields)))
      (hash-table-set! h (list-ref fields i)(vector-ref vec i)))
    h))


;; runs proc on each row and returns the resulting list
(define (sqltbl:map self proc)
  (map (lambda (row)
         (proc (sqltbl:vector->hash self row))) (sqltbl:tbl-get-rows self)))
   
