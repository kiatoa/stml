;; Copyright 2007-2008, Matthew Welland. Kiatoa.com All rights reserved.
;; 

;; DON'T USE THIS!!!! It was a bad idea :-(

;; If performance becomes an issue upgrade this to use a vector to 

(require-extension tinyclos)

(define-class <sqltbl> ()
  (rows
   fields       ;; list of field
   fields-hash  ;; hash of fields -> number
   query        ;; query string using ?'s
   query-params ;; list of params for the query
   conn         ;; connection to db
   num-rows     ;; whatever 
   curr-row-ptr ;; number of the current row
   curr-row     ;; the current row vector (?? do we really want this ??)
   ))

(define-method (initialize (self <sqltbl>) initargs)
  (call-next-method)
  (slot-set! self 'num-rows 0)
  (slot-set! self 'curr-row-ptr 0)
  (slot-set! self 'fields '())
  (slot-set! self 'fields-hash (make-hash-table))
  (initialize-slots self initargs))
;;  (if (> (length (slot-ref self 'rows) 0))
;;      (slot-set! self 'curr-row (car rows))))

(define-method (sqltbl:next-row (self <sqltbl>))
  (let ((curr-row-ptr (+ (slot-ref self 'curr-row-ptr) 1))
        (num-rows     (slot-ref self 'num-rows))
        (rows         (slot-ref self 'rows)))
    (if (> curr-row-prt (slot-ref self 'num-rows)) #f ;; there is no next row
        (let ((new-curr-row (list-ref rows curr-row-ptr)))
          (slot-set! self 'curr-row new-curr-row)
          (slot-set! self 'curr-row-prt curr-row-prt)
          new-curr-row))))

;; run the query and fill the rows list 
(define-method (sqltbl:run-query (self <sqltbl>) . params)
  (let* ((query  (slot-ref self 'query))
         (fields (slot-ref self 'fields))
         (rows (let ((result '())
                     (actual-query (apply s:sqlparam query fields params)))
                 ;; (s:log "actual-query=" actual-query)
                 (dbi:for-each-row (lambda (tuple)
                                      (set! result (cons tuple result)))
                                    (slot-ref self 'conn)
				actual-query)
                 (slot-set! self 'query-params params)
                 (slot-set! self 'num-rows (length result))
                 (sqltbl:setup-fields self)  ;; update the fields lookup hash
                 (reverse result))))
    (slot-set! self 'rows rows)
    (if (not (null? rows))
        (slot-set! self 'curr-row (car rows)))
    (slot-set! self 'curr-row-ptr 0)
    rows))

(define-method (sqltbl:setup-fields (self <sqltbl>))
  (let ((fields-hash (slot-ref self 'fields-hash))
        (fields-list (slot-ref self 'fields)))
    (let loop ((head (car fields-list))
               (tail (cdr fields-list))
               (fnum 0))
      (hash-table-set! fields-hash head fnum)
      (if (null? tail) fnum
          (loop (car tail)(cdr tail)(+ fnum 1))))))

;; get a value from the current row
(define-method (sqltbl:get-field-value-curr (self <sqltbl>) field)
  (let ((curr-row  (slot-ref self 'curr-row))
        (field-num (hash-table-ref/default (slot-ref self 'fields-hash) field #f)))
    (if field-num 
        (vector-ref curr-row field-num)
        #f))) ;; not found -> #f

(define-method (sqltbl:vector->hash (self <sqltbl>) vec)
  (let ((h      (make-hash-table))
        (fields (slot-ref self 'fields)))
    (do ((i 0 (+ i 1)))
        ((>= i (length fields)))
      (hash-table-set! h (list-ref fields i)(vector-ref vec i)))
    h))


;; runs proc on each row and returns the resulting list
(define-method (sqltbl:map (self <sqltbl>) proc)
  (map (lambda (row)
         (proc (sqltbl:vector->hash self row))) (slot-ref self 'rows)))
   
