#lang scheme

; a.
;  Each division file must be prefix with a unique tag that will be used to dispatch
;  the call to get-record to the right procedure.

(define (make-file division records)
  (list division records))
(define (division-file file) (car file))
(define (records file) (cadr file))

(define (get-record employee-name file)
  ((get 'get-record (division-file file)) employee-name (records file)))

; b.
;  Similarly we should prefix each record with the unique tag of each division

(define (make-record division record)
  (list division record))
(define (division-record tagged-record) (car record))
(define (record tagged-record) (cadr record))

(define (get-salary tagged-record)
  ((get 'get-salary (division-record record)) (record tagged-record)))

; c.
;  

(define (find-employee-record employee-name files)
  (define (iter employee-name files)
    (if (null? files)
        null
        (let ((record (get-record employee-name (car files))))
          (if (null? record)
              (iter employee-name (cdr files))
              record))))
  (iter employee-name files))

; d.
;
; When a new division is created:
;  1. a unique tag must be assigned to the division
;  2. the file and its records must be prefixed with this tag
;  3. the procedures that operate on this file and records must be implemented and declared in the dispatching table
;
; Note: There is no need to modify the generic operations like get-record, get-salary and find-employee-record.

; Mock

(define (get op type)
  null)
