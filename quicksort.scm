(define (part-less l1 main)
    (cond 
    ((null? l1) list())
    ((> main (car l1)) (cons (car l1) (part-less (cdr l1) main)))
    (else (part-less (cdr l1) main))))

(define (part-eq l1 main)
    (cond
    ((null? l1) list())
    ((= main (car l1)) (cons (car l1) (part-eq (cdr l1) main)))
    (else (part-eq (cdr l1) main))))

(define (part-more l1 main)
    (cond 
    ((null? l1) list())
    ((< main (car l1)) (cons (car l1) (part-more (cdr l1) main)))
    (else (part-more (cdr l1) main))))

(define (quicksort l1)
 (cond
    ((null? l1) list())
    (else (let ((main (car l1)))
           (append (append (quicksort (part-less l1 main))
                            (part-eq l1 main))
                   (quicksort (part-more l1 main))))))
)

(display (quicksort '(4 5 2 4 1 6 78 1 2 4 7 8 3 2)))