(define (make-matrix matrix)
    (let ((matrix matrix) )
        (define (row-1) (car matrix))
        (define (row-2) (car (cdr matrix)))
        (define (col-1) (list (car (row-1)) (car (row-2))))
        (define (col-2) (list (car (cdr (row-1))) (car (cdr (row-2)))))
        (define (display) (format #t "|~a|\n|~a|\n\n" (row-1) (row-2)))
        (define (value-by-pos x y) 
            (let ((fRow (row-1)) (sRow (row-2)) (x x) (y y))
                (if (= x 1)
                    (if (= y 1)
                        (car fRow)
                        (car (cdr fRow))
                    )
                    (if (= y 1)
                        (car sRow)
                        (car (cdr sRow))
                    )
                )
            )    
        )
    
        (define (plus matrix2)
            (make-matrix 
                (list (map + (row-1) (matrix2 'row-1))
                      (map + (row-2) (matrix2 'row-2))
                )
            )
        )
    
        (define (minus matrix2)
            (make-matrix 
                (list (map - (row-1) (matrix2 'row-1))
                      (map - (row-2) (matrix2 'row-2))
                )
            )
        )
    
        (define (MatMalt matrix2)
            (make-matrix
                (list
                    (list
                        (apply + (map * (row-1) (matrix2 'col-1)))
                        (apply + (map * (row-1) (matrix2 'col-2)))
                    )
                
                    (list
                        (apply + (map * (row-2) (matrix2 'col-1)))
                        (apply + (map * (row-2) (matrix2 'col-2)))
                    )
                )
            )    
        )
    
        (define (VecMalt vec)
            (list
                (apply + (map * (row-1) vec))
                (apply + (map * (row-2) vec))  
            )
        )
    
        (define (LamMalt n)
            (make-matrix
                (list
                    (map * (list n n) (row-1))   
                    (map * (list n n) (row-2)) 
                )
            )
        )
    
    
        (define (transpose)
            (make-matrix (list (col-1) (col-2)))
        )
    
    
        (lambda args
            (apply
                (case (car args)
                    ((row-1) row-1)
                    ((row-2) row-2)
                    ((col-1) col-1)
                    ((col-2) col-2)
                    ((display) display)
                    ((plus) plus)
                    ((minus) minus)
                    ((MatMalt) MatMalt)
                    ((VecMalt) VecMalt)
                    ((LamMalt) LamMalt)
                    ((transpose) transpose)
                    ((value-by-pos) value-by-pos)
                    (else 
                    (
                        begin 
                        (display "Invalid method\n") 
                        (exit 1) 
                    ))
                )
                (cdr args)
            )
        )
    )
)
 


(define M1 (make-matrix (list (list 1 2) (list 3 4))))
(define M2 (make-matrix (list (list 5 6) (list 7 8))))
(M1 'display)  
(M2 'display) 
 
((M1 'plus M2) 'display) 

((M1 'minus M2) 'display) 

((M1 'LamMalt 2) 'display) 

((M1 'MatMalt M2) 'display)  

(display (M1 'VecMalt (list 2 3))) (newline) 

((M1 ' transpose) 'display) (newline)

(display (M1 'row-1)) (newline) 
(display (M1 'row-2)) (newline) 

(display (M1 'col-1)) (newline) 
(display (M1 'col-2)) (newline) 

(display (M1 'value-by-pos 2 2)) (newline) 