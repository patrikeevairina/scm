(define (make-matrix matrix)
    (let ((matrix matrix))
        (define (row-f) (car matrix))
        (define (row-s) (car (cdr matrix)))
        (define (col-f) (list (car (row-f)) (car (row-s))))
        (define (col-s) (list (car (cdr (row-f))) (car (cdr (row-s)))))
        (define (mat_disp) (format #t "|~a|\n|~a|\n\n" (row-f) (row-s)))
        (define (value-by-pos x y) 
            (let ((fRow (row-f)) (sRow (row-s)) (x x) (y y))
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
                (list (map + (row-f) (matrix2 'row-f))
                      (map + (row-s) (matrix2 'row-s))
                )
            )
        )
    
        (define (minus matrix2)
            (make-matrix 
                (list (map - (row-f) (matrix2 'row-f))
                      (map - (row-s) (matrix2 'row-s))
                )
            )
        )
    
        (define (MatMalt matrix2)
            (make-matrix
                (list
                    (list
                        (apply + (map * (row-f) (matrix2 'col-f)))
                        (apply + (map * (row-f) (matrix2 'col-s)))
                    )
                
                    (list
                        (apply + (map * (row-s) (matrix2 'col-f)))
                        (apply + (map * (row-s) (matrix2 'col-s)))
                    )
                )
            )    
        )
    
        (define (VecMalt vec)
            (list
                (apply + (map * (row-f) vec))
                (apply + (map * (row-s) vec))  
            )
        )
    
        (define (LamMalt n)
            (make-matrix
                (list
                    (map * (list n n) (row-f))   
                    (map * (list n n) (row-s)) 
                )
            )
        )
    
    
        (define (transpose)
            (make-matrix (list (col-f) (col-s)))
        )
    
    
        (lambda args
            (apply
                (case (car args)
                    ((row-f) row-f)
                    ((row-s) row-s)
                    ((col-f) col-f)
                    ((col-s) col-s)
                    ((mat_disp) mat_disp)
                    ((plus) plus)
                    ((minus) minus)
                    ((MatMalt) MatMalt)
                    ((VecMalt) VecMalt)
                    ((LamMalt) LamMalt)
                    ((transpose) transpose)
                    ((value-by-pos) value-by-pos)
                    (else 
                    (begin (display "Invalid method\n") (exit 1) ))
                )
                (cdr args)
            )
        )
    )
)
 


(define M1 (make-matrix (list (list 1 2) (list 3 4))))
(define M2 (make-matrix (list (list 5 6) (list 7 8))))
(M1 'mat_disp)  
(M2 'mat_disp) 
 
((M1 'plus M2) 'mat_disp) 

((M1 'minus M2) 'mat_disp) 

((M1 'LamMalt 2) 'mat_disp) 

((M1 'MatMalt M2) 'mat_disp)  

(display (M1 'VecMalt (list 2 3))) (newline) 

((M1 ' transpose) 'mat_disp) (newline)

(display (M1 'row-f)) (newline) 
(display (M1 'row-s)) (newline) 

(display (M1 'col-f)) (newline) 
(display (M1 'col-s)) (newline) 


(display (M1 'value-by-pos 2 2)) (newline) 