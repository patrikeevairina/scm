(define-syntax memo
    (syntax-rules ()
        ((memo (define func (lambda (args ...) body ...)))
            (begin
                (define target-func (lambda (args ...) body ...))
                (define func 
                    (letrec ((memo-data (make-hash-table 'equal?)))
                        (lambda (args ...)
                            (let ((key (list args ...)))
                                (if not(hash-table-exists? memo-data key) 
                                    (begin
                                        (display "memo")(newline)
                                        (hash-table-set! memo-data key (target-func args ...))
                                    )
                                    '()
                                     
                                )
                                (hash-table-ref memo-data key))))))
        )    
    )
)


(memo 
(define fib
  (lambda (n i)
    (if (or (= n 1)
            (= n 2))
        1
        (+ (fib (- n 1) i)
          (fib (- n 2) i)
          i)))))

(display (fib 10 1))
(newline) 
(display (fib 10 1))
(newline) 