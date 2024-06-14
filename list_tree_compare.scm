(define left-branch car)
(define right-branch caddr)
(define node-value cadr)

(define (make-tree-iterator tree)
 (let ((caller #f))
  (letrec ((traverse  ;; traverse procedure
						(lambda ()
						 (let loop ((tree tree))
							(if (not (null? tree))
							 (begin
								(loop (left-branch tree))
								;; -- do st-th with key
								(call/cc (lambda (rest-of-tree) 
													(set! traverse (lambda () (rest-of-tree 'dummy)))
													(caller (node-value tree))
												 )
								)
								(loop (right-branch tree))
							 )
							)
						 )
						 (caller 'end)
							
						)
					)) ;; letrec

	 (lambda () ;; iterator procedure
		(call/cc (lambda (k) (set! caller k) (traverse)))
	 ) ;; iterator proc end
	)
 )
)

(define make-list-iterator
  (lambda (a-list)
    (define iter
      (lambda ()
        (call-with-current-continuation control-state)))
    (define control-state
      (lambda (return)
        (for-each
          (lambda (element)
            (set! return (call-with-current-continuation
                           (lambda (resume-here)
                             (set! control-state resume-here)
                             (return element)))))
          a-list)
        (return 'end)))
    iter))


(define Tree '(
							 ( 
								(() 4 ()) 
								2 
								(() 5 ()) 
							 ) 
							 1 
							 ( 
								(() 6 ()) 
								3 
								(() 7 ())
							 )
							)
)

(define Data '(4 2 5 1 6 3 7))

(define (compare-list-tree t1 t2)
	(let (
	         (caller #f) 
	         (iter1 (make-list-iterator t1)) 
	         (iter2 (make-tree-iterator t2)))
	(letrec (
	            (loop (lambda (v1 v2) ; определяем рекурсивную функцию
	                   ;   (
	                   ;       begin
	                   ;       (display v1)
	                   ;       (display v2)(newline)
								(cond 
									((eq? v1 'end) (caller (eq? v2 'end)))
									((eq? v2 'end) (caller #f))
									((eq? v1 v2) (loop (iter1) (iter2)))
									(else (caller #f))
								)
	                   ;   )
				      )
				)
            )
			(call/cc 
				(lambda (cc) 
					(set! caller cc) 
					(loop (iter1) (iter2))
				)
			)
		)
	)
)

(display (compare-list-tree Data Tree))(newline)