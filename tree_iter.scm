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
(define Tree2 '(
							 ( 
								(() 1()) 
								9 
								(() 3 ()) 
							 ) 
							 10 
							 ( 
								(() 5 ()) 
							 )
							)
)

(define left-branch car)
(define right-branch caddr)
(define node-value cadr)	


(format #t "Tree: ~a\n" Tree)
(format #t "Tree2: ~a\n" Tree2)

; (display "Recursive traverse:")(newline)

(define (Traverse-rec tree callback) 
 (if (not (null? tree))
	(begin
	 (Traverse-rec (left-branch tree) callback)
	 (callback (node-value tree))
	 (Traverse-rec (right-branch tree) callback)
	)
 )
)

(define (print-key k) (display k)(newline))
; (Traverse-rec Tree print-key)

;;(quit)

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

; (define (compare-tree t1 t2)
;     (let    (
;                 (iter1 (make-tree-iterator t1))
;                 (iter2 (make-tree-iterator t2))
;                 (let loop ((v1 (iter1)) (v2 iter2))
;                      )
;                 (cond 
;                     ((eq? v1 'end) (eq? v2 'end))
;                     ((eq? v2 'end) #f)
;                     ((eq? v1 v2) (loop (iter1) (iter2)))
;                     (else #f)
;                 )
;             )
         
;     )    
; )

(define (compare-tree t1 t2)
	(let (
	         (caller #f) 
	         (iter1 (make-tree-iterator t1)) 
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

(display (compare-tree Tree Tree2))(newline)

