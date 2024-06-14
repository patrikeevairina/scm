(define list-iter
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

(define Data '(1 2 3 4 5 6 7))

(define get-next-list-key (list-iter Data))
(display (get-next-list-key))(newline)
(display (get-next-list-key))(newline)

; (let ((get-next (list-iter Data)))
;  (let loop ((e (get-next)))
; 	(if (eq? e 'end) 'end 
; 	 (begin (display e)(newline)(loop (get-next)))
;   )
;  )	
; )
