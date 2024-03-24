(define (reverse-list lst)
    (if (null? lst)
        lst
        (append (reverse-list (cdr lst)) (list (car lst)))
    )
)
  
(define my-list (list 1 2 3 4 5))
(display (reverse-list my-list)); возвращает (5 4 3 2 1)