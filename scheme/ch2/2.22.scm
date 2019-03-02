(define nil ())

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))

(newline)
(display (square-list (list 1 2 3 4 5))) ; Wrong! List is reversed (25 16 9 4 1)

; The order of the list is reversed because we append elements to the head of the list going through the original list
; (1 2 3 4 5) () -> (2 3 4 5) (1) -> (3 4 5) (2 1) -> ... -> (5 4 3 2 1)

; Exchanging the order of the arguments to cons does not help as we would not even get a list data structure as a result
; list is (cons 1 (cons 2 (cons 3 nil))) not (cons (cons (cons 1 nil) 2) 3)

; Correct iterative version

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer (list (square (car things)))))))
  (iter items nil))

(newline)
(display (square-list (list 1 2 3 4 5)))