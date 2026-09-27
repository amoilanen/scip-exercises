(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))

(define (count-pairs x)
  (let ((visited '()))
    (define (walk x)
      (if (or (not (pair? x)) (memq x visited))
          0
          (begin (set! visited (cons x visited))
                 (+ (walk (car x))
                    (walk (cdr x))
                    1))))
    (walk x)))

(define three (list 'a 'b 'c))

(define four
  (let ((x (list 'a)))
    (list x x)))

(define seven
  (let* ((x (list 'a))
         (y (cons x x)))
    (cons y y)))

(define cycle
  (let ((x (list 'a 'b 'c)))
    (set-cdr! (cddr x) x)
    x))

(check (count-pairs '()) => 0)
(check (count-pairs 'a) => 0)
(check (count-pairs three) => 3)
(check (count-pairs four) => 3)
(check (count-pairs seven) => 3)
(check (count-pairs cycle) => 3)
(check (count-pairs (list (list 1 2) (list 3))) => 5)
