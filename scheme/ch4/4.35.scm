(load "lib/check.scm")
(load "ch4/lib/amb.scm")

(define integer-between-program
  '((define (an-integer-between low high)
      (require (<= low high))
      (amb low (an-integer-between (+ low 1) high)))
    (define (a-pythagorean-triple-between low high)
      (let ((i (an-integer-between low high)))
        (let ((j (an-integer-between i high)))
          (let ((k (an-integer-between j high)))
            (require (= (+ (* i i) (* j j)) (* k k)))
            (list i j k)))))))

(define env (apply amb-environment integer-between-program))

(check (amb-collect '(an-integer-between 3 6) env) => '(3 4 5 6))
(check (amb-collect '(an-integer-between 4 4) env) => '(4))
(check (amb-collect '(an-integer-between 5 4) env) => '())

(check (amb-collect '(a-pythagorean-triple-between 1 15) env)
       => '((3 4 5) (5 12 13) (6 8 10) (9 12 15)))
(check (amb-collect '(a-pythagorean-triple-between 4 15) env)
       => '((5 12 13) (6 8 10) (9 12 15)))
(check (amb-collect '(a-pythagorean-triple-between 1 4) env) => '())
