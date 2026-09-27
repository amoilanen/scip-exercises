(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/eceval.scm" (current-load-pathname)))

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let->combination exp)
  (let ((bindings (let-bindings exp)))
    (cons (make-lambda (map car bindings) (let-body exp))
          (map cadr bindings))))

(define derived-forms-code
  '(ev-cond
    (assign exp (op cond->if) (reg exp))
    (goto (label eval-dispatch))
    ev-let
    (assign exp (op let->combination) (reg exp))
    (goto (label eval-dispatch))))

(define derived-eceval
  (make-eceval (append eceval-dispatch-table
                       '((cond? ev-cond)
                         (let? ev-let)))
               derived-forms-code
               (operation-entries 'cond? cond?
                                  'cond->if cond->if
                                  'let? let?
                                  'let->combination let->combination)))

(define (run . exps)
  (apply eceval-run derived-eceval exps))

(check (let->combination '(let ((a 1) (b 2)) (display a) (+ a b)))
       => '((lambda (a b) (display a) (+ a b)) 1 2))

(define sign
  '(define (sign x)
     (cond ((< x 0) 'negative)
           ((= x 0) 'zero)
           (else 'positive))))

(check (run sign '(list (sign -5) (sign 0) (sign 7)))
       => '(negative zero positive))
(check (run '(cond ((= 1 2) 'no))) => #f)
(check (run '(cond ((= 1 1) (define x 1) (+ x 1)) (else 'no))) => 2)

(check (run '(let ((x 2) (y 3)) (* x y))) => 6)
(check (run '(let () 5)) => 5)
(check (run '(define x 10)
            '(let ((x 1) (y x))
               (let ((z (+ x y)))
                 (list x y z))))
       => '(1 10 11))

(define (count-down-depth n)
  (run '(define (count-down n)
          (cond ((= n 0) 'done)
                (else (let ((m (- n 1)))
                        (count-down m)))))
       (list 'count-down n))
  (cdr (assq 'maximum-depth (stack-statistics derived-eceval))))

;; The expanded forms go through eval-dispatch, so tail calls stay tail calls.
(check (count-down-depth 5) => (count-down-depth 50))
