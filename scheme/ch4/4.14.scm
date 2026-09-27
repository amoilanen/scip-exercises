(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/mceval.scm" (current-load-pathname)))

;; Eva's map is an evaluated procedure, so it calls its argument through
;; mc-apply, which knows how to apply both kinds of evaluator procedures.
;; Louis's map is the underlying Scheme's map installed as a primitive: it
;; receives evaluator procedure objects, which are just lists such as
;; (procedure (x) ((* x x)) <env>) or (primitive <Scheme car>),
;; and tries to call them as Scheme procedures, which fails.

(define eva-map
  '(define (map proc items)
     (if (null? items)
         '()
         (cons (proc (car items)) (map proc (cdr items))))))

(check (interpret eva-map '(map (lambda (x) (* x x)) '(1 2 3))) => '(1 4 9))
(check (interpret eva-map '(map car '((1 2) (3 4)))) => '(1 3))

(define primitive-procedures
  (cons (list 'map map) primitive-procedures))

(check-error (interpret '(map (lambda (x) (* x x)) '(1 2 3))))
(check-error (interpret '(map car '((1 2) (3 4)))))
(check (interpret '(map (lambda (x) x) '())) => '())
