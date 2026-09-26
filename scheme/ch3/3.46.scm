(load "lib/check.scm")
(load "ch3/lib/serializers.scm")

;; If test-and-set! reads the cell and sets it in two separate steps, two
;; processes can both acquire the mutex:
;;
;;   process 1               cell    process 2
;;   reads false             false
;;                           false   reads false
;;   sets true               true
;;                           true    sets true
;;   has the mutex                   has the mutex

(define (atomic-test-and-set! cell)
  (atomic (if (car cell)
              true
              (begin (set-car! cell true)
                     false))))

(define (unsafe-test-and-set! cell)
  (if (atomic (car cell))
      true
      (begin (atomic (set-car! cell true))
             false)))

;; Each of two processes tries once to acquire a free mutex; the outcome is
;; how many succeeded.
(define (acquirers test-and-set!)
  (possible-outcomes
   (lambda ()
     (let ((cell (list false))
           (acquired 0))
       (define (try-to-acquire)
         (if (not (test-and-set! cell))
             (atomic (set! acquired (+ acquired 1)))))
       (parallel-execute try-to-acquire try-to-acquire)
       acquired))))

(check (acquirers atomic-test-and-set!) => '(1))
(check (acquirers unsafe-test-and-set!) (=> same-set?) '(1 2))
