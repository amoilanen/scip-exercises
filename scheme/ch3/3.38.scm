(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/serializers.scm" (current-load-pathname)))

;; a. Run one after another, in any of the six orders, the transactions
;;    leave 35, 40, 45 or 50.
;; b. Interleaved, each transaction reads the balance and only later writes
;;    it, so a write can overwrite another's; Mary even reads it twice.  Any
;;    of the values below can result, e.g. 110: Peter reads 100, Paul and
;;    Mary finish, and Peter writes 100 + 10.

(define (sequential-world)
  (let ((balance 100))
    (parallel-execute
     (lambda () (atomic (set! balance (+ balance 10))))
     (lambda () (atomic (set! balance (- balance 20))))
     (lambda () (atomic (set! balance (- balance (/ balance 2))))))
    balance))

(check (possible-outcomes sequential-world) (=> same-set?) '(35 40 45 50))

(define (interleaved-world)
  (let ((balance 100))
    (parallel-execute
     (lambda ()
       (let ((peter (atomic balance)))
         (atomic (set! balance (+ peter 10)))))
     (lambda ()
       (let ((paul (atomic balance)))
         (atomic (set! balance (- paul 20)))))
     (lambda ()
       (let* ((minuend (atomic balance))
              (half (/ (atomic balance) 2)))
         (atomic (set! balance (- minuend half))))))
    balance))

(check (sort (possible-outcomes interleaved-world) <)
       => '(25 30 35 40 45 50 55 60 65 70 80 90 110))
