(load "lib/check.scm")
(load "ch3/lib/serializers.scm")

;; The squaring reads x twice under the serializer, so it can no longer see
;; x change halfway (11 and 110 are gone), but its write is outside the
;; serializer: 100 remains, when the increment runs between the squaring's
;; read and write.  101 and 121 are the two sequential orders.


(define x)

(define (unserialized-world)
  (set! x 10)
  (parallel-execute
   (lambda ()
     (let* ((a (atomic x))
            (b (atomic x)))
       (atomic (set! x (* a b)))))
   (lambda ()
     (let ((a (atomic x)))
       (atomic (set! x (+ a 1))))))
  x)

(check (possible-outcomes unserialized-world)
       (=> same-set?)
       '(11 100 101 110 121))

(define (serialized-world)
  (set! x 10)
  (let ((s (make-serializer)))
    (parallel-execute
     (lambda ()
       (let ((squared ((s (lambda ()
                            (let* ((a (atomic x))
                                   (b (atomic x)))
                              (* a b)))))))
         (atomic (set! x squared))))
     (s (lambda ()
          (let ((a (atomic x)))
            (atomic (set! x (+ a 1)))))))
    x))

(check (possible-outcomes serialized-world) (=> same-set?) '(100 101 121))
