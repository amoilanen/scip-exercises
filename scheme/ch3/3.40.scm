(load "lib/check.scm")
(load "ch3/lib/serializers.scm")

;; Each process reads x once per factor, so a process can multiply values
;; of x from before and after the other's write.  Unserialized, x ends as
;; 10^2, 10^3, 10^4, 10^5 or 10^6; serialized, only 10^6 = (10^2)^3 =
;; (10^3)^2 remains.

(define (world serialize)
  (let ((x 10))
    (parallel-execute
     (serialize (lambda ()
                  (let* ((a (atomic x))
                         (b (atomic x)))
                    (atomic (set! x (* a b))))))
     (serialize (lambda ()
                  (let* ((a (atomic x))
                         (b (atomic x))
                         (c (atomic x)))
                    (atomic (set! x (* a b c)))))))
    x))

(check (possible-outcomes (lambda () (world (lambda (p) p))))
       (=> same-set?)
       '(100 1000 10000 100000 1000000))

(check (possible-outcomes (lambda () (world (make-serializer))))
       => '(1000000))
