(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/serializers.scm" (current-load-pathname)))

;; Only 110 disappears: the squaring reads x twice under the serializer,
;; so the increment can no longer change x between the two reads.  The
;; squaring's write is not serialized, so it can still fall between the
;; increment's read and write (11), or the whole increment between the
;; squaring's reads and its write (100).  101 and 121 are the sequential
;; orders.

;; The squaring's two reads of x are separate steps, as in (* x x).

(define (unserialized-world)
  (let ((x 10))
    (parallel-execute
     (lambda ()
       (let* ((a (atomic x))
              (b (atomic x)))
         (atomic (set! x (* a b)))))
     (lambda ()
       (let ((a (atomic x)))
         (atomic (set! x (+ a 1))))))
    x))

(check (possible-outcomes unserialized-world)
       (=> same-set?)
       '(11 100 101 110 121))

(define (serialized-world)
  (let ((x 10)
        (s (make-serializer)))
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

(check (possible-outcomes serialized-world) (=> same-set?) '(11 100 101 121))
