(load "lib/check.scm")
(load "ch3/3.74.scm")

;; Louis passes the average avpt on as last-value, so each "average" is
;; taken between a new value and the previous average: an ever longer
;; weighted mean of the whole signal instead of the mean of two successive
;; values.  The fix keeps the last raw value and the last average apart.
(define (louis-zero-crossings input-stream last-value)
  (if (stream-null? input-stream)
      the-empty-stream
      (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
        (cons-stream (sign-change-detector avpt last-value)
                     (louis-zero-crossings (stream-cdr input-stream)
                                           avpt)))))

(define (smoothed-zero-crossings input-stream last-value last-avpt)
  (if (stream-null? input-stream)
      the-empty-stream
      (let* ((value (stream-car input-stream))
             (avpt (/ (+ value last-value) 2)))
        (cons-stream (sign-change-detector avpt last-avpt)
                     (smoothed-zero-crossings (stream-cdr input-stream)
                                              value
                                              avpt)))))

;; The averages of successive values are 4, 3.5, -1 and -1; Louis's running
;; means are 4, 1.5, 0.25 and -0.375, so he reports the crossing late.
(define signal (list->stream '(8 -1 -1 -1)))

(check (stream->list (smoothed-zero-crossings signal 0 0)) => '(0 0 -1 0))
(check (stream->list (louis-zero-crossings signal 0)) => '(0 0 0 -1))

;; Smoothing removes the spurious crossings caused by the noise at the start.
(define noisy (list->stream '(1 -0.5 1 -3 -1 2 2)))

(check (stream->list (make-zero-crossings noisy)) => '(0 -1 1 -1 0 1 0))
(check (stream->list (smoothed-zero-crossings noisy 0 0))
       => '(0 0 0 -1 0 1 0))
