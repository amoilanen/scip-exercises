(load "lib/check.scm")
(load "ch3/3.55.scm")

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream (partial-sums (ln2-summands 1)))

(define (elements-until-within tolerance s)
  (let loop ((s s) (count 1))
    (if (< (abs (- (stream-car s) (log 2))) tolerance)
        count
        (loop (stream-cdr s) (+ count 1)))))

(check (stream-ref ln2-stream 3) (=> (approx= 1e-12)) 7/12)

;; The partial sums converge slowly: their error is about 1/(2n), so they
;; need 500 terms to get within 1e-3 of ln 2 and would need 500000 for 1e-6.
;; The Euler transform gets within 1e-6 after 49 terms and the accelerated
;; sequence after 5; its 8th term is exact to machine precision.
(check (elements-until-within 1e-3 ln2-stream) => 500)
(check (elements-until-within 1e-3 (euler-transform ln2-stream)) => 4)
(check (elements-until-within 1e-6 (euler-transform ln2-stream)) => 49)
(check (elements-until-within 1e-6
                              (accelerated-sequence euler-transform
                                                    ln2-stream))
       => 5)
(check (stream-ref (accelerated-sequence euler-transform ln2-stream) 7)
       (=> (approx= 1e-14))
       (log 2))
