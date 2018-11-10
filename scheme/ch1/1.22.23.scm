(define (naive-next d)
  (+ 1 d))

(define (faster-next d)
  (if (= 2 d) (+ d 1)
                (+ d 2)))

(define (smallest-divisor next n)
  (find-divisor next n 2))
(define (find-divisor next n test-divisor)
  (sleep-current-thread 10)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor next n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? next n)
  (= n (smallest-divisor next n)))

(define (timed-prime-test next n)
  (start-prime-test next n (real-time-clock)))
(define (start-prime-test next n start-time)
  (if (prime? next n)
      (report-prime n (- (real-time-clock) start-time))))

(define (report-prime n elapsed-time)
  (display n)
  (display  " *** ")
  (display elapsed-time)
  (newline))

(define (search-for-primes next from to)
  (timed-prime-test next from)
  (if (>= from to)
      ((newline) (display "finished"))
      (search-for-primes next (+ 1 from) to)))

; Sizes of the input: first call n, second call 100 * n
; since time is square root, first call executes at t, second at 10 * t
(
  (search-for-primes faster-next 10000 10100)
  (search-for-primes faster-next 100 200)
  (newline)
  (display "faster-next")
  (search-for-primes naive-next 10000 10100)
  (search-for-primes naive-next 100 200)
  (newline)
  (display "naive-next")
)