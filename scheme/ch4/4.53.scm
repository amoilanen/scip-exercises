(load "lib/check.scm")
(load "ch4/4.51.scm")
(load "ch4/4.52.scm")

;; Loading 4.52 reloaded the evaluator, which dropped permanent-set!.
(install-special-form! 'permanent-set! analyze-permanent-assignment)

;; The (amb) after each permanent-set! forces the search to go on until
;; every pair has been found; when none are left, if-fail returns the list
;; the permanent assignments built, most recent first.

(define env
  (amb-environment
   '(define (prime? n)
      (define (divisible-from? d)
        (cond ((> (square d) n) false)
              ((= (remainder n d) 0) true)
              (else (divisible-from? (+ d 1)))))
      (and (> n 1) (not (divisible-from? 2))))
   '(define (prime-sum-pair list1 list2)
      (let ((a (an-element-of list1))
            (b (an-element-of list2)))
        (require (prime? (+ a b)))
        (list a b)))))

(check (amb-collect '(let ((pairs '()))
                       (if-fail (let ((p (prime-sum-pair '(1 3 5 8)
                                                         '(20 35 110))))
                                  (permanent-set! pairs (cons p pairs))
                                  (amb))
                                pairs))
                    env)
       => '(((8 35) (3 110) (3 20))))
