(load "lib/check.scm")
(load "ch4/lib/amb.scm")

;; The order of the requirements cannot change the answer: all five floors
;; are chosen before any requirement is checked, so the same 3125
;; assignments are generated and each one is accepted exactly when all
;; requirements hold.
;;
;; The order does affect the time, though only moderately.  An assignment's
;; requirements are checked until the first one fails, so the order decides
;; how much work each rejected assignment costs.  distinct? is by far the
;; most expensive check; testing it last, only for the few assignments that
;; pass the cheap comparisons, makes the whole search faster.  The
;; generation of the 3125 assignments costs the same either way.

(define reordered-env
  (amb-environment
   '(define (multiple-dwelling)
      (let ((baker (amb 1 2 3 4 5))
            (cooper (amb 1 2 3 4 5))
            (fletcher (amb 1 2 3 4 5))
            (miller (amb 1 2 3 4 5))
            (smith (amb 1 2 3 4 5)))
        (require (not (= baker 5)))
        (require (not (= cooper 1)))
        (require (not (= fletcher 5)))
        (require (not (= fletcher 1)))
        (require (> miller cooper))
        (require (not (= (abs (- smith fletcher)) 1)))
        (require (not (= (abs (- fletcher cooper)) 1)))
        (require (distinct? (list baker cooper fletcher miller smith)))
        (list (list 'baker baker)
              (list 'cooper cooper)
              (list 'fletcher fletcher)
              (list 'miller miller)
              (list 'smith smith))))))

(define book-env (apply amb-environment multiple-dwelling-program))

(define (solutions-and-work env)
  (let ((solutions '()))
    (let ((work (count-applications
                 (lambda ()
                   (set! solutions (amb-collect '(multiple-dwelling) env))))))
      (cons solutions work))))

(let ((book (solutions-and-work book-env))
      (reordered (solutions-and-work reordered-env)))
  (check (car book)
         => '(((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))))
  (check (car reordered) => (car book))
  (check (< (cdr reordered) (cdr book)) => #t))
