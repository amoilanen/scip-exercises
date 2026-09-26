(load "lib/check.scm")
(load "ch4/lib/amb.scm")

;; Choosing each of the five people a floor independently gives 5^5 = 3125
;; assignments; only 5! = 120 of them assign distinct floors.
;;
;; The faster version chooses the people one at a time, gives each one only
;; a floor that is still free, and checks every requirement as soon as the
;; floors it mentions are known, so that a hopeless partial assignment is
;; abandoned before the remaining people are placed.

(define env
  (amb-environment
   '(define (a-free-floor taken)
      (let ((floor (amb 1 2 3 4 5)))
        (require (not (memq floor taken)))
        floor))
   '(define (multiple-dwelling)
      (let ((fletcher (a-free-floor '())))
        (require (not (= fletcher 5)))
        (require (not (= fletcher 1)))
        (let ((cooper (a-free-floor (list fletcher))))
          (require (not (= cooper 1)))
          (require (not (= (abs (- fletcher cooper)) 1)))
          (let ((miller (a-free-floor (list fletcher cooper))))
            (require (> miller cooper))
            (let ((smith (a-free-floor (list fletcher cooper miller))))
              (require (not (= (abs (- smith fletcher)) 1)))
              (let ((baker
                     (a-free-floor (list fletcher cooper miller smith))))
                (require (not (= baker 5)))
                (list (list 'baker baker)
                      (list 'cooper cooper)
                      (list 'fletcher fletcher)
                      (list 'miller miller)
                      (list 'smith smith))))))))))

(check (length (amb-collect '(list (amb 1 2 3 4 5) (amb 1 2 3 4 5)
                                   (amb 1 2 3 4 5) (amb 1 2 3 4 5)
                                   (amb 1 2 3 4 5))
                            env))
       => 3125)

(check (length (amb-collect '(let* ((a (a-free-floor '()))
                                    (b (a-free-floor (list a)))
                                    (c (a-free-floor (list a b)))
                                    (d (a-free-floor (list a b c))))
                               (list a b c d (a-free-floor (list a b c d))))
                            env))
       => 120)

;; The book's version makes over 50000 procedure applications (see 4.39);
;; this one needs fewer than one per assignment the book's version examines.
(check (amb-collect '(multiple-dwelling) env)
       => '(((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))))
(check (< (count-applications
           (lambda () (amb-collect '(multiple-dwelling) env)))
          3125)
       => #t)
