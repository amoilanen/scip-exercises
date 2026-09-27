(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/compiler.scm" (current-load-pathname)))

(define factorial
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n))))

(define factorial-alt
  '(define (factorial-alt n)
     (if (= n 1)
         1
         (* n (factorial-alt (- n 1))))))

;; Operands are evaluated right to left.  In factorial, n is evaluated first
;; and put into argl, so argl has to be saved around the recursive call.  In
;; factorial-alt the recursive call comes first; env has to be saved around
;; it because n is looked up afterwards, while argl is built only after the
;; call.  Everything else is the same: one save is traded for another, so
;; neither program is more efficient.

(define (saved-registers exp)
  (filter-map (lambda (inst) (and (tagged-list? inst 'save) (cadr inst)))
              (statements (compile exp 'val 'next))))

(check (saved-registers factorial) => '(continue env continue proc argl proc))
(check (saved-registers factorial-alt)
       => '(continue env continue proc env proc))

(define (run-with-statistics . exps)
  (let ((machine (apply make-compiled-machine exps)))
    (start machine)
    (list (get-register-contents machine 'val)
          (stack-statistics machine))))

(check (run-with-statistics factorial '(factorial 6))
       => (run-with-statistics factorial-alt '(factorial-alt 6)))
(check (car (run-with-statistics factorial-alt '(factorial-alt 6))) => 720)
