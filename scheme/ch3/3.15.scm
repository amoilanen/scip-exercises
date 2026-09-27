(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

;; z1 -> [*|*]
;;        | |
;;        v v
;;   x -> [a|*]->[b|/]
;; The car and cdr of z1 are the same list x, so changing (car z1) changes
;; (cdr z1) too: z1 becomes ((wow b) wow b).
;;
;; z2 -> [*|*]->[a|*]->[b|/]
;;        |
;;        +---->[a|*]->[b|/]
;; The car of z2 is a fresh list (a b) equal to but distinct from the cdr, so
;; only the car changes: z2 becomes ((wow b) a b).

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(check (set-to-wow! z1) => '((wow b) wow b))
(check (set-to-wow! z2) => '((wow b) a b))
(check (eq? (car z1) (cdr z1)) => #t)
(check (eq? (car z2) (cdr z2)) => #f)
