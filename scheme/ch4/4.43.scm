(load "lib/check.scm")
(load "ch4/lib/amb.scm")

;; Every father's yacht is known: Parker's is the one name left, Mary Ann.
;; Sir Barnacle's daughter is Melissa, so only four daughters remain to be
;; placed, each checked against her father's yacht as soon as she is chosen.
;; Lorna's father is Colonel Downing.  Without knowing that Mary Ann's last
;; name is Moore there are two solutions: Lorna's father is then Downing or
;; Parker.

(define env
  (amb-environment
   '(define yachts
      '((barnacle gabrielle) (moore lorna) (downing melissa)
        (hall rosalind) (parker mary-ann)))
   '(define (yacht-of father)
      (cadr (assq father yachts)))
   '(define (father-of daughter daughters)
      (cond ((null? daughters) false)
            ((eq? (cadr (car daughters)) daughter) (car (car daughters)))
            (else (father-of daughter (cdr daughters)))))
   '(define (a-daughter-of father taken)
      (let ((daughter
             (an-element-of '(mary-ann gabrielle lorna rosalind melissa))))
        (require (not (memq daughter taken)))
        (require (not (eq? daughter (yacht-of father))))
        daughter))
   '(define (daughters mary-ann-moore?)
      (let* ((barnacle 'melissa)
             (moore (a-daughter-of 'moore (list barnacle))))
        (if mary-ann-moore?
            (require (eq? moore 'mary-ann)))
        (let* ((downing (a-daughter-of 'downing (list barnacle moore)))
               (hall (a-daughter-of 'hall (list barnacle moore downing)))
               (parker
                (a-daughter-of 'parker (list barnacle moore downing hall)))
               (daughters (list (list 'barnacle barnacle)
                                (list 'moore moore)
                                (list 'downing downing)
                                (list 'hall hall)
                                (list 'parker parker))))
          (require (eq? (yacht-of (father-of 'gabrielle daughters)) parker))
          daughters)))))

(check (amb-collect '(daughters true) env)
       => '(((barnacle melissa) (moore mary-ann) (downing lorna)
             (hall gabrielle) (parker rosalind))))
(check (amb-collect '(father-of 'lorna (daughters true)) env) => '(downing))

(check (amb-collect '(father-of 'lorna (daughters false)) env)
       => '(downing parker))
