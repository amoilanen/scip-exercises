(load (merge-pathnames "../lib/check.scm" (current-load-pathname)))
(load (merge-pathnames "lib/generic-arithmetic.scm" (current-load-pathname)))

(define (div-terms l1 l2)
  (cond ((empty-termlist? l2)
         (error "Division by the zero polynomial -- DIV-TERMS" l1))
        ((empty-termlist? l1)
         (list (the-empty-termlist) (the-empty-termlist)))
        (else
         (let ((t1 (first-term l1))
               (t2 (first-term l2)))
           (if (> (order t2) (order t1))
               (list (the-empty-termlist) l1)
               (let* ((new-term (make-term (- (order t1) (order t2))
                                           (div (coeff t1) (coeff t2))))
                      (rest-of-result
                       (div-terms (sub-terms l1 (mul-term-by-all-terms
                                                 new-term l2))
                                  l2)))
                 (list (adjoin-term new-term (car rest-of-result))
                       (cadr rest-of-result))))))))

(define (div-poly p1 p2)
  (let ((var (common-variable p1 p2 'div-poly))
        (result (div-terms (term-list p1) (term-list p2))))
    (list (make-poly var (car result))
          (make-poly var (cadr result)))))

(put 'div '(polynomial polynomial)
     (lambda (p1 p2)
       (map (lambda (p) (attach-tag 'polynomial p))
            (div-poly p1 p2))))

(check (div (make-polynomial 'x '((5 1) (0 -1)))
            (make-polynomial 'x '((2 1) (0 -1))))
       => (list (make-polynomial 'x '((3 1) (1 1)))
                (make-polynomial 'x '((1 1) (0 -1)))))

(check (div (make-polynomial 'x '((2 1) (1 2) (0 1)))
            (make-polynomial 'x '((1 1) (0 1))))
       => (list (make-polynomial 'x '((1 1) (0 1)))
                (make-polynomial 'x '())))

(check (div (make-polynomial 'x '((2 1) (0 1)))
            (make-polynomial 'x '((1 2))))
       => (list (make-polynomial 'x '((1 1/2)))
                (make-polynomial 'x '((0 1)))))

(check (div (make-polynomial 'x '((1 1)))
            (make-polynomial 'x '((2 1))))
       => (list (make-polynomial 'x '())
                (make-polynomial 'x '((1 1)))))

(check (div (make-polynomial 'x '())
            (make-polynomial 'x '((1 1))))
       => (list (make-polynomial 'x '())
                (make-polynomial 'x '())))

(check-error (div (make-polynomial 'x '((1 1)))
                  (make-polynomial 'x '())))
(check-error (div (make-polynomial 'x '((1 1)))
                  (make-polynomial 'y '((1 1)))))
