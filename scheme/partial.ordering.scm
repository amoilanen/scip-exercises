(define items (list (list 1 2) (list 'a 'b) (list 2 3) (list 9 10) (list 5 6) (list 'b 'c) (list 3 4)))

;(define items (list (list 'real 'complex) (list 'rational 'real) (list 'integer 'rational) (list 'b 'a) (list 'c 'b)))

(define (pairs-of list)
  (define (pairs-with item rest-of-items)
    (append
      (map (lambda (x) (cons item x)) rest-of-items)
      (pairs-of rest-of-items)))
  (if (null? list)
      '()
      (pairs-with (car list) (cdr list))))

(define (build-ordering relations)
  (define (last-element list) (car (reverse list)))
  (define (segment-start segment)
    (car segment))
  (define (segment-end segment)
    (cadr segment))
  (define (segment-elements segment)
    (caddr segment))
  (define (make-segment start end elements)
    (list start end elements))
  (define (segments-can-be-merged first second)
    (or
      (eq? (segment-end first) (segment-start second))
      (eq? (segment-end second) (segment-start first))))
  (define (merge-two-segments first second)
    (cond
      ((eq? (segment-end first) (segment-start second))
        (make-segment
          (segment-start first)
          (segment-end second)
          (append (segment-elements first) (cdr (segment-elements second)))))
      ((eq? (segment-end second) (segment-start first))
        (make-segment
          (segment-start second)
          (segment-end first)
          (append (segment-elements second) (cdr (segment-elements first)))))
      (else error "Cannot merge segments" first second)))

  (define (merge-segments segments)
    (let ((merged-segments (try-merging segments)))
      (if (= (length merged-segments) (length segments))
        segments
        (merge-segments merged-segments))))
  (define (try-merging segments)
    (let ((segment-pairs (pairs-of segments)))
      (let ((mergable-segment-pair
             (find (lambda (pair)
                     (segments-can-be-merged (car pair) (cdr pair)))
                   segment-pairs)))
        (if mergable-segment-pair
            (let ((first-segment (car mergable-segment-pair))
                  (second-segment (cdr mergable-segment-pair)))
              (let ((rest-of-segments
                     (filter
                       (lambda (segment)
                         (and (not (eq? segment first-segment)) (not (eq? segment second-segment))))
                       segments)))
                (cons (merge-two-segments first-segment second-segment) rest-of-segments)))
            segments))))
  (define (to-relations segments)
    (map segment-elements segments))
  (let ((segments (map (lambda (relation) (make-segment (car relation) (last-element relation) relation)) relations)))
    (to-relations (merge-segments segments))))

(newline)
(display (build-ordering items))