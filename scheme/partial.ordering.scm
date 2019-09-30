(define items (list (list 1 2) (list 'a 'b) (list 2 3) (list 9 10) (list 5 6) (list 'b 'c) (list 3 4)))

(define (build-ordering relations)
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
          (append (segment-elements first) (segment-elements second))))
      ((eq? (segment-end second) (segment-start first))
        (make-segment
          (segment-start second)
          (segment-end first)
          (append (segment-elements second) (segment-elements first))))
      (else error "Cannot merge segments" first second)))

  (define (merge-segments segments)
    (let ((merged-segments (try-merging segments)))
      (if (= (length merged-segments) (length segments))
        segments
        (merge-segments merged-segments))))
  (define (try-merging segments)
    ;TODO: Generate the list of all possible segment pairs, find a pair that is mergable, cons the merged pair with the rest of the segments
    segments
  )
  (let ((segments (map (lambda (relation) (make-segment (car relation) (cadr relation) relation)) relations)))
    (merge-segments segments)))

(newline)
(display (build-ordering items))