;; Author: Peter Samarin

(module gng-utilities racket
  (provide (all-defined-out))

  ;;; Units
  ;; need a data structure to represent the units
  ;; each unit will have a position in R^n and local error
  ;; position is a vector of real values
  ;; error is a scalar value
  (define-struct unit (position error edges) #:mutable)
  (define-struct edge (unit1 unit2 age)      #:mutable)

  ;; 3. search through the GNG list
  ;; find the two nearest units and return them in a list
  (define (find-two-nearest data units)
    (let loop ([first-min   0]
               [second-min  0]
               [first-unit  null]
               [second-unit null]
               [units       units])
      (if (empty? units)
          (list first-unit second-unit)
          (let* ([unit (car units)]
                 [new-distance (euclidean-distance (unit-position unit) data)])
            (cond [(null? first-unit)
                   (loop new-distance 0 unit null (cdr units))]
                  [(< new-distance first-min)
                   (loop new-distance first-min unit first-unit (cdr units))]
                  [(or (null? second-unit) (< new-distance second-min))
                   (loop first-min new-distance first-unit unit (cdr units))]
                  [else
                   (loop first-min second-min first-unit
                         second-unit (cdr units))])))))

  ;; finds the edge from unit1 to unit2
  (define (find-edge unit1 unit2 edges)
    (filter (lambda (edge)
              (define n1 (edge-unit1 edge))
              (define n2 (edge-unit2 edge))
              (or (and (equal? unit1 n1)
                       (equal? unit2 n2))
                  (and (equal? unit2 n1)
                       (equal? unit1 n2))))
            edges))

  ;; 4. increment the age of an edge
  (define (increment-age! edge)
    (set-edge-age! edge (+ (edge-age edge) 1)))

  ;; 5. Update local error
  (define (update-local-error! unit error)
    (set-unit-error! unit (+ (unit-error unit) error)))

  ;; 6. Find neighbors
  ;; search the edges and return the the units that are not the provided unit
  (define (find-neighbors unit edges)
    (map (lambda (edge)
           (let ([unit1 (edge-unit1 edge)])
             (if (equal? unit1 unit)
                 (edge-unit2 edge)
                 unit1)))
         edges))

  ;; units without edges have edge count of zero
  (define (find-units-without-edges units)
    (filter (lambda (unit)
              (empty? (unit-edges unit)))
            units))

  ;; 6. Move a unit towards the input by a fraction
  (define (move-by-a-fraction! unit x epsilon)
    (let ([position (unit-position unit)])
      (set-unit-position! unit
                          (vector-process
                           + position
                           (vector-mul (vector-process - x (unit-position unit)) epsilon)))))

  ;; 7. set the age of the edge to zero, or create if edge nonexistent
  ;; (define (insert-or-update-edge unit1 unit2 edges))

  ;; 4. set the age to zero
  (define (reset-age! edge)
    (set-edge-age! edge 0))

  ;; 8. remove edges whose age is larger than a-max
  ;; returns units whose edges were removed
  (define (remove-edges! edges)
    (for ([edge (in-list edges)])
      (define unit1 (edge-unit1 edge))
      (define unit2 (edge-unit2 edge))
      (set-unit-edges! unit1 (remove edge (unit-edges unit1)))
      (set-unit-edges! unit2 (remove edge (unit-edges unit2)))))


  (define (euclidean-distance x1 x2)
    (sqrt (reduce-vector + 0 (vector-map sqr (vector-map - x1 x2)))))
  
  (define (squared-distance x1 x2)
    (reduce-vector + 0 (vector-map sqr (vector-map - x2 x1))))

  (define (squared-2d-distance x1 x2)
    (+ (sqr (- (vector-ref x2 0)
               (vector-ref x1 0)))
       (sqr (- (vector-ref x2 1)
               (vector-ref x1 1)))))

  (define (euclidean-distance-2d-points x1 x2)
    (sqrt (+ (sqr (- (vector-ref x2 0)
                  (vector-ref x1 0)))
          (sqr (- (vector-ref x2 1)
                  (vector-ref x1 1))))))

  

  ;; ;; Squared distance between two vectors
  ;; (define (euclidean-distance x1 x2)
  ;;   (sqrt (reduce-vector + 0 (vector-map sqr (vector-map - x2 x1)))))

  ;; reduce-vector: fn N vector -> N
  ;; to reduce the vector by subsequently applying the operator "fn"
  ;; starting in the initial state "start"
  (define (reduce-vector fn start vector)
    (let ([length (vector-length vector)])
      (define (reduce-vector-acc result n)
        (if (= n length)
            result
            (reduce-vector-acc (+ result (vector-ref vector n)) (+ n 1))))
      (reduce-vector-acc start 0)))

  ;; dealing with matrices
  (define (vector-remove matrix row)
    (vector-append
     (vector-take matrix row)
     (vector-drop matrix (+ row 1))))

  (define (remove-column matrix column)
    (vector-map (lambda (vector) (vector-remove vector column)) matrix))

  (define (vector-mul vec k)
    (vector-map (lambda (v-n) (* v-n k)) vec))

  (define (vector-process fn vec1 vec2)
    (vector-map fn vec1 vec2))

  (define (scale-cities cities W H)
    (map (lambda (entry)
           (vector (vector-ref entry 0)
                   (/ (* (vector-ref entry 1) W)
                      20000.0)
                   (/ (* (vector-ref entry 2) H)
                      20000.0)))
         cities))


  (define (units->centroids  units (scale-factor 1.0) (scale-max 1.0)
                             #:scale? (scale? #f))
    (for/vector ([unit (in-list units)]
                 [id (in-range 0 (length units))])
      (define posn (unit-position unit))
      (if scale? 
          (vector id
                  (/ (* (vector-ref posn 0) scale-max) scale-factor)
                  (/ (* (vector-ref posn 1) scale-max) scale-factor))
          (vector id
                  (vector-ref posn 0)
                  (vector-ref posn 1)))))

  )
