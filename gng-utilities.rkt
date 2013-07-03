(module gng-utilities racket
  (provide (all-defined-out))

  ;;; Units
  ;; need a data structure to represent the units
  ;; each unit will have a position in R^n and local error
  ;; position is a vector of real values
  ;; error is a scalar value
  (define-struct node (position error edges) #:mutable)
  (define-struct edge (node1 node2 age)      #:mutable)

  ;; 3. search through the GNG list
  ;; find the two nearest nodes and return them in a list
  (define (find-two-nearest data nodes)
    (let loop ([first-min   0]
               [second-min  0]
               [first-node  null]
               [second-node null]
               [nodes       nodes])
      (if (empty? nodes)
          (list first-node second-node)
          (let* ([node (car nodes)]
                 [new-distance (euclidean-distance (node-position node) data)])
            (cond [(null? first-node)
                   (loop new-distance 0 node null (cdr nodes))]
                  [(< new-distance first-min)
                   (loop new-distance first-min node first-node (cdr nodes))]
                  [(or (null? second-node) (< new-distance second-min))
                   (loop first-min new-distance first-node node (cdr nodes))]
                  [else
                   (loop first-min second-min first-node
                         second-node (cdr nodes))])))))

  ;; finds the edge from node1 to node2
  (define (find-edge node1 node2 edges)
    (filter (lambda (edge)
              (define n1 (edge-node1 edge))
              (define n2 (edge-node2 edge))
              (or (and (equal? node1 n1)
                       (equal? node2 n2))
                  (and (equal? node2 n1)
                       (equal? node1 n2))))
            edges))

  ;; 4. increment the age of an edge
  (define (increment-age! edge)
    (set-edge-age! edge (+ (edge-age edge) 1)))

  ;; 5. Update local error
  (define (update-local-error! node error)
    (set-node-error! node (+ (node-error node) error)))

  ;; 6. Find neighbors
  ;; search the edges and return the the nodes that are not the provided node
  (define (find-neighbors node edges)
    (map (lambda (edge)
           (let ([node1 (edge-node1 edge)])
             (if (equal? node1 node)
                 (edge-node2 edge)
                 node1)))
         edges))

  ;; nodes without edges have edge count of zero
  (define (find-nodes-without-edges nodes)
    (filter (lambda (node)
              (empty? (node-edges node)))
            nodes))

  ;; 6. Move a node towards the input by a fraction
  (define (move-by-a-fraction! node x epsilon)
    (let ([position (node-position node)])
      (set-node-position! node
                          (vector-process
                           + position
                           (vector-mul (vector-process - x (node-position node)) epsilon)))))

  ;; 7. set the age of the edge to zero, or create if edge nonexistent
  ;; (define (insert-or-update-edge node1 node2 edges))

  ;; 4. set the age to zero
  (define (reset-age! edge)
    (set-edge-age! edge 0))

  ;; 8. remove edges whose age is larger than a-max
  ;; returns nodes whose edges were removed
  (define (remove-edges! edges)
    (for ([edge (in-list edges)])
      (define node1 (edge-node1 edge))
      (define node2 (edge-node2 edge))
      (set-node-edges! node1 (remove edge (node-edges node1)))
      (set-node-edges! node2 (remove edge (node-edges node2)))))


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


  (define (nodes->centroids  nodes (scale-factor 1.0) (scale-max 1.0)
                             #:scale? (scale? #f))
    (for/vector ([node (in-list nodes)]
                 [id (in-range 0 (length nodes))])
      (define posn (node-position node))
      (if scale? 
          (vector id
                  (/ (* (vector-ref posn 0) scale-max) scale-factor)
                  (/ (* (vector-ref posn 1) scale-max) scale-factor))
          (vector id
                  (vector-ref posn 0)
                  (vector-ref posn 1)))))

  )
