;;; Libraries
(require "plotting.rkt")
(require "gng-utilities.rkt")


;; the nodes are saved in a list
(define nodes empty)

(define (create-GNG-network dimensionality (rang 700) (data-fn random))
  ;; add 2 nodes to the nodes list
  (set! nodes (build-list 2 (lambda (n)
                              (make-node
                               (build-vector dimensionality
                                             (lambda (n) (random rang)))
                               0.0   ;; error
                               empty ;; edges
                               ))))
  (define initial-edge (make-edge (first nodes) (second nodes) 0))
  (set-node-edges! (first nodes) (list initial-edge))
  (set-node-edges! (second nodes) (list initial-edge)))


;; 9. Insert a new unit
(define (insert-new-node alpha)
  ;; 9. Find the node with maximum accumulated error and its edges
  (define highest-error-node (argmax node-error nodes))
  (define emanating-edges (node-edges highest-error-node))
  ;;(find-emanating-edges edges highest-error-node))
  (define highest-error-neighbor
    (argmax node-error (find-neighbors highest-error-node emanating-edges)))
  ;; 9. Decrease the error of node and its neighbors
  (set-node-error! highest-error-node
                   (* alpha (node-error highest-error-node)))
  (set-node-error! highest-error-neighbor
                   (* alpha (node-error highest-error-neighbor)))
  ;; 9. Insert a new node between highest and its neighbor
  (define new-node
    (make-node (vector-mul
                (vector-process +
                                (node-position highest-error-node)
                                (node-position highest-error-neighbor))
                0.5)
               ;; error value from the just-changed node
               (node-error highest-error-node)
               empty))
  ;; a new node always has 2 edges
  ;; Insert edges connecting the new unit and node, the new unit and neighbor
  ;; add the edges between the 2 nodes and the new node
  (define new-edge0 (make-edge new-node highest-error-node 0))
  (define new-edge1 (make-edge new-node highest-error-neighbor 0))
  (set-node-edges! new-node (list new-edge0 new-edge1))
  (set! nodes (cons new-node nodes))
  ;; Remove the edge between node and neighbor
  (set-node-edges! highest-error-node
                   (cons new-edge0
                         (filter (lambda (edge)
                                   (define node1 (edge-node1 edge))
                                   (define node2 (edge-node2 edge))
                                   (not (or
                                         (and (equal? node1 highest-error-node)
                                              (equal? node2 highest-error-neighbor))
                                         (and (equal? node1 highest-error-neighbor)
                                              (equal? node2 highest-error-node)))))
                                 (node-edges highest-error-node))))
  (set-node-edges! highest-error-neighbor
                   (cons new-edge1
                         (filter (lambda (edge)
                                   (define node1 (edge-node1 edge))
                                   (define node2 (edge-node2 edge))
                                   (not (or
                                         (and (equal? node1 highest-error-node)
                                              (equal? node2 highest-error-neighbor))
                                         (and (equal? node1 highest-error-neighbor)
                                              (equal? node2 highest-error-node)))))
                                 (node-edges highest-error-neighbor)))))

;; final algorithm
(define (GNG-update data)
  ;; 3. find the nearest
  (let ([epsilon-b 1.0] ;; movement fraction for the nearest
        [epsilon-n 0.01] ;; movement fraction for the neighbors of nearest
        [age-max 20] ;; delete an edge after its age is greater than age-max
        [global-error-decrease 0.995])
    (define two-nearest (find-two-nearest data nodes))
    (define nearest (car two-nearest))
    (define second-nearest (cadr two-nearest))
    ;; 4. Find all emanating edges from nearest node
    (define emanating-edges (node-edges nearest))
    ;; 4. Increment the age of all the edges
    (map increment-age! emanating-edges)
    ;; 5. Update local error
    (set-node-error! nearest (squared-distance (node-position nearest) data))
    ;; find neighbors
    (define neighbors (find-neighbors nearest emanating-edges))
    ;;(printf "neighbors ~a~n" neighbors)
    (map (lambda (node) (set-node-error! node
                                    (squared-distance (node-position node) data)))
         neighbors)
    ;; 6. Move a node towards the input by two different fractions
    (move-by-a-fraction! nearest data epsilon-b)
    (map (lambda (node) (move-by-a-fraction! node data epsilon-n)) neighbors)
    ;; 7. set the age of the edge to zero, or create if edge nonexistent
    (define edge-nearest-second-nearest (find-edge nearest
                                                   second-nearest
                                                   emanating-edges))
    (if (empty? edge-nearest-second-nearest)
        ;; create a new edge
        (let ([new-edge (edge nearest second-nearest 0)])
          ;;(increment-edge-count! new-edge);; increase edge count of the two nodes
          (set-node-edges! nearest (cons new-edge (node-edges nearest)))
          (set-node-edges! second-nearest (cons new-edge (node-edges
                                                          second-nearest))))
        ;; if edge exists, set its age to zero
        (set-edge-age! (car edge-nearest-second-nearest) 0))
    ;; 8. remove edges whose age is larger than a-max
    (remove-edges! (filter (lambda (edge) (> (edge-age edge) age-max)) emanating-edges))
    ;; 8. A node has no emanating edges, remove it
    (define nodes-to-remove (find-nodes-without-edges nodes))
    (set! nodes (remove* nodes-to-remove nodes))
    ;; 10. Decrease all error variables by multiplying them with a constant d
    (andmap (lambda (node)
              (set-node-error! node (* (node-error node)
                                       global-error-decrease)))
            nodes)
    ))

(define (run-GNG n-times data-fn gng-dc)
  (let ([node-insertion-interval 100]
        [alpha 0.0]
        [n-max 3000])
    (define (run-GNG-aux n)
      (when (< n n-times)
        (define data (data-fn))        
        (GNG-update data)
        ;; 9. Insert edge when the node-insertion-interval exceeded
        (when (zero? (remainder n 500))
          (printf "~a: ~a~n" n (length nodes)))
        (when (zero? (remainder (+ n 1) 1000))
          (send gng-dc clear)
          (plot-nodes nodes gng-dc 700 700 20000 20000 #:node-size 5)
          (plot-data-point data gng-dc 700 700 20000 20000))
        (when (and (< (length nodes) n-max)
                   (zero? (remainder n node-insertion-interval)))
          (insert-new-node alpha))
        (run-GNG-aux (+ n 1))))
    (create-GNG-network 2 20000 data-fn)
    (run-GNG-aux 1)
    (plot-nodes nodes gng-dc 700 700 20000 20000 #:node-size 30)))


(define dc (plot-gng "GNG test" 1024 768))
(send dc set-smoothing 'smoothed)


;; random data function
(define data-fn
  (lambda ()
    (define object 2)
    (define magnitude (* 10000 (random)))
    (define angle (/ (* (random 360) pi) 180))
    (define x (* (cos angle) magnitude))
    (define y (* (sin angle) magnitude))
    (define displacement 5000)
    (vector (+ (* object displacement) x)
            (+ (* object displacement) y))))

(time (run-GNG 1000000 data-fn dc))

(time (send gng-dc clear)
      (plot-nodes nodes gng-dc 700 700 20000 20000 #:node-size 0))
