;;; Libraries
(require "gng-utilities.rkt")

  ;;; Plotting the progress
(define (plot-nodes nodes gng-dc w h data-w data-h
                    #:node-size (node-size 8))
  ;; draw edges
  (send gng-dc set-pen black-pen)
  (andmap (lambda (edge)
            (let ([posn1 (node-position (edge-node1 edge))]
                  [posn2 (node-position (edge-node2 edge))])
              (let ([x1 (/ (* w (& posn1 0)) data-w)]
                    [y1 (/ (* h (& posn1 1)) data-h)]
                    [x2 (/ (* w (& posn2 0)) data-w)]
                    [y2 (/ (* h (& posn2 1)) data-h)])
                (send gng-dc draw-line x1 y1 x2 y2))))
          (remove-duplicates (flatten (map node-edges nodes))))
  ;; draw nodes
  (send gng-dc set-brush blue-brush)
  (send gng-dc set-pen no-pen)
  (andmap (lambda (node)
            (let ([posn (node-position node)])
              (send gng-dc set-brush (make-object brush%
                                                  (make-object color%
                                                               200
                                                               200
                                                               200)
                                                  'solid))
              (send gng-dc draw-ellipse
                    (- (/ (* w (& posn 0)) data-w) (/ node-size 2.0))
                    (- (/ (* h (& posn 1)) data-h) (/ node-size 2.0))
                    node-size node-size)))
          nodes))

(define (plot-data-point data gng-dc w h data-w data-h)
  (send gng-dc set-brush yellow-brush)
  (send gng-dc set-pen yellow-pen)
  (send gng-dc draw-ellipse
        (/ (* (& data 0) w) data-w)
        (/ (* (& data 1) h) data-h)
        20 20)
  (send gng-dc set-brush blue-brush)
  (send gng-dc set-pen black-pen))

;;(send gng-dc set-smoothing 'smoothed)

(define gng-dc #f)
;; Make some pens and brushes
(define no-pen (make-object pen% "BLACK" 1 'transparent))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define blue-brush (make-object brush% "BLUE" 'solid))
(define red-brush (make-object brush% "RED" 'solid))
(define orange-brush (make-object brush% "ORANGE" 'solid))
(define yellow-brush (make-object brush% "YELLOW" 'solid))
(define red-pen (make-object pen% "RED" 2 'solid))
(define yellow-pen (make-object pen% "yellow" 2 'solid))
(define blue-pen (make-object pen% "RED" 2 'solid))
(define black-pen (make-object pen% "BLACK" 1 'solid))

(define (f)
  ;; Make a 300 x 300 frame
  (define frame (new frame% [label "Plotting GNG"]
                     [width 700]
                     [height 700]))
  ;; Make the drawing area
  (define canvas (new canvas%
                      [parent frame]
                      [min-width 700]
                      [min-height 700]))
  ;; Get the canvas's drawing context
  (set! gng-dc (send canvas get-dc))
  (send gng-dc set-pen blue-pen)
  (send gng-dc set-brush blue-brush)
  ;; Show the frame
  (send frame show #t)
  ;; Wait a second to let the window get ready
  (sleep/yield 1))


;; the nodes are saved in a list
(define nodes empty)

(define (create-GNG-network dimensionality (rang 700) (data-fn random))
  ;; add 2 nodes to the nodes list
  (set! nodes (build-list 2 (lambda (n)
                              (make-node
                               (build-vector dimensionality
                                             (lambda (n) (random rang)))
                               0.0 ;; error
                               empty ;; edges
                               ))))
  (define initial-edge (make-edge (first nodes) (second nodes) 0))
  (set-node-edges! (first nodes) (list initial-edge))
  (set-node-edges! (second nodes) (list initial-edge)))


;; 9. Insert a new unit
(define (insert-new-node alpha)
  ;; 9. Find the node with maximum accumulate error
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
    (define two-nearest (find-nearest nodes data))
    (define nearest (car two-nearest))
    (define second-nearest (cdr two-nearest))
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

(define (run-GNG n-times data-fn)
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
    (run-GNG-aux 1)))



(f)
(send gng-dc set-smoothing 'smoothed)


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

(time (run-GNG 100000 data-fn))
(time (send gng-dc clear)
      (plot-nodes nodes gng-dc 700 700 20000 20000 #:node-size 0))


