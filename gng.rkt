;;; Libraries
(require "plotting.rkt")
(require "gng-utilities.rkt")


;; the units are saved in a list
(define units empty)

(define (create-GNG-network dimensionality (rang 700) (data-fn random))
  ;; add 2 units to the units list
  (set! units (build-list 2 (lambda (n)
                              (make-unit
                               (build-vector dimensionality
                                             (lambda (n) (random rang)))
                               0.0   ;; error
                               empty ;; edges
                               ))))
  (define initial-edge (make-edge (first units) (second units) 0))
  (set-unit-edges! (first units) (list initial-edge))
  (set-unit-edges! (second units) (list initial-edge)))


;; 9. Insert a new unit
(define (insert-new-unit alpha)
  ;; 9. Find the unit with maximum accumulated error and its edges
  (define highest-error-unit (argmax unit-error units))
  (define emanating-edges (unit-edges highest-error-unit))
  (define highest-error-neighbor
    (argmax unit-error (find-neighbors highest-error-unit emanating-edges)))
  ;; 9. Decrease the error of unit and its neighbors
  (set-unit-error! highest-error-unit
                   (* alpha (unit-error highest-error-unit)))
  (set-unit-error! highest-error-neighbor
                   (* alpha (unit-error highest-error-neighbor)))
  ;; 9. Insert a new unit between highest and its neighbor
  (define new-unit
    (make-unit (vector-mul
                (vector-process +
                                (unit-position highest-error-unit)
                                (unit-position highest-error-neighbor))
                0.5)
               ;; error value from the just-changed unit
               (unit-error highest-error-unit)
               empty))
  ;; a new unit always has 2 edges
  ;; Insert edges connecting the new unit and unit, the new unit and neighbor
  ;; add the edges between the 2 units and the new unit
  (define new-edge0 (make-edge new-unit highest-error-unit 0))
  (define new-edge1 (make-edge new-unit highest-error-neighbor 0))
  (set-unit-edges! new-unit (list new-edge0 new-edge1))
  (set! units (cons new-unit units))
  ;; Remove the edge between unit and neighbor
  (set-unit-edges! highest-error-unit
                   (cons new-edge0
                         (filter (lambda (edge)
                                   (define unit1 (edge-unit1 edge))
                                   (define unit2 (edge-unit2 edge))
                                   (not (or
                                         (and (equal? unit1 highest-error-unit)
                                              (equal? unit2 highest-error-neighbor))
                                         (and (equal? unit1 highest-error-neighbor)
                                              (equal? unit2 highest-error-unit)))))
                                 (unit-edges highest-error-unit))))
  (set-unit-edges! highest-error-neighbor
                   (cons new-edge1
                         (filter (lambda (edge)
                                   (define unit1 (edge-unit1 edge))
                                   (define unit2 (edge-unit2 edge))
                                   (not (or
                                         (and (equal? unit1 highest-error-unit)
                                              (equal? unit2 highest-error-neighbor))
                                         (and (equal? unit1 highest-error-neighbor)
                                              (equal? unit2 highest-error-unit)))))
                                 (unit-edges highest-error-neighbor)))))

;; final algorithm
(define (GNG-update data)
  ;; 3. find the nearest
  (let ([epsilon-b 1.0]  ;; movement fraction for the nearest
        [epsilon-n 0.01] ;; movement fraction for the neighbors of nearest
        [age-max   20]   ;; delete an edge after its age is greater than age-max
        [global-error-decrease 0.995])
    (define two-nearest (find-two-nearest data units))
    (define nearest (car two-nearest))
    (define second-nearest (cadr two-nearest))
    ;; 4. Find all emanating edges from nearest unit
    (define emanating-edges (unit-edges nearest))
    ;; 4. Increment the age of all the edges
    (map increment-age! emanating-edges)
    ;; 5. Update local error
    (set-unit-error! nearest (squared-distance (unit-position nearest) data))
    ;; find neighbors
    (define neighbors (find-neighbors nearest emanating-edges))
    ;; (printf "neighbors ~a~n" neighbors)
    (map (lambda (unit) (set-unit-error! unit
                                    (squared-distance (unit-position unit) data)))
         neighbors)
    ;; 6. Move a unit towards the input by two different fractions
    (move-by-a-fraction! nearest data epsilon-b)
    (map (lambda (unit) (move-by-a-fraction! unit data epsilon-n)) neighbors)
    ;; 7. set the age of the edge to zero, or create if edge nonexistent
    (define edge-nearest-second-nearest (find-edge nearest
                                                   second-nearest
                                                   emanating-edges))
    (if (empty? edge-nearest-second-nearest)
        ;; create a new edge
        (let ([new-edge (edge nearest second-nearest 0)])
          ;;(increment-edge-count! new-edge);; increase edge count of the two units
          (set-unit-edges! nearest (cons new-edge (unit-edges nearest)))
          (set-unit-edges! second-nearest (cons new-edge (unit-edges
                                                          second-nearest))))
        ;; if edge exists, set its age to zero
        (set-edge-age! (car edge-nearest-second-nearest) 0))
    ;; 8. remove edges whose age is larger than a-max
    (remove-edges! (filter (lambda (edge) (> (edge-age edge) age-max)) emanating-edges))
    ;; 8. A unit has no emanating edges, remove it
    (define units-to-remove (find-units-without-edges units))
    (set! units (remove* units-to-remove units))
    ;; 10. Decrease all error variables by multiplying them with a constant d
    (andmap (lambda (unit)
              (set-unit-error! unit (* (unit-error unit)
                                       global-error-decrease)))
            units)))

(define (run-GNG n-times data-fn gng-dc)
  (let ([unit-insertion-interval 100]
        [alpha 0.3]
        [n-max 100])
    (define (run-GNG-aux n)
      (when (< n n-times)
        (define data (data-fn))        
        (GNG-update data)
        ;; 9. Insert edge when the unit-insertion-interval exceeded
        (when (zero? (remainder n 500))
          (printf "~a: ~a~n" n (length units)))
        (when (zero? (remainder (+ n 1) 1000))
          (send gng-dc clear)
          (draw-units units gng-dc 700 700 20000 20000 #:unit-size 8)
          (draw-data-point data gng-dc 700 700 20000 20000))
        (when (and (< (length units) n-max)
                   (zero? (remainder n unit-insertion-interval)))
          (insert-new-unit alpha))
        (run-GNG-aux (+ n 1))))
    (create-GNG-network 2 20000 data-fn)
    (run-GNG-aux 1)
    (draw-units units gng-dc 700 700 20000 20000 #:unit-size 10)))


(define dc (make-gng-dc "GNG test" 700 700))
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

(time (run-GNG 200000 data-fn dc))

(time (send dc clear)
      (draw-units units gng-dc 700 700 20000 20000 #:unit-size 0))
