(module plotting racket/gui
  (require "gng-utilities.rkt")
  ;;; Plotting the progress
  (provide plot-nodes)
  (define (plot-nodes nodes gng-dc w h data-w data-h
                      #:node-size (node-size 8))
    ;; draw edges
    (send gng-dc set-pen black-pen)
    (andmap (lambda (edge)
              (let ([posn1 (node-position (edge-node1 edge))]
                    [posn2 (node-position (edge-node2 edge))])
                (let ([x1 (/ (* w (vector-ref posn1 0)) data-w)]
                      [y1 (/ (* h (vector-ref posn1 1)) data-h)]
                      [x2 (/ (* w (vector-ref posn2 0)) data-w)]
                      [y2 (/ (* h (vector-ref posn2 1)) data-h)])
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
                      (- (/ (* w (vector-ref posn 0)) data-w) (/ node-size 2.0))
                      (- (/ (* h (vector-ref posn 1)) data-h) (/ node-size 2.0))
                      node-size node-size)))
            nodes))

  (provide plot-data-point)
  (define (plot-data-point data gng-dc w h data-w data-h)
    (send gng-dc set-brush yellow-brush)
    (send gng-dc set-pen yellow-pen)
    (send gng-dc draw-ellipse
          (/ (* (vector-ref data 0) w) data-w)
          (/ (* (vector-ref data 1) h) data-h)
          20 20)
    (send gng-dc set-brush blue-brush)
    (send gng-dc set-pen black-pen))

  
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


  (provide plot-gng)
  (define (plot-gng name W H)
    ;; Make a 300 x 300 frame
    (define frame (new frame% [label name]
                       [width W]
                       [height H]))
    ;; Make the drawing area
    (define canvas (new canvas%
                        [parent frame]
                        [min-width W]
                        [min-height H]))
    ;; Get the canvas's drawing context
    (define gng-dc (send canvas get-dc))
    (send gng-dc set-pen blue-pen)
    (send gng-dc set-brush blue-brush)
    ;; Show the frame
    (send frame show #t)
    ;; Wait a second to let the window get ready
    (sleep/yield 1)
    gng-dc)

  )
