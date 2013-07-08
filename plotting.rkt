;; Author: Peter Samarin
(module plotting racket/gui
  (require "gng-utilities.rkt")
  
  ;;; Plotting the progress
  (provide draw-units)
  (define (draw-units units gng-dc w h data-w data-h
                      #:unit-size (unit-size 8))
    ;; draw edges
    (send gng-dc set-pen black-pen)
    (andmap (lambda (edge)
              ;; get unit 1 and unit 2
              (let ([posn1 (unit-position (edge-unit1 edge))]
                    [posn2 (unit-position (edge-unit2 edge))])
                ;; get their (2D) coordinates
                (let ([x1 (/ (* w (vector-ref posn1 0)) data-w)]
                      [y1 (/ (* h (vector-ref posn1 1)) data-h)]
                      [x2 (/ (* w (vector-ref posn2 0)) data-w)]
                      [y2 (/ (* h (vector-ref posn2 1)) data-h)])
                  (send gng-dc draw-line x1 y1 x2 y2))))
            (remove-duplicates (flatten (map unit-edges units))))
    ;; draw units
    (send gng-dc set-brush blue-brush)
    (send gng-dc set-pen no-pen)
    (send gng-dc set-brush brush-gray200)
    (for ([unit (in-list units)])
      (define posn (unit-position unit))
      (send gng-dc draw-ellipse
            (- (/ (* w (vector-ref posn 0)) data-w) (/ unit-size 2.0))
            (- (/ (* h (vector-ref posn 1)) data-h) (/ unit-size 2.0))
            unit-size unit-size)))


  (provide draw-data-point)
  (define (draw-data-point data gng-dc w h data-w data-h)
    (send gng-dc set-brush yellow-brush)
    (send gng-dc set-pen yellow-pen)
    (send gng-dc draw-ellipse
          (/ (* (vector-ref data 0) w) data-w)
          (/ (* (vector-ref data 1) h) data-h)
          20 20)
    (send gng-dc set-brush blue-brush)
    (send gng-dc set-pen black-pen))

  
  ;; Make some pens and brushes
  (define color-gray200 (make-object color% 200 200 200))
  (define brush-gray200 (make-object brush% color-gray200 'solid))
  (define brush-white   (make-object brush% "WHITE" 'solid))
  (define no-pen        (make-object pen% "BLACK" 1 'transparent))
  (define no-brush      (make-object brush% "BLACK" 'transparent))
  (define blue-brush    (make-object brush% "BLUE" 'solid))
  (define red-brush     (make-object brush% "RED" 'solid))
  (define orange-brush  (make-object brush% "ORANGE" 'solid))
  (define yellow-brush  (make-object brush% "YELLOW" 'solid))
  (define red-pen       (make-object pen% "RED" 2 'solid))
  (define yellow-pen    (make-object pen% "yellow" 2 'solid))
  (define blue-pen      (make-object pen% "RED" 2 'solid))
  (define black-pen     (make-object pen% "BLACK" 1 'solid))


  ;; Make a drawing context
  (provide make-gng-dc)
  (define (make-gng-dc name W H)
    (define frame (new frame%
                       [label name]
                       [width W]
                       [height H]))
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

  (provide gng-snapshot)
  (define (gng-snapshot dc file)
    (define-values (w h) (send dc get-size))
    (define bm (make-bitmap w h))
    (define bdc (new bitmap-dc% [bitmap bm]))
    (send bdc set-smoothing 'smoothed)
    (send bdc set-brush brush-white)
    (send bdc draw-rectangle 0 0 w h)
    (draw-units units bdc 700 700 20000 20000 #:unit-size 10)
    (send bm save-file file 'png))
  

  )
