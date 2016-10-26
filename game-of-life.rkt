#lang racket
(require 2htdp/image)
(require 2htdp/universe)


;;;;;;;;;
;; Drawing Definitions
(define rows 8)     ; cells
(define cols 8)
(define cell 40)
(define margin (/ cell 2))
(define background (empty-scene
                    (* cols cell)
                    (* rows cell)))


;;;;;;;;;;
;; Blocks
(define dead-block
  (square cell "outline" "black"))
;;(square CELL-SIZE #:outline #:black))
(define alive-block
  (square cell "solid" "darkpurple"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structs
(struct node (alive x y img) #:transparent #:mutable)
;;(define-struct world (grid))
(define (make-state)
  "A Vector of Vectors for the grid in Lists"
  (vector->list
   (build-vector
    rows
    (lambda (m) ;; m is the y coord
      (vector->list
       (build-vector
        cols
        (lambda (n) ;; n is the x coord
          (node #f m n dead-block))))))))

(define state (make-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw Functions
(define (draw-node n bkg)
  "Draw a node and set image according to status"
  (if
   (node-alive n)
   (set-node-img! n alive-block)
   (set-node-img! n dead-block)
   )
  (place-image
   (node-img n)
   (+ (* cell (node-x n)) margin)
   (- (- (* cell (node-y n)) (- (* cell cols) margin)))
   bkg))

(define (draw-col nodes background)
  "Draw a column from a list of nodes"
  (foldl draw-node background nodes))
(define (draw-nodes nodes)
  "Draws a List of List node grid"
  (foldl draw-col background nodes))

(define (render w)
  "Render Takes in World state and returns state"
  (draw-nodes w))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Functionality
;; TODO: Select seed pattern


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Neighbor Functions
;; Outer List is Y and inner X
(define (tally-neighbors n state)
  (check-above (node-x n) (node-y n) state)
  )

(define (check-above x y state)
  "Check if node above is alive"
  (let ([n (list-ref
            (list-ref state (+ x 0))
            (+ y 1))])
    (display n)
    (if
     (node-alive n)
     #t
     #f
     )
    )
  )

(define (check-below x y state)
  "Check if node below is alive"
  (let ([n (list-ref
            (list-ref state (+ x 0))
            (- y 1))])
    (display n)
    (if
     (node-alive n)
     #t
     #f
     )
    )
  )

(define (check-right x y state)
  "Check if node right is alive"
  (let ([n (list-ref
            (list-ref state (+ x 1))
            (+ y 0))])
    (display n)
    (if
     (node-alive n)
     #t
     #f
     )
    )
  )

(define (check-left x y state)
  "Check if node left is alive"
  (let ([n (list-ref
            (list-ref state (- x 1))
            (+ y 0))])
    (display n)
    (if
     (node-alive n)
     #t
     #f
     )
    )
  )

;; Diagnols

(define (check-right-above x y state)
  (let ([n (list-ref
            (list-ref state (+ x 1))
            (+ y 1))])
    (display n)
    (if
     (node-alive n)
     #t
     #f
     )
    )
  )

(define (check-right-below x y state)
  (let ([n (list-ref
            (list-ref state (+ x 1))
            (- y 1))])
    (display n)
    (if
     (node-alive n)
     #t
     #f
     )
    )
  )

(define (check-left-above x y state)
  (let ([n (list-ref
            (list-ref state (- x 1))
            (+ y 1))])
    (display n)
    (if
     (node-alive n)
     #t
     #f
     )
    )
  )

(define (check-left-below x y state)
  (let ([n (list-ref
            (list-ref state (- x 1))
            (- y 1))])
    (display n)
    (if
     (node-alive n)
     #t
     #f
     )
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Loop

;; TODO: set Seed position

(set-node-alive!
 (list-ref
  (list-ref state 4) 3)
 #t)

;;(display (tally-neighbors (list-ref (list-ref state 4) 2) state))
(big-bang state
          (to-draw render))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Seed Positions
