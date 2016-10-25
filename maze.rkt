#lang racket

(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;
;; Drawing Definitions
(define rows 4)     ; cells
(define cols 4)
(define cell 40)
(define margin (/ cell 2))
(define background (empty-scene
                    (* cols cell)
                    (* rows cell)))

;;;;;;;;;;
;; Blocks
(define (empty-block)
  (square cell "outline" "black"))
;;(square CELL-SIZE #:outline #:black))
(define (full-block)
  (square cell "solid" "black"))
  ;;(square cell #:solid #:black))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structs
(struct node (visited x y img) #:transparent #:mutable)
(define-struct world (grid))
(define (make-board)
  "A Vector of Vectors for the grid"
  (vector->list
   (build-vector rows
                 (lambda (m) ;; m is the y coord
                   (vector->list
                    (build-vector cols
                                  (lambda (n) ;; n is the x coord
                                    (node #f m n (empty-block)))))))))

(define maze
  (make-world
   (make-board)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw Functions
(define (draw-node n bkg)
  "Draw a node"
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


(define (render m)
  "game state is maze, list of nodes"
  (draw-nodes (world-grid m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User input function
(define (press m key)
  (display m)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Loop
(set-node-img! (list-ref (list-ref (world-grid maze) 0) 0) (full-block))
(big-bang maze
          (to-draw render)
          (on-key press))
