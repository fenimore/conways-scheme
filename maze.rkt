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
;;(define full-block
  ;;(square cell #:solid #:black))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structs
(struct node (visited x y img) #:transparent)
(define maze '())


(define (make-board)
  "A Vector of Vectors for the grid"
  (vector->list
   (build-vector rows
                 (lambda (m) ;; m is the y coord
                   (vector->list
                    (build-vector cols
                                  (lambda (n) ;; n is the x coord
                                    (node #f m n (empty-block)))))))))

;; depricated
(define (draw-onto-grid img x y bkg)
  "Wrapper for place-image"
  (place-image
   img
   (+ (* cell x) margin)
   (- (- (* cell y) (- (* cell cols) margin)))
   bkg))

(define (draw-node n bkg)
  (place-image
   (node-img n)
   (+ (* cell (node-x n)) margin)
   (- (- (* cell (node-y n)) (- (* cell cols) margin)))
   bkg))

(define (draw-col nodes background)
  (foldl draw-node background nodes))

(define (draw-nodes nodes)
  (foldl draw-col background nodes))

;; Step One: Loop through rws
;; In each row, loop through columns
;; For each node, call draw-onto-grid
;;(for ([i (make-board)])
;;  (for ([j i])
;    (display j)))

(define (render w)
  "game state is maze, list of nodes"
;;  (draw-node
  ;;   (vector-ref (vector-ref (make-board) 0) 0) background))
  (draw-nodes (make-board)))
  ;;(for ([row (make-board)])
  ;;(draw-nodes row background)))



(big-bang maze
          (to-draw render))
