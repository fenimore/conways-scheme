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

(define (empty-block)
  (square cell "outline" "black"))
;;(square CELL-SIZE #:outline #:black))

;;(define full-block
  ;;(square cell #:solid #:black))

(struct node (visited x y img) #:transparent)

(define (make-board)
  (build-vector rows
                (lambda (m)
                  (build-vector cols
                                (lambda (n)
                                  (node #f m n (empty-block)))))))

(define (draw-onto-grid img x y bkg)
  "Wrapper for place-image"
  (place-image
   img
   (+ (* cell x) margin)
   (- (- (* cell y) (- (* cell cols) margin)))
   bkg))

(define (draw-node n bkg)
  (node-img n)
  (node-x n)
  (node-y n)
  bkg)

;; Step One: Loop through rows
;; In each row, loop through columns
;; For each node, call draw-onto-grid
(for ([i (make-board)])
  (for ([j i])
    (display j)
    (newline)))
