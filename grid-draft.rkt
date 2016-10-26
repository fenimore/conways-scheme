#lang racket

(require 2htdp/universe 2htdp/image)


; rendering functions
; world->scene: World -> Image

(struct node (visited x y img) #:transparent)

(define ROWS 4)     ; cells
(define COLS 4)
(define CELL-SIZE 40)
(define MARGIN (/ CELL-SIZE 2))
(define FILLING "solid")
(define BACKGROUND (empty-scene (* COLS CELL-SIZE)
                                (* ROWS CELL-SIZE)))

(define-struct world (maze))

(define blue-block (square CELL-SIZE FILLING "blue"))
(define green-block (square CELL-SIZE FILLING "green"))
(define red-block (square CELL-SIZE FILLING "red"))
(define yellow-block (square CELL-SIZE FILLING "yellow"))
(define purple-block (square CELL-SIZE FILLING "purple"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define my Nodes here
;; visited, x, y, image
(define n1 (node #f 0 0 blue-block)) ;; origin
(define n2 (node #f 0 1 green-block)) ;; above
(define n3 (node #f 1 0 red-block)) ;; right of ;; green
(define n4 (node #f 1 1 yellow-block)) ;; Diagnol above ;; blue
(define n5 (node #f 2 2 purple-block))
(define n6 (node #f 3 3 blue-block))
(define maze-world ;; The GAME WORLD
  (make-world
   (list n1 n2 n3 n4 n5 n6)))



(define (draw-onto-grid img x y bkg)
  "Wrapper for place-image"
  (place-image
   img
   (+ (* CELL-SIZE x) MARGIN)
   (- (- (* CELL-SIZE y) (- (* CELL-SIZE COLS) MARGIN)))
   bkg))

(define (draw-nodes node-lst bkg)
  "Recursively draw all nodes"
  (if
   (= (length node-lst) 1)
   (draw-onto-grid
    (node-img (car node-lst))
    (node-x (car node-lst))
    (node-y (car node-lst))
    bkg)
   (draw-nodes
    (drop node-lst 1)
    (draw-onto-grid
     (node-img (car node-lst))
     (node-x (car node-lst))
     (node-y (car node-lst))
     bkg))))


;; RENDER
(define (render w)
  (draw-nodes (world-maze w) BACKGROUND))


;; RENDER
(big-bang maze-world
          ;;(on-tick add-3-to-state)
          (to-draw render))
