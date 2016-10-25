#lang racket

(require 2htdp/universe 2htdp/image)


; rendering functions
; world->scene: World -> Image

(struct node (visited doors x y) #:transparent)

(define ROWS 30)     ; cells
(define COLS 30)
(define CELL-SIZE 10)
(define BACKGROUND (empty-scene (* COLS CELL-SIZE)
                                (* ROWS CELL-SIZE)))

(define-struct world (maze))

;; NODES
(define n1 (node #f 0 0 0)) ;; origin
(define n2 (node #f 0 0 10)) ;; above
(define n3 (node #f 0 10 0)) ;; right of
(define n4 (node #f 0 10 10)) ;; Diagnol above
(define maze-world ;; The GAME WORLD
  (make-world
   (list n1 n2 n3 n4)))


(define ufo (square 30 "solid" "darkmagenta"))

(define (add-3-to-state current-state)
  (+ current-state 3))


(define (draw-onto-grid img x y bkg)
  (place-image
   img
   (* CELL-SIZE x)
   (* CELL-SIZE (- y ROWS))
   bkg))

;; place-image takes image coords and background
(define (draw-maze w)
  (draw-onto-grid ufo 100 100
                  (draw-onto-grid ufo 10 10 BACKGROUND)))

;;(node-visited (third nodes))
;;(node-visited n1)
(define (render w)
  ;;
  (draw-onto-grid ufo
                  (node-x (second (world-maze maze-world)))
                  (node-y (second (world-maze maze-world)))
                  BACKGROUND))

;; RENDER
(big-bang maze-world
          ;;(on-tick add-3-to-state)
          (to-draw render))
