#lang racket

(require 2htdp/universe 2htdp/image)


; rendering functions
; world->scene: World -> Image

(struct node (visited x y) #:transparent)

(define ROWS 30)     ; cells
(define COLS 30)
(define CELL-SIZE 10)
(define BACKGROUND (empty-scene (* COLS CELL-SIZE)
                                (* ROWS CELL-SIZE)))

(define-struct world (maze))

;; NODES
(define n1 (node #f 0 0)) ;; origin
(define n2 (node #f 0 1)) ;; above
(define n3 (node #f 1 0)) ;; right of ;; green
(define n4 (node #f 3 1)) ;; Diagnol above ;; blue
(define maze-world ;; The GAME WORLD
  (make-world
   (list n1 n2 n3 n4)))


(define blue-block (square 10 "solid" "blue"))
(define green-block (square 10 "solid" "green"))
(define red-block (square 10 "solid" "red"))
(define yellow-block (square 10 "solid" "yellow"))



(define (draw-onto-grid img x y bkg)
  (place-image
   img
   (+ (* CELL-SIZE x) 5)
   (- (- (* CELL-SIZE y) 295))
   bkg))


;;(node-visited (third nodes))
;;(node-visited n1)
(define (render w)
  ;;
  (draw-onto-grid
   yellow-block
   (node-x (first (world-maze maze-world)))
   (node-y (first (world-maze maze-world)))

   (draw-onto-grid
    red-block
    (node-x (second (world-maze maze-world)))
    (node-y (second (world-maze maze-world)))

    (draw-onto-grid
     green-block
     (node-x (third (world-maze maze-world)))
     (node-y (third (world-maze maze-world)))

     (draw-onto-grid
      blue-block
      (node-x (fourth (world-maze maze-world)))
      (node-y (fourth (world-maze maze-world)))
      BACKGROUND)))))

;; RENDER
(big-bang maze-world
          ;;(on-tick add-3-to-state)
          (to-draw render))
