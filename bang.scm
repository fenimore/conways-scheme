#lang racket

(require 2htdp/universe 2htdp/image)


; rendering functions
; world->scene: World -> Image

(struct node (visited x y img) #:transparent)

(define ROWS 10)     ; cells
(define COLS 10)
(define CELL-SIZE 30)
(define MARGIN (/ CELL-SIZE 2))
(define BACKGROUND (empty-scene (* COLS CELL-SIZE)
                                (* ROWS CELL-SIZE)))

(define-struct world (maze))

(define blue-block (square 30 "solid" "blue"))
(define green-block (square 30 "solid" "green"))
(define red-block (square 30 "solid" "red"))
(define yellow-block (square 30 "solid" "yellow"))


;; NODES
(define n1 (node #f 0 0 blue-block)) ;; origin
(define n2 (node #f 0 1 green-block)) ;; above
(define n3 (node #f 1 0 red-block)) ;; right of ;; green
(define n4 (node #f 1 1 yellow-block)) ;; Diagnol above ;; blue
(define maze-world ;; The GAME WORLD
  (make-world
   (list n1 n2 n3 n4)))





(define (draw-onto-grid img x y bkg)
  (place-image
   img
   (+ (* CELL-SIZE x) MARGIN)
   (- (- (* CELL-SIZE y) (- (* CELL-SIZE COLS) MARGIN)))
   bkg))


(define (d-node n bkg)
  (draw-onto-grid
   (node-img n)
   (node-x n)
   (node-y n)
   (bkg)))

(define (d-nodes node-lst bkg)
  (if
   (= (length node-lst) 1)
   (d-node (car node-lst) bkg)
   (d-nodes (drop node-lst 1) (d-node (car node-lst) bkg))
   )
  )

(define (test-render w)
  (draw-onto-grid
   (node-img (car (world-maze w)))
   (node-x (car (world-maze w)))
   (node-y (car (world-maze w)))
   BACKGROUND)
  )

(define (render w)
  (test-nodes (world-maze w) BACKGROUND))

(define (test-nodes node-lst bkg)
  (draw-onto-grid
   (node-img (car node-lst))
   (node-x (car node-lst))
   (node-y (car node-lst))
   bkg)
  )
  ;;(draw-node (car (world-maze maze-world)) BACKGROUND))
  ;;(draw-nodes (world-maze w) BACKGROUND))

;;(node-visited (third nodes))
;;(node-visited n1)
(define (depricated-render w)
  ;;
  (draw-onto-grid
   (node-img (first (world-maze maze-world)))
   (node-x (first (world-maze maze-world)))
   (node-y (first (world-maze maze-world)))

   (draw-onto-grid
    (node-img (second (world-maze maze-world)))
    (node-x (second (world-maze maze-world)))
    (node-y (second (world-maze maze-world)))

    (draw-onto-grid
     (node-img (third (world-maze maze-world)))
     (node-x (third (world-maze maze-world)))
     (node-y (third (world-maze maze-world)))

     (draw-onto-grid
      (node-img (fourth (world-maze maze-world)))
      (node-x (fourth (world-maze maze-world)))
      (node-y (fourth (world-maze maze-world)))
      BACKGROUND)))))

;; RENDER
(big-bang maze-world
          ;;(on-tick add-3-to-state)
          (to-draw render))
