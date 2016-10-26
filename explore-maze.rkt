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
(define empty-block
  (square cell "outline" "black"))
;;(square CELL-SIZE #:outline #:black))
(define full-block
  (square cell "solid" "darkpurple"))
(define here-block
  (overlay
   (circle (/ cell 2) "solid" "blue")
   (square cell "solid" "darkpurple")))

;;(define (here-block)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structs
(struct node (visited x y img) #:transparent #:mutable)
(struct posn (x y) #:mutable #:transparent)
(define-struct world (grid loco))
(define (make-grid)
  "A Vector of Vectors for the grid"
  (vector->list
   (build-vector
    rows
    (lambda (m) ;; m is the y coord
      (vector->list
       (build-vector
        cols
        (lambda (n) ;; n is the x coord
          (node #f m n empty-block))))))))

(define maze
  (make-world
   (make-grid)
   (posn 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw Functions
(define (draw-node n bkg)
  "Draw a node"
  (place-image
   (node-img n)
   (+ (* cell (node-x n)) margin)
   (- (- (* cell (node-y n)) (- (* cell cols) margin)))
   bkg))
(define (visit-node n)
  "Color in visited nodes"
  (if
   (node-visited n)
   (set-node-img! n full-block)
   (set-node-img! n empty-block)
   )
  )
(define (draw-col nodes background)
  "Draw a column from a list of nodes"
  (foldl draw-node background nodes))
(define (draw-nodes nodes)
  "Draws a List of List node grid"
  (foldl draw-col background nodes))
(define (fill-visited-rows nodes)
  (map visit-node nodes))
(define (fill-visited nodes)
  (map fill-visited-rows nodes))

(define (render m)
  "game state is maze, list of nodes"
  ;; Draw in Visited nodes
  (fill-visited (world-grid m))
  ;; Set User Location
  (set-node-img!
   (list-ref
    (list-ref
     (world-grid m)
     (posn-x (world-loco m)))
    (posn-y (world-loco m)))
   here-block)
  ;; Draw on Nodes
  (draw-nodes (world-grid m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User input function
(define (press m key)
  (cond
    [(key=? key "up") (move-up m)]
    [(key=? key "right") (move-right m)]
    [(key=? key "left") (move-left m)]
    [(key=? key "down") (move-down m)])

  ;;(display (world-loco m))
  ;; set the visited node to full

  (set-node-visited!
   (list-ref
    (list-ref
     (world-grid m)
     (posn-x (world-loco m)))
    (posn-y (world-loco m)))
   #t)
  ;; return the world/ maze
  m
  )

(define (move-up maze)
  (if
   (and
    (< (posn-y (world-loco maze)) (- rows 1))
    (> (posn-y (world-loco maze)) -1))
   (set-posn-y!
    (world-loco maze)
    (+ (posn-y (world-loco maze)) 1))
   "do nothing"))

(define (move-down maze)
  (if
   (and
    (< (posn-y (world-loco maze)) rows)
    (> (posn-y (world-loco maze)) 0))
   (set-posn-y!
    (world-loco maze)
    (- (posn-y (world-loco maze)) 1))
   "do nothing"))


(define (move-right maze)
  (if
   (and
    (< (posn-x (world-loco maze)) (- cols 1))
    (> (posn-x (world-loco maze)) -1))
   (set-posn-x!
    (world-loco maze)
    (+ (posn-x (world-loco maze)) 1))
   "do nothing"))

(define (move-left maze)
  (if
   (and
    (< (posn-x (world-loco maze)) cols)
    (> (posn-x (world-loco maze)) 0))
   (set-posn-x!
    (world-loco maze)
    (- (posn-x (world-loco maze)) 1))
   "do nothing"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Loop
(set-node-visited!
 (list-ref
  (list-ref (world-grid maze) 0) 0)
 #t)

(big-bang maze
          (to-draw render)
          (on-key press))
