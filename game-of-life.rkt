#lang racket
(require 2htdp/universe 2htdp/image)


;;;;;;;;;
;; Drawing Definitions
(define rows 5)     ; cells
(define cols 5)
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
          (node #f n m dead-block))))))))

(define state (make-state))
;;(define state-copy '())

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
  ;;(set! state-copy nodes)
  "Draws a List of List node grid"
  (foldl draw-col background nodes))


(define (apply-rules n old-state)
  "Returns true or false for state"
  (let ([neighbors (tally-neighbors n old-state)])
    (cond
      [(< neighbors 2) #f]
      [(and
        (node-alive n)
        ;;(= neighbors 3)) #t]
        (or (= neighbors 2) (= neighbors 3))) #t]
      [(and (node-alive n) (> neighbors 3)) #f]
      [(and (not (node-alive n)) (= neighbors 3)) #t]
      [else #f])))


(define (new-state old-state)
  "A Vector of Vectors for the grid in Lists"
  (vector->list
   (build-vector
    rows
    (lambda (m) ;; m is the y coord
      (vector->list
       (build-vector
        cols
        (lambda (n) ;; n is the x coord
          (node
           (apply-rules
            (list-ref
             (list-ref old-state m) n)
            old-state)
           n m '()))))))))

(define (render w)
  "Render Takes in World state and returns state"
  ;;(set! w (new-state w))
  (draw-nodes (new-state w)))
;;  (draw-nodes (new-state (new-state w))))
;;  (draw-nodes w))

(define (tick w)
  "Draw the new state of the world"
  (new-state w))
;;  (draw-nodes (new-state w)))

(define (press w key)
  (new-state w))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Functionality
;; TODO: Select seed pattern




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Seed Positions


;; Rules:
;; 1. Fewer than two neighbors dies
;; 2. Two or three neighbors lives if living
;; 3. Alive cells with > 3 n dies
;; 4. Dead cell with exactly 3 neighbors reproduces

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Neighbor Functions
;; Outer List is X and inner Y
(define (tally-neighbors n old-state)
  (length
   (filter
    identity
    (list
     (check-above (node-x n) (node-y n) old-state)
     (check-below (node-x n) (node-y n) old-state)
     (check-right (node-x n) (node-y n) old-state)
     (check-left (node-x n) (node-y n) old-state)
     (check-right-above (node-x n) (node-y n) old-state)
     (check-right-below (node-x n) (node-y n) old-state)
     (check-left-above (node-x n) (node-y n) old-state)
     (check-left-below (node-x n) (node-y n) old-state)))))

(define (check-above x y state)
  "Check if node above is alive"
  (if (= y (- rows 1)) #f
      (let ([n (list-ref (list-ref state (+ y 1)) x)])
        (if (node-alive n) #t #f))))
(define (check-below x y state)
  "Check if node below is alive"
  (if (= y 0) #f
      (let ([n (list-ref (list-ref state (- y 1)) x)])
        (if (node-alive n) #t #f))))
(define (check-right x y state)
  "Check if node right is alive"
  (if (= x (- cols 1)) #f
      (let ([n (list-ref (list-ref state y) (+ x 1))])
        (if (node-alive n) #t #f))))
(define (check-left x y state)
  "Check if node left is alive"
  (if (= x 0) #f
      (let ([n (list-ref (list-ref state y) (- x 1))])
        (if (node-alive n) #t #f))))
(define (check-right-above x y state)
  (if (or (= y (- rows 1)) (= x (- cols 1))) #f
      (let ([n (list-ref (list-ref state (+ y 1)) (+ x 1))])
        (if (node-alive n) #t #f))))
(define (check-right-below x y state)
  (if (or (= x (- cols 1)) (= y 0)) #f
      (let ([n (list-ref (list-ref state (- y 1)) (+ x 1))])
        (if (node-alive n) #t #f))))
(define (check-left-above x y state)
  (if (or (= y (- rows 1)) (= x 0)) #f
      (let ([n (list-ref (list-ref state (+ y 1)) (- x 1))])
        (if (node-alive n) #t #f))))
(define (check-left-below x y state)
  (if (or (= x 0) (= y 0)) #f
      (let ([n (list-ref (list-ref state (- y 1)) (- x 1))])
        (if (node-alive n) #t #f))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Loop

;; TODO: set Seed position
;; X is outer list
(set-node-alive!
 (list-ref
  (list-ref state 3) 2)
 #t)
(set-node-alive!
 (list-ref
  (list-ref state 2) 2)
 #t)
(set-node-alive!
 (list-ref
  (list-ref state 1) 2)
 #t)
;;(set-node-alive!
;; (list-ref
;;  (list-ref state 1) 2) #t)
;;(set-node-alive!
;; (list-ref
;;  (list-ref state 3) 2) #t)


;;(display (tally-neighbors (list-ref (list-ref state 1) 1) state))
;;(display (node-alive (list-ref (list-ref state 1) 1)))
;;(display (apply-rules (list-ref (list-ref state 1) 1) state))
;;n(newline)
;;(display (tally-neighbors (list-ref (list-ref state 0) 0) state))
;;(newline)
;;(display (tally-neighbors (list-ref (list-ref state 3) 1) state))
;;(display (tally-neighbors (list-ref (list-ref state 1) 1) state))
;;(display (tally-neighbors (list-ref (list-ref state 1) 2) state))
;;(new-state state)
(big-bang state
          (on-tick tick 1)
          (on-key press)
          (to-draw render))
