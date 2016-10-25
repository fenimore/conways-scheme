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

(define empty-block
  (square CELL-SIZE #:outline #:black))

(define full-block
  (square CELL-SIZE #:solid #:black))


;; Rendering World
;;(define world

(struct node (visited x y img))



(define (make-board)
  (build-vector rows
                (lambda (m)
                  (build-vector cols
                                (lambda (n)
                                  (node #f m n empty-block))))))

(define (render board)
  )
