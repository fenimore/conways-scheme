#lang racket

(require 2htdp/image)
(require 2htdp/universe)

;;(define maze-world (maze))
(struct maze (maze-nodes))
(struct node (visited doors x y) #:transparent)
(define n1 (node #f 0 0 0)) ;; origin
(define n2 (node #f 0 0 1)) ;; above
(define n3 (node #f 0 1 0)) ;; right of
(define n4 (node #f 0 1 1)) ;; Diagnol above

(define nodes (list n1 n2 n3 n4))
(third nodes)
;;(node-visited (third nodes))
;;(node-visited n1)
;;(set-node-visited! #t)

;;(make-maze-world
;; (make-maze (gen-nodes)))

;; Draw
;;(square 50 "outline" "darkmagenta")


;;(big-bang ")
