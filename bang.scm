#lang racket

(require 2htdp/universe 2htdp/image)


(define HEIGHT 300)     ; cells
(define WIDTH 300)

(define ufo (square 50 "solid" "darkmagenta"))

(define (add-3-to-state current-state)
  (+ current-state 3))


(define (draw-a-ufo-onto-an-empty-scene current-state)
  (place-image ufo (/ WIDTH 2) current-state
               (empty-scene WIDTH HEIGHT)))

;; RENDER
(big-bang 0
          (on-tick add-3-to-state)
          (to-draw draw-a-ufo-onto-an-empty-scene))
