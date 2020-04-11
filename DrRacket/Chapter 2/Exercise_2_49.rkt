#lang scheme

; See http://planet.plt-scheme.org/package-source/soegaard/sicp.plt/2/1/planet-docs/sicp-manual/index.html
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define bottom-left (make-vect 0 0))
(define bottom-right (make-vect 1 0))
(define top-right (make-vect 1 1))
(define top-left (make-vect 0 1))

; a. Outline of the designated frame

(define outline (segments->painter (list (make-segment bottom-left bottom-right)
                                         (make-segment bottom-right top-right)
                                         (make-segment top-right top-left)
                                         (make-segment top-left bottom-left))))


(paint outline)

; b. X

(define X (segments->painter (list (make-segment bottom-left top-right)
                                   (make-segment bottom-right top-left))))

(paint X)

; c. Diamond

(define diamond (segments->painter (list (make-segment (make-vect 0 0.5)
                                                       (make-vect 0.5 0))
                                         (make-segment (make-vect 0.5 0)
                                                       (make-vect 1 0.5))
                                         (make-segment (make-vect 1 0.5)
                                                       (make-vect 0.5 1))
                                         (make-segment (make-vect 0.5 1)
                                                       (make-vect 0 0.5)))))

(paint diamond)

; d. The wave painter

; source: http://www.billthelizard.com/2011/10/sicp-249-defining-primitive-painters.html
(define wave-segments
 (list
  (make-segment
   (make-vect 0.006 0.840)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.006 0.635)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.403 0.646))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.354 0.492))
  (make-segment
   (make-vect 0.403 0.646)
   (make-vect 0.348 0.845))
  (make-segment
   (make-vect 0.354 0.492)
   (make-vect 0.249 0.000))
  (make-segment
   (make-vect 0.403 0.000)
   (make-vect 0.502 0.293))
  (make-segment
   (make-vect 0.502 0.293)
   (make-vect 0.602 0.000))
  (make-segment
   (make-vect 0.348 0.845)
   (make-vect 0.403 0.999))
  (make-segment
   (make-vect 0.602 0.999)
   (make-vect 0.652 0.845))
  (make-segment
   (make-vect 0.652 0.845)
   (make-vect 0.602 0.646))
  (make-segment
   (make-vect 0.602 0.646)
   (make-vect 0.751 0.646))
  (make-segment
   (make-vect 0.751 0.646)
   (make-vect 0.999 0.343))
  (make-segment
   (make-vect 0.751 0.000)
   (make-vect 0.597 0.442))
  (make-segment
   (make-vect 0.597 0.442)
   (make-vect 0.999 0.144))))

(define wave (segments->painter wave-segments))

(paint wave)


