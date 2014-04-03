#lang racket
(require gigls/unsafe)

;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; 
;;;;   I dug up this file for use as an additional GitHub file.  ;;;; 
;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; ;;;; 


;;; Procedure:
;;;   cartesian->polar-radius
;;; Parameters:
;;;   x, a number
;;;   y, a number
;;; Purpose:
;;;   Convert cartesian coordinates to a polar coordinate radius
;;; Produces:
;;;   radius, a number
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   radius = sqrt( x^2 + y^2)
;;;   radius >= 0
(define cartesian->polar-radius
  (lambda (x y)
    (sqrt (+ (square x) (square y)))))

;;; Procedure:
;;;   cartesian->polar-angle
;;; Parameters:
;;;   x, a number
;;;   y, a number
;;; Purpose:
;;;   Convert cartesian coordinates to a polar coordinate angle
;;; Produces:
;;;   angle, a number
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   -pi <= angle <= pi
;;;   If x is 0 and y is positive, y/x is undefined.  However, trigonometry
;;;     suggests that this "degenerate triangle" has an angle of pi/2, and
;;;     so this procedure returns pi/2.
;;;   If x is 0 and y is negative, y/x is undefined.  However, this 
;;;     procedure returns -pi/2.
;;;   If x and y are both 0, the angle is undefined.  However, 
;;;     this procedure returns 0.
(define cartesian->polar-angle
  (lambda (x y)
    (if (and (zero? x) (zero? y))
        0
        (atan y x))))


;;; Procedure:
;;;   polar-rose
;;; Parameters:
;;;   petals, an integer
;;;   size, an integer
;;; Purpose:
;;;   Compute an image of a flower using polar coordinates
;;; Produces:
;;;   image, an image
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (image-height image) = (image-width image) = size
;;;   image contains a visualization of the polar rose
(define polar-rose
  (lambda (petals size)
    (let ([x-center (/ size 2)]
          [y-center (/ size 2)])
      (image-compute
       (lambda (col row)
         (let* ([x (- col x-center)]
                [y (- row y-center)]
                [radius (cartesian->polar-radius x y)]
                [scaled-radius (* 4 (/ radius size))]
                [angle (cartesian->polar-angle x y)])
           (if (<= scaled-radius (+ 1 (sin (* petals angle))))
               (rgb-new 255 0 0)
               (rgb-new 0 0 0))))
       size size))))

;;; Bent Banana
(define bent-banana
  (lambda (color size)
    (let ([x-center (/ size 2)]
          [y-center (/ size 2)])
      (image-compute
       (lambda (col row)
         (let* ([x (- col x-center)]
                [y (- row y-center)]
                [radius (cartesian->polar-radius x y)]
                [scaled-radius (* 4 (/ radius size))]
                [angle (cartesian->polar-angle x y)])
           (if (<= scaled-radius 
                   (+ (cos angle) 
                      (* (* 4 (cos angle)) (expt (sin angle) 2))))
               color
               (rgb-new 0 0 0))))
       size size))))

;;; Polar fun!
(define polar-shape
  (lambda (polar-fun color size)
    (let ([x-center (/ size 2)]
          [y-center (/ size 2)])
      (image-compute
       (lambda (col row)
         (let* ([x (- col x-center)]
                [y (- row y-center)]
                [radius (cartesian->polar-radius x y)]
                [scaled-radius (* 4 (/ radius size))]
                [angle (cartesian->polar-angle x y)])
           (if (<= scaled-radius (polar-fun angle))
               color
               (rgb-new 0 0 0))))
       size size))))


;;; Polar rose blend

;;; Color only exists within the petals of the polar rose.
;;; The red component increases at a greater rate within a circle that
;;; is half the diameter of the rose.
;;; When column > row, the green component decreases while the blue
;;; component increases, and when column > row, the green component 
;;; increases while the blue component decreases. 
(define polar-rose-blend
  (lambda (petals size)
    (let ([x-center (/ size 2)]
          [y-center (/ size 2)])
      (image-compute
       (lambda (col row)
         (let* ([x (- col x-center)]
                [y (- row y-center)]
                [radius (cartesian->polar-radius x y)]
                [scaled-radius (* 4 (/ radius size))]
                [angle (cartesian->polar-angle x y)])
           (if (<= scaled-radius (+ 1 (sin (* petals angle))))
               (rgb-new  
                (if (<= radius (/ size 4))
                    (* col 2)
                    (* row 1))   
                (if (> col row)
                    (- 255 (* col 2))
                    (* col 2))
                (if (> row col)
                    (- 255 (* row 2))
                    (* row 2)))
               (rgb-new 0 0 0))))
       size size))))

;;; Procedure
;;;    polar-blend-kappa
;;; Parameters
;;;    size, an integer
;;; Purpose
;;;    Produces a creatively-colored, square drawing of a kappa curve
;;;    with width and height being defined by size. 
;;; Produces
;;;    image, an image
;;; Preconditions
;;;    size is an integer 
;;; Postconditions
;;;    drawing-height = size
;;;    drawing-width = size


(define polar-blend-kappa
  (lambda (size)
    (let ([x-center (/ size 2)]
          [y-center (/ size 2)])
      (image-compute
       (lambda (col row)
         (let* ([x (- col x-center)]
                [y (- row y-center)]
                [radius (cartesian->polar-radius x y)]
                [scaled-radius (* 4 (/ radius size))]
                [angle (cartesian->polar-angle x y)])
           (if (<= scaled-radius (* 1 (/ 1 (tan (+ .1 angle)))))
               ;;; The equation for this shape (the kappa curve) was taken from 
               ;;; http://www-groups.dcs.st-and.ac.uk/~history/Java/Kappa.html
               (rgb-new  
                (if (<= radius (/ size 5))
                    (* col 2)
                    (if (<= radius (/ size 3))
                        (* row (/ size 3))
                        (* col 1)))   
                (if (> col (/ 2 size))
                    (- 255 (* col 4))
                    (* col 1.5))
                (if (<= radius (/ size 6))
                    (- 255 (* row (/ size 5)))
                    (* row (/ size 5))))
               (rgb-new 0 0 0))))
       size size))))

;;; Procedure
;;;    polar-blend-limacon
;;; Parameters
;;;    size, an integer
;;; Purpose
;;;    Produces a creatively-colored, square drawing of a limacon
;;;    with width and height being defined by size. 
;;; Produces
;;;    image, an image
;;; Preconditions
;;;    size is an integer 
;;; Postconditions
;;;    drawing-height = size
;;;    drawing-width = size


(define polar-blend-limacon
  (lambda (size)
    (let ([x-center (/ size 2)]
          [y-center (/ size 2)])
      (image-compute
       (lambda (col row)
         (let* ([x (- col x-center)]
                [y (- row y-center)]
                [radius (cartesian->polar-radius x y)]
                [scaled-radius (* 4 (/ radius size))]
                [angle (cartesian->polar-angle x y)])
           (if (<= scaled-radius (+ 1 (* 3 (cos angle))))
               ;;; The equation for this shape (the limacon) was taken from 
               ;;; http://www-groups.dcs.st-and.ac.uk/~history/Java/Limacon.html
               (rgb-new  
                (if (<= radius (/ size 4))
                    (* col 2)
                    (- (* row 2) 255))   
                (if (<= radius (/ size 6))
                    (* col (/ size 100))
                    (* col (/ size 200)))
                (- 255 (* row 2)))
               (rgb-new 0 0 0))))
       size size))))

(image-show (polar-blend-kappa 200))
(image-show (polar-blend-limacon 400))
