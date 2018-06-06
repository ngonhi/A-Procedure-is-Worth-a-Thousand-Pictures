#lang racket
(require gigls/unsafe)

;;; Procedure:
;;;   turtle-polygon!
;;; Parameters:
;;;   turtle, a turtle
;;;   side-length, a positive integer
;;;   sides, a posivitve integer
;;; Purpose:
;;;   Use a turtle to draw a regular polygon
;;;   with the specified number of sides,    
;;;   with each side of the specified length.
;;; Produces:
;;;   an image of a polygon, a side-effect
(define turtle-polygon!
  (lambda (turtle side-length sides)
    (repeat sides
            (lambda ()
              (turtle-forward! turtle side-length)
              (turtle-turn! turtle (/ 360 sides))))))


;;; Procedure:
;;;   square-flower
;;; Parameters:
;;;   n, a non-negtaive integer
;;;   length, a positive integer
;;;   x, a non-negative integer
;;;   y, a non-negative integer
;;;   turtle, a turtle
;;;   color, an irgb-encoded color
;;; Purpose:
;;;   Use a turtle to recursively draw a colored flower
;;;   consisted of squares with different positions and
;;;   side-lengths centered at (x, y).
;;; Produces:
;;;   an image of a flower consisted of squares, a side-effect
(define square-flower
  (lambda (n length x y turtle color)
    (let ([draw-square
           (lambda (turtle x y length color)
             (turtle-set-color! turtle color)
             (turtle-teleport! turtle (+ x (/ length (sqrt 2))) y)
             (turtle-face! turtle 135)
             (turtle-polygon! turtle length 4)
             (turtle-teleport! turtle (+ x (/ length 2)) (+ y (* length 0.5 (tan (* 22.5 (/ pi 180))))))
             (turtle-turn! turtle 22.5)
             (turtle-polygon! turtle (/ length (sqrt 2) (cos (* 22.5 (/ pi 180)))) 4)
             (turtle-teleport! turtle (+ x (/ length 2)) (+ y (/ length 2)))
             (turtle-turn! turtle 22.5)
             (turtle-polygon! turtle length 4)
             (turtle-teleport! turtle (+ x (* (/ length 2 (cos (* 22.5 (/ pi 180)))) (cos (* 67.5 (/ pi 180))))) (+ y (* (/ length 2 (cos (* 22.5 (/ pi 180)))) (sin (* 67.5 (/ pi 180))))))
             (turtle-turn! turtle 22.5)
             (turtle-polygon! turtle (/ length (sqrt 2) (cos (* 22.5 (/ pi 180)))) 4))])
      (when (> n 0)
        (draw-square turtle x y length color)
        (square-flower (- n 1) (/ length 2 (square (cos (* 22.5 (/ pi 180))))) x y turtle color)))))

;;; Procedure:
;;;   pentagon-flower
;;; Parameters:
;;;   n, a non-negtaive integer
;;;   length, a positive integer
;;;   x, a non-negative integer
;;;   y, a non-negative integer
;;;   turtle, a turtle
;;;   color, an irgb-encoded color
;;; Purpose:
;;;   Use a turtle to recursively draw a colored flower
;;;   consisted of pentagons with different positions and
;;;   side-lengths centered at (x, y).
;;; Produces:
;;;   an image of a flower consisted of pentagons, a side-effect
(define pentagon-flower
  (lambda (n turtle x y length color)
    (let ([l (* length (cos (* 54 (/ pi 180))))]
          [r (* (/ length 2))]
          [d (cos (* 18 (/ pi 180)))]
          [e (cos (* 36 (/ pi 180)))])
      (turtle-set-color! turtle color)
      (turtle-teleport! turtle x (- y r))
      (turtle-face! turtle 36)
      (let kernel ([side l]
                   [distance (sqrt (- (+ 1 (/ (sqr e) (sqr d))) (* 2 e)))]
                   [times n])
        (when (> times 0)
          (turtle-polygon! turtle side 5)
          (turtle-up! turtle)
          (turtle-forward! turtle (* r distance))
          (turtle-down! turtle)
          (turtle-turn! turtle 18)
          (kernel (* (/ e d) side) (* (/ e d) distance) (- times 1)))))))

;;; Procedure:
;;;   radial-color-blend
;;; Parameters:
;;;   width, a positive integer
;;;   height, a positive integer
;;;   radius, a positive integer
;;;   center-col, a positive integer
;;;   center-row, a positive integer
;;;   color1, an irgb-encoded color
;;;   color2, an irgb-encoded color
;;; Purpose:
;;;   Create a width-by-height image that contains a color1 to
;;;   color2 radial blend centered at (center-col, center-row).
;;; Produces:
;;;   blend, an image
(define radial-color-blend
  (lambda (width height radius center-col center-row color1 color2)
    (let ([euclidean-distance
           (lambda (col1 row1 col2 row2)
             (sqrt (+ (square (- col2 col1)) (square (- row2 row1)))))])
      (image-compute
       (lambda (col row)
         (irgb
          (if (<= (euclidean-distance col row center-col center-row) radius)
              (+ (irgb-red color1)
                 (* (/ (- (irgb-red color2) (irgb-red color1)) (- radius 1))
                    (euclidean-distance col row center-col center-row)))
              (irgb-red color2))
          (if (<= (euclidean-distance col row center-col center-row) radius)
              (+ (irgb-green color1)
                 (* (/ (- (irgb-green color2) (irgb-green color1)) (- radius 1))
                    (euclidean-distance col row center-col center-row)))
              (irgb-green color2))
          (if (<= (euclidean-distance col row center-col center-row) radius)
              (+ (irgb-blue color1)
                 (* (/ (- (irgb-blue color2) (irgb-blue color1)) (- radius 1))
                    (euclidean-distance col row center-col center-row)))
              (irgb-blue color2))))
       width height))))

;;; Procedure:
;;;   stroke-border
;;; Parameters:
;;;   image, an image
;;;   brush, a string
;;;   color, an irgb-encoded color
;;;   size, a positive integer
;;; Purpose:
;;;   Stroke the border of a rectangular image with the brush of selected color and size.
;;; Produces:
;;;   border, called for a side effect
(define stroke-border
  (lambda (image brush color size)
    (image-select-rectangle! image REPLACE 0 0 (image-width image) (image-height image))
    (context-set-brush! brush size)
    (context-set-fgcolor! color)
    (image-stroke-selection! image))) 



(define colors 
  (list
   (list (irgb 255 255 0) (irgb 255 250 250) (irgb 0 191 225))
   (list (irgb 255 127 80) (irgb 72 61 139) (irgb 230 230 250))
   (list (irgb 64 224 208) (irgb 75 0 130) (irgb 255 240 245))
   (list (irgb 255 218 185) (irgb 255 250 240) (irgb 160 82 45))
   (list (irgb 255 0 0) (irgb 255 165 0) (irgb 148 0 211))
   (list (irgb 25 25 112) (irgb 128 0 0) (irgb 240 128 128))
   (list (irgb 72 209 204) (irgb 240 248 255) (irgb 255 140 0))
   (list (irgb 0 0 0) (irgb 220 220 220) (irgb 169 169 169))
   (list (irgb 219 112 147) (irgb 255 228 225) (irgb 128 0 0))
   (list (irgb 230 230 250) (irgb 186 85 211) (irgb 0 255 255))))


(define brushes
  (list  "1. Pixel"
  "2. Block 01"
  "2. Block 02"
  "2. Block 03"
  "2. Hardness 025"
  "2. Hardness 050"
  "2. Hardness 075"
  "2. Hardness 100"
  "2. Star"
  "2. Star"))

(define num->id
  (lambda (num)
    (let ([idplus (string-append "00" (number->string num))])
      (substring idplus (- (string-length idplus) 3)))))

(define image-series-file
  (lambda (prefix n width height)
    (let ([filename (string-append prefix 
                                   "." (num->id n)
                                   "." (number->string width)
                                   "x" (number->string height)
                                   ".png")]
          [image (image-series n width height)])
      (image-save image filename)
      (gimp-image-delete image))))

;;; Procedure:
;;;   paste-and-scale!
;;; Parameters:
;;;   target, an image id
;;;   left, an integer
;;;   top, an integer
;;;   width, a positive integer
;;;   height, a positive integer
;;; Purpose:
;;;   Paste whatever has been copied into the specified region,
;;;   flattening the image afterwards.
;;; Returns:
;;;   target, the same image id
(define paste-and-scale!
  (lambda (target left top width height)
    (image-select-rectangle! target REPLACE left top width height)
    (let ([pasted (car (gimp-edit-paste (image-get-layer target) 1))])
      (image-select-nothing! target)
      (gimp-layer-scale pasted width height 1)
      (gimp-image-flatten target)
      target)))

;;; Procedure:
;;;   grid!
;;; Parameters:
;;;   image, an image id
;;; Purpose:
;;;   Produce grid-image that takes image as input and
;;;   updates image to create a two-by-two grid
;;; Produces:
;;;   grid-image, an image id
(define grid!
  (lambda (image)
   (let ([w (/ (image-width image) 2)]
         [h (/ (image-height image) 2)])
    (image-select-all! image)
    (gimp-edit-copy-visible image)
    (map paste-and-scale!
         (make-list 4 image)
         (list 0 w 0 w)
         (list 0 0 h h)
         (make-list 4 w)
         (make-list 4 h))
     (context-update-displays!))))
         

;;; Procedure:
;;;   image-series
;;; Parameters:
;;;   n, a non-negative integer
;;;   width, a positive integer
;;;   height, a positive integer
;;; Purpose:
;;;   Create more than 1000 distinctly artistic images
;;; Produces:
;;;   flower, an image ID
;;; Preconditions:
;;;   n < 1000
;;; Postcondtions:
;;;   When n is an odd number, the image is a 2x2 grid, otherwise it's not a grid.
;;;   The units-digit alters 9 brushes being used.
;;;   The tens-digit alters 2 different types of flowers (square flower and pentagon flower)
;;;    When the tens-digit is smaller than 5, we have a square flower with different times of recursion
;;;     which is (+ 2 tens-digit). 
;;;    Otherwise, we have a square flower with different times of recursion
;;;     which is (+ 2 (* 2 tens-digit)).
;;;   The hundreds-digit alters the coloring of the background, the flower and the border of the image.
;;;     Each hundreds-digit, from 0 to 9, is assigned a set of three colors.
(define image-series
  (lambda (n width height)
    (let* ([n1 (floor (/ n 100))]
           [n2 (floor (/ (- n (* n1 100)) 10))]
           [n3 (- n (* n1 100) (* n2 10))]
           [length (min width height)]
           [x (/ width 2)]
           [y (/ height 2)]
           [image (radial-color-blend width height (/ length 2) x y (car (list-ref colors n1)) (cadr (list-ref colors n1)))]
           [tommy (turtle-new image)])
      (map stroke-border
           (make-list 10 image)
           (make-list 10 (list-ref brushes n3))
           (make-list 10 (caddr (list-ref colors n1)))
           (make-list 10 (/ length 10)))
      (cond [(< n2 5) (square-flower (+ 2 n2) (/ length (sqrt 3)) x y tommy (caddr (list-ref colors n1)))]
            [else (pentagon-flower (+ 2 (* 2 n2)) tommy x y (/ length (sqrt 2)) (caddr (list-ref colors n1)))])
      (cond [(even? n) image]
            [else (grid! image) image]))))