;;; /r/dailyprogrammer Challenge 214 [Hard] Chester the greedy Pomeranian
;;; https://www.reddit.com/r/dailyprogrammer/comments/3629st/20150515_challenge_214_hard_chester_the_greedy/

(defun distance-sq (a b)
  "Evaluate the square of the Euclidean distance between points A and B"
  (+ (expt (- (car a) (car b)) 2)
     (expt (- (cadr a) (cadr b)) 2)))

(defun distance (a b)
  "Evaluate the Euclidean distance between points A and B"
  (sqrt (distance-sq a b)))

(defun nearest-neighbor (pos points)
  "Find the nearest neighbor of POS in POINTS"
  (reduce (lambda (best next)
            (if (<= (distance-sq pos best) (distance-sq pos next))
                best
                next))
          points))

(defun solve (&rest points)
  "Find the total distance needed to visit each point in POINTS
using a greedy nearest-neighbor algorithm and starting at (0.5 0.5)"
  (labels ((f (pos points distance)
             (if (null points) distance
                 (let ((point (nearest-neighbor pos points)))
                   (f point (remove point points :test #'equal)
                      (+ distance (distance pos point)))))))
    (f '(0.5d0 0.5d0) points 0)))

(defun solve-file (filename)
  (let ((points nil))
    (with-open-file (file filename)
      (setf points (loop repeat (read file)
                      collect (list (read file) (read file)))))
    (apply #'solve points)))

(defstruct point (x 0.0 :type double-float) (y 0.0 :type double-float))

(defun distance-sq (a b)
  (declare (type point a b) (optimize (safety 0) speed))
  "Evaluate the square of the Euclidean distance between points A and B"
  (+ (expt (- (point-x a) (point-x b)) 2)
     (expt (- (point-y a) (point-y b)) 2)))

(defun distance (a b)
  (declare (point a b))
  "Evaluate the Euclidean distance between points A and B"
  (sqrt (distance-sq a b)))

(defun nearest-neighbor (pos points)
  (declare (optimize speed))
  "Find the nearest neighbor of POS in POINTS"
  (reduce (lambda (best next)
            (if (<= (distance-sq pos best) (distance-sq pos next))
                best
                next))
          points))

(defun solve (&rest points)
  (declare (optimize speed))
  "Find the total distance needed to visit each point in POINTS
using a greedy nearest-neighbor algorithm and starting at (0.5 0.5)"
  (labels ((f (pos points distance)
             (if (null points) distance
                 (let ((point (nearest-neighbor pos points)))
                   (f point (remove point points :test #'equal)
                      (+ distance (distance pos point)))))))
    (f (make-point :x 0.5d0 :y 0.5d0) points 0)))

(defun solve-file (filename)
  "Find the total distance needed to visit each point in POINTS
using a greedy nearest-neighbor algorithm and starting at (0.5 0.5)
File format: {<point x value> <point y value> <newline>}*"
  (let ((points nil)
        (*read-default-float-format* 'double-float))
    (with-open-file (file filename)
      (setf points (loop repeat (read file)
                      collect (make-point :x (read file) :y (read file)))))
    (apply #'solve points)))
