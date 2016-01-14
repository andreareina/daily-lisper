;;; /r/dailyprogrammer Challenge 247 [Intermediate] Moving (diagonally) Up in Life
;;; https://www.reddit.com/r/dailyprogrammer/comments/3ysdm2/20151230_challenge_247_intermediate_moving/?ref=search_posts

(defun point<= (point &rest points)
  "True if all points are in monotonically increasing order"
  (and (apply #'<= (mapcar #'car (cons point points)))
       (apply #'<= (mapcar #'cdr (cons point points)))))

(defun point- (a b)
  "Subtract two points"
  (cons (- (car a) (car b))
        (- (cdr a) (cdr b))))

(let ((paths (make-hash-table :test 'equal)))
  (defun delannoy (x y)
    "Number of paths from (0, 0) to (x, y) stepping only N, E, or NE"
    (if (some #'zerop (list x y))
        1
        (or (gethash (cons x y) paths)
            (setf (gethash (cons x y) paths)
                  (+ (delannoy (1- x) y)
                     (delannoy x (1- y))
                     (delannoy (1- x) (1- y))))))))

(defun iterated-delannoy (&rest points)
  "Number of Delannoy paths that go through POINTS in order"
  (and (apply #'point<= points)
       (apply #'*
              (loop for point-a = (car points) then (car more-points)
                    for more-points = (cdr points) then (cdr more-points)
                    for point-b = (car more-points) then (car more-points)
                    while point-b
                    collect (delannoy (- (car point-b) (car point-a))
                                      (- (cdr point-b) (cdr point-a)))))))

(defun points (string)
  "List of points from a lattice with waypoints such as
.........X
..........
....X.....
..........
..........
....X.....
..........
.X........
..........
X........."
  (let ((lines (with-open-stream (s (make-string-input-stream string))
                 (loop for line = (read-line s nil nil)
                       while line collect line))))
    (loop for line in (nreverse lines)
          for y from 0
          append (loop for c across line
                       for x from 0
                       when (char= #\X c)
                         collect (cons x y)))))
