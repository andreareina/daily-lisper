;;; /r/dailyprogrammer Challenge #218 [Easy] Making numbers palindromic
;;; https://www.reddit.com/r/dailyprogrammer/comments/38yy9s/20150608_challenge_218_easy_making_numbers/

(defun num-reverse (n)
  (labels ((f (n accum)
             (if (zerop n)
                 accum
                 (f (truncate n 10) (+ (mod n 10) (* 10 accum))))))
    (f n 0)))

(defun palindrome-p (n)
  (let ((str (write-to-string n)))
    (equal str (reverse str))))

(defun make-palindrome (n)
  (labels ((f (n steps)
             (if (or (palindrome-p n)
                     (= 10000 steps))
                 (values n steps)
                 (f (+ n (num-reverse n)) (1+ steps)))))
    (f n 0)))

(defun do-challenge (numlist)
  (dolist (n numlist)
    (multiple-value-bind (m steps) (make-palindrome n)
        (format t "~d gets palindromic after ~d steps: ~d~%"
                n steps m))))

(defparameter *palindromes* (make-hash-table))
(loop for n from 1 to 1000
      do (push n (gethash (make-palindrome n) *palindromes*)))
(loop for key being the hash-keys of *palindromes*
        using (hash-value value)
      if (palindrome-p key)
        collect (cons key value) into palindromes
      else
        collect value into unpalindromes
      finally (return (values palindromes unpalindromes)))
