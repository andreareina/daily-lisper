;;; /r/dailyprogrammer Challenge 215 [Intermediate] Validating sorting networks
;;; https://www.reddit.com/r/dailyprogrammer/comments/36m83a/20150520_challenge_215_intermediate_validating/

;;; An n-wire sorting network is tested with the 2^n n-sequences of 0s and 1s
;;; The n-bit sequence is encoded as an integer

(declaim (optimize (safety 0) speed))

(defstruct comparator-pair (a 0 :type integer) (b 0 :type integer))

(defun comparator (number comp-pair)
  (declare (integer number) (comparator-pair comp-pair))
  (let* ((pos-a (comparator-pair-a comp-pair))
         (pos-b (comparator-pair-b comp-pair))
         (bit-a (ldb (byte 1 pos-a) number))
         (bit-b (ldb (byte 1 pos-b) number))
         (mask-a (expt 2 pos-a))
         (mask-b (expt 2 pos-b)))
    (if (zerop (logxor bit-a bit-b))
        number
        (min number (logxor number mask-a mask-b)))))

(defun valid-sorting-network-p (bits comp-pairs)
  (declare (integer bits))
  (labels ((net-sort (number)
             (reduce #'comparator comp-pairs :initial-value number))
           (valid-p (number)
             (zerop (nth-value 1 (truncate (log (1+ (net-sort number)) 2))))))
    (loop for n from 1 below (expt 2 bits)
       always (valid-p n))))

(defun load-network-from-file (filename)
  (with-open-file (file filename)
    (loop with bits = (read file)
       repeat (read file)
       collect (make-comparator-pair :a (read file) :b (read file)) into pairs
       finally (return (cons bits (list pairs))))))

(defun valid-sorting-network-p (bits comp-pairs)
  (declare (integer bits))
  (labels ((net-sort (number)
             (reduce #'comparator comp-pairs :initial-value number))
           (valid-p (number)
             (setf number (net-sort number))
             ;; a valid number is always in the form 2^n - 1
             (zerop (logand number (1+ number)))))
    (loop for n from 1 below (expt 2 bits)
       always (valid-p n))))
