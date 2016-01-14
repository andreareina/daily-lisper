;;; /r/dailyprogrammer Challenge 243 [Intermediate] Jenny's Fruit Basket
;;; https://www.reddit.com/r/dailyprogrammer/comments/3v4zsf/20151202_challenge_243_intermediate_jennys_fruit/

(defvar *sample-input*
  '(("banana" . 32)
    ("kiwi" . 41)
    ("mango" . 97)
    ("papaya" . 254)
    ("pineapple" . 399)))

(defvar *challenge-input*
  '(("apple" . 59)
    ("banana" . 32)
    ("coconut" . 155)
    ("grapefruit" . 128)
    ("jackfruit" . 1100)
    ("kiwi" . 41)
    ("lemon" . 70)
    ("mango" . 97)
    ("orange" . 73)
    ("papaya" . 254)
    ("pear" . 37)
    ("pineapple" . 399)
    ("watermelon" . 500)))

(defun cost (pricelist fruit)
  (cdr (find fruit pricelist :key #'car :test #'string-equal)))

(defun basket-cost (pricelist basket)
  (reduce #'+ (mapcar (lambda (fruit) (cost pricelist fruit)) basket)))

(defun buy-fruits (pricelist cash)
  "All combinations of fruits that total to CASH.
Produces duplicates"
  (let ((results nil)
        (fruits (mapcar #'car pricelist)))
    (labels
        ((f (partial)
           (cond
             ((= (basket-cost pricelist partial) cash)
              (push partial results))
             ((< (basket-cost pricelist partial) cash)
              (mapcar #'f (mapcar (lambda (fruit) (cons fruit partial))
                                  fruits))))))
      (f nil)
      results)))

(defun buy-fruits (pricelist cash)
  "All combination of fruits (with repetition) that total to CASH"
  (let ((results nil)
        (fruits (mapcar #'car pricelist)))
    (labels
        ((f (partial fruits)
           (cond
             ((= (basket-cost pricelist partial) cash)
              (push partial results))
             ((< (basket-cost pricelist partial) cash)
              (mapcar (lambda (fruit)
                        (f (cons fruit partial) (member fruit fruits
                                                        :test #'string-equal)))
                      fruits)))))
      (f nil fruits)
      results)))

(defun basket-quantities (basket)
  "<list of fruits with repetition> -> ((fruit . qty)*)"
  (let ((fruits (remove-duplicates basket :test #'string-equal)))
    (mapcar (lambda (fruit) (cons fruit
                                  (count fruit basket :test #'string-equal)))
            fruits)))

(defun pretty-print-basket-quantities (basket-quantities)
  (let* ((fruits (mapcar #'car basket-quantities))
         (quantities (mapcar #'cdr basket-quantities))
         (format-string
           (format nil "~{~~a ~a~~:p~^, ~}~%" fruits))
         (format-args (append (list t format-string) quantities)))
    (apply #'format format-args)))
