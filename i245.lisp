;;; /r/dailyprogrammer 245 [Intermediate] Ggggggg gggg Ggggg-ggggg!
;;; https://www.reddit.com/r/dailyprogrammer/comments/3x3hqa/20151216_challenge_245_intermediate_ggggggg_gggg/

(defstruct btree-node value lchild rchild weight (name (gensym)))

(defun nodes (sequence)
  (sort
   (map 'list (lambda (item)
                (make-btree-node :value item
                                 :weight (count item sequence)))
        (remove-duplicates sequence))
   #'< :key #'btree-node-weight))

(defun make-huffman-tree (nodes)
  (let ((left (car nodes))
        (right (cadr nodes)))
    (if (= (length nodes) 1)
        (car nodes)
        (make-huffman-tree
         (merge 'list (list (make-btree-node
                             :lchild left
                             :rchild right
                             :weight (+ (btree-node-weight left)
                                        (btree-node-weight right))))
                (cddr nodes)
                #'< :key #'btree-node-weight)))))

(defun codes (huffman-tree)
  (let ((codes nil))
    (labels ((f (subtree &optional prefix)
               (if (and (null (btree-node-lchild subtree))
                        (null (btree-node-rchild subtree)))
                   (push (cons (btree-node-value subtree) prefix) codes)
                   (progn
                     (f (btree-node-lchild subtree)
                        (concatenate 'string prefix "0"))
                     (f (btree-node-rchild subtree)
                        (concatenate 'string prefix "1"))))))
      (f huffman-tree)
      codes)))

(defun huffman-encode (sequence &optional codes)
  (unless codes (setf codes (codes (make-huffman-tree (nodes sequence)))))
  (values
   (apply #'concatenate 'string
          (map 'list (lambda (element) (cdr (assoc element codes)))
               sequence))
   codes))

;;; try (multiple-value-bind(str codes) (huffman-encode "hello there earthling") (huffman-decode str codes))
(defun huffman-decode (bitstring codes)
  (labels ((f (str n)
             (unless (zerop (length str))
               (let ((code? (find-if
                             (lambda (code)
                               (string-equal (subseq str 0 n) (cdr code)))
                             codes)))
                 (if code?
                     (cons (car code?) (f (subseq str n) 0))
                     (f str (1+ n)))))))
    (coerce (f bitstring 0) 'string)))

(defun translate (set1 set2 sequence)
  (map (type-of sequence)
       (lambda (e)
         (if (find e set1)
             (elt set2 (position e set1))
             e))
       sequence))

(defun tree-dot-string (tree)
  "String suitable for passing into dot for visualization of the huffman tree"
  (labels ((f (node)
             (when node
               (let* ((value (btree-node-value node))
                      (weight (btree-node-weight node))
                      (name (btree-node-name node))
                      (lnode (btree-node-lchild node))
                      (rnode (btree-node-rchild node)))
                 (format t "~%~a [label=\"value: ~s\\nweight: ~a\"]"
                         name value weight)
                 (format t "~{~%~a -> ~a~}"
                       (mapcan (lambda (child)
                                 (when child
                                   (list name (btree-node-name child))))
                               (list lnode rnode)))
                      (f lnode)
                 (f rnode)))))
    (with-output-to-string (s)
      (let ((*standard-output* s))
        (format t "~&digraph G {")
        (f tree)
        (format t "~%}~%")))))
