(in-package :funds)

(defun make-vector (size &key (initial-contents nil) (initial-element nil))
  "A functional vector of the given size with the given initial contents."
  (let ((length (length initial-contents)))
    (labels ((f (start end)
               (if (= start end)
                   (make-binary-tree)
                   (let ((midpoint (floor (+ end start) 2)))
                     (make-instance
                      'binary-tree
                      :key midpoint
                      :value (if (< start length)
                                 (elt initial-contents midpoint)
                                 initial-element)
                      :left (f start midpoint)
                      :right (f (1+ midpoint) end))))))
      (f 0 size))))

(defun vector-ref (vector index)
  "The element at the given index of the given vector."
  (tree-find vector index :test #'=))

(defun vector-set (vector index element)
  "An vector similar to the given vector except that index and element are
associated in the returned vector."
  (tree-insert vector index element :test #'=))

(defun vector-size (vector)
  "The number of elements in the given vector."
  (labels ((f (tree amount)
             (if (tree-empty-p tree)
                 amount
                 (f (bt-right tree) (1+ (bt-key tree))))))
    (f vector 0)))

(defun vector-count (item vector &key (key #'identity) (test #'eql))
  "The number of elements in the given vector that satisfy the test."
  (tree-count
   item vector
   :key (lambda (tree)
          (funcall key (bt-value tree)))
   :test test))

(defun vector-count-if (pred vector &key (key #'identity))
  "The number of elements in the given vector that satisfy the test."
  (tree-count-if
   pred vector
   :key (lambda (tree)
          (funcall key (bt-value tree)))))

(defun map-vector (function vector)
  "A new vector whose elements are the results of the application
of the given function to the elements of the given vector."
  (map-tree
   (lambda (tree)
     (funcall function (bt-value tree)))
   vector))

(defun vector-as-list (vector)
  (mapcar #'cdr (tree-as-alist vector)))

(defun vector-from-list (list)
  (make-vector (length list) :initial-contents list))

(defun vector-copy (vector &optional start end)
  (if start
      (vector-from-list (subseq (vector-as-list vector) start end))
      (vector-from-list (vector-as-list vector))))
