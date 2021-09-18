(in-package :funds)

(defun make-array (size &key (initial-contents nil) (initial-element nil))
  "A functional array of the given size with the given initial contents."
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

(defun array-ref (array index)
  "The element at the given index of the given array."
  (tree-find array index :test #'=))

(defun array-set (array index element)
  "An array similar to the given array except that index and element are
associated in the returned array."
  (tree-insert array index element :test #'=))

(defun array-size (array)
  "The number of elements in the given array."
  (labels ((f (tree amount)
             (if (tree-empty-p tree)
                 amount
                 (f (bt-right tree) (1+ (bt-key tree))))))
    (f array 0)))

(defun array-count (item array &key (key #'identity) (test #'eql))
  "The number of elements in the given array that satisfy the test."
  (tree-count
   item array
   :key (lambda (tree)
          (funcall key (bt-value tree)))
   :test test))

(defun array-count-if (pred array &key (key #'identity))
  "The number of elements in the given array that satisfy the test."
  (tree-count-if
   pred array
   :key (lambda (tree)
          (funcall key (bt-value tree)))))

(defun map-array (function array)
  "A new array whose elements are the results of the application
of the given function to the elements of the given array."
  (map-tree
   (lambda (tree)
     (funcall function (bt-value tree)))
   array))

(defun array-as-list (array)
  (mapcar #'cdr (tree-as-alist array)))

(defun array-from-list (list)
  (make-array (length list) :initial-contents list))

(defun array-copy (array &optional start end)
  (if start
      (array-from-list (subseq (array-as-list array) start end))
      (array-from-list (array-as-list array))))
