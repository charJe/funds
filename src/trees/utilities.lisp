(in-package :funds)

(defun tree-count (item tree &key (key #'identity) (test #'eql))
  "The number of sub-trees in the tree that satisfy the test."
  (tree-count-if (lambda (x) (funcall test x item))
                 tree
                 :key key))

(defun tree-count-if (predicate tree &key (key #'identity))
  "The number of sub-trees in the given tree that satisfy the test."
  (if (tree-empty-p tree)
      0
      (+ (tree-count-if predicate (bt-left tree) :key key)
         (if (funcall predicate (funcall key tree))
             1
             0)
         (tree-count-if predicate (bt-right tree) :key key))))

(defun map-tree (function tree)
  "A tree each node of which corresponds to the application of
function to one node of the given tree."
  (if (tree-empty-p tree)
      tree
      (stitch-tree tree
                   :value (funcall function tree)
                   :left (map-tree function (bt-left tree))
                   :right (map-tree function (bt-right tree)))))
