;;;;
;;;; Copyright 2007 Andrew Baine
;;;;
;;;; Licensed under the Apache License, Version 2.0 (the "License");
;;;; you may not use this file except in compliance with the License.
;;;; You may obtain a copy of the License at
;;;;
;;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing, software
;;;; distributed under the License is distributed on an "AS IS" BASIS,
;;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;; See the License for the specific language governing permissions and
;;;; limitations under the License.
;;;;

(in-package :funds)

(defgeneric tree-remove (tree key &key test order)
  (:documentation
   "A new tree with the given key removed.  The test function is used to compare
the tree's key to the given key.  The order function is used to determine
whether the key can be found to the left or right if it is not found at the
root."))

(defmethod tree-remove ((tree leaf) key &key test order)
  (declare (ignore test order key))
  tree)

(defmethod tree-remove ((tree binary-tree) key &key (test #'eql) (order #'<))
  (if (funcall test key (bt-key tree))
      (remove-root tree :order order :test test)
      (let* ((side (if (funcall order key (bt-key tree))
                       :left
                       :right))
             (other-side (other-side side)))
        (stitch-tree tree
                     side (tree-remove (tree-child tree :side side) key
                                       :test test
                                       :order order)
                     other-side (tree-child tree :side other-side)))))

(defun remove-root (tree &key test order)
  (cond ((tree-empty-p (bt-left tree))
         (bt-right tree))
        ((tree-empty-p (bt-right tree))
         (bt-left tree))
        (t (remove-root-with-children tree :test test :order order))))

(defun remove-root-with-children (tree &key test order)
  (let* ((next (next-in-order tree)))
    (stitch-tree next
                 :left (bt-left tree)
                 :right (tree-remove (bt-right tree) (bt-key next)
                                     :test test :order order))))
