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

(defgeneric tree-insert (tree key value &key test order)
  (:documentation
   "A new tree similar to the given tree except that the given key and
value are now associated with one another.  If the given key is already
contained in the tree, according to the test function, then the old value
is replaced by the specified value in the returned tree.  The order function
specifies whether the given key-value pair should be inserted to the left or
right of the given tree.  The given tree is not modified."))

(defmethod tree-insert ((tree bt-leaf) key value &key test order)
  (declare (ignore test order))
  (stitch-binary-tree :key key :value value))

(defmethod tree-insert ((tree avl-leaf) key value &key test order)
  (declare (ignore test order))
  (stitch-avl-tree :key key :value value))

(defmethod tree-insert (tree key value &key (test #'eql) (order #'<))
  (if (funcall test key (bt-key tree))
      (stitch-tree tree :key key :value value :left (bt-left tree) :right (bt-right tree))
      (let* ((side (side-to-insert tree key :order order))
             (other-side (other-side side)))
        (stitch-tree tree
                     side (tree-insert (tree-child tree :side side) key value
                                       :test test
                                       :order order)
                     other-side (tree-child tree :side other-side)))))
