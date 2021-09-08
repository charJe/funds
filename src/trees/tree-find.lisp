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

(defgeneric tree-find (tree key &key test order)
  (:documentation
   "The value to which the given key is mapped, or nil if this tree contains no
such key.  The second value returned indicates whether the tree contained the
key.  So

    (find t k) -> nil t

if k maps to nil.  But

    (find t k) -> nil nil
if there is no mapping for k in the tree."))

(defmethod tree-find ((tree leaf) key &key test order)
  (declare (ignore key test order))
  (values nil nil))

(defmethod tree-find (tree key &key (test #'eql) (order #'<))
  (cond ((funcall test key (bt-key tree))
         (values (bt-value tree) t))
        ((funcall order key (bt-key tree))
         (tree-find (bt-left tree) key
                    :test test :order order))
        (t (tree-find (bt-right tree) key
                      :test test :order order))))
