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

(defgeneric tree-height (tree)
  (:documentation "The height of the given tree."))

(defmethod tree-height ((tree leaf)) 0)

(defmethod tree-height ((tree binary-tree))
  (let ((a (tree-height (bt-left tree)))
        (b (tree-height (bt-right tree))))
    (1+ (if (> a b) a b))))

(defmethod tree-height ((tree avl-tree))
  (avl-height tree))
