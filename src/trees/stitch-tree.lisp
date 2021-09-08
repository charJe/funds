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

(defmethod stitch-tree ((tree binary-tree) &key (key (bt-key tree)) (value (bt-value tree)) left right)
  (stitch-binary-tree :key key :value value :left left :right right))

(defmethod stitch-tree ((tree avl-tree) &key (key (bt-key tree)) (value (bt-value tree)) left right)
  (balance key value left right))

(defun stitch-avl-tree (&key root (key (bt-key root)) (value (bt-value root))(left (make-avl-leaf)) (right (make-avl-leaf)))
  (make-instance
   'avl-tree
   :key key :value value
   :left left :right right
   :height (parent-height left right)))

(defun stitch-binary-tree (&key root (key (bt-key root)) (value (bt-value root)) (left (make-binary-tree)) (right (make-binary-tree)))
  (make-instance
   'binary-tree
   :key  key
   :value value
   :left left
   :right right))
