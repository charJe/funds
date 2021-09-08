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

(defclass tree ()
  ()
  (:documentation "The foundation of all trees."))

(defclass leaf (tree)
  ()
  (:documentation "A leaf with no data or children."))

(defclass avl-leaf (leaf)
  ()
  (:documentation "A leaf node of an AVL tree."))

(defclass bt-leaf (leaf)
  ()
  (:documentation "A leaf node of an AVL tree."))

(defclass binary-tree (tree)
  ((key :initarg :key :reader bt-key)
   (value :initarg :value :reader bt-value)
   (left :initarg :left :reader bt-left :initform (make-binary-tree))
   (right :initarg :right :reader bt-right :initform (make-binary-tree)))
  (:documentation "A binary tree that holds a key-value pair in its root."))

(defclass avl-tree (binary-tree)
  ((height :initarg :height :reader avl-height)
   (left :initform (make-avl-tree))
   (right :initform (make-avl-tree)))
  (:documentation "A height-balanced binary tree."))
