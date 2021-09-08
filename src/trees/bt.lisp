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

(defun left-p (side)
  (eq side :left))

(defun other-side (side)
  (if (left-p side)
      :right
      :left))

(defun tree-child (tree &key side)
  (funcall (if (left-p side) #'bt-left #'bt-right) tree))

(defun next-in-order (tree)
  (labels ((f (tree)
         (if (tree-empty-p (bt-left tree))
         tree
         (f (bt-left tree)))))
    (f (bt-right tree))))

(defun side-to-insert (tree key &key order)
  (if (funcall order key (bt-key tree))
      :left
      :right))
