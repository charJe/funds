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

(defgeneric heap-remove (heap &key order)
  (:documentation "The heap that results when first value is removed
from the given heap."))

(defmethod heap-remove ((heap heap-leaf) &key order)
  (declare (ignore order))
  heap)

(defmethod heap-remove (heap &key (order #'<))
  (let ((last-node (last-node heap)))
    (if (eq last-node heap)
        (make-heap)
        (let* ((side (last-direction heap))
               (other-side (other-side side)))
          (bubble-down
           last-node
           side (clip-last (tree-child heap :side side))
           other-side (tree-child heap :side other-side)
           :order order)))))

(defun bubble-down (root &key left right order)
  (cond ((and (not (heap-empty-p left))
              (in-order-p left root :order order)
              (or (heap-empty-p right)
                  (in-order-p left right :order order)))
         (attach-heap
          left
          :left (bubble-down
                 root
                 :left (bt-left left)
                 :right (bt-right left)
                 :order order)
          :right right))
        ((and (not (heap-empty-p right))
              (in-order-p right root :order order))
         (attach-heap
          right
          :left left
          :right (bubble-down
                  root
                  :left (bt-left right)
                  :right (bt-right right)
                  :order order)))
        (t (attach-heap root :left left :right right))))

(defun in-order-p (h1 h2 &key order)
  (funcall order (heap-priority h1) (heap-priority h2)))

(defun clip-last (heap)
  "The heap that results when the last node is removed."
  (if (no-children-p heap)
      (make-heap)
      (let ((side (last-direction heap)))
        (make-heap
         side (clip-last (tree-child heap :side side))
         (other-side side)
         (tree-child heap :side (other-side side))
         :priority (heap-priority heap)
         :value (bt-value heap)))))

(defun no-children-p (heap)
  (and (heap-empty-p (bt-left heap))
       (heap-empty-p (bt-right heap))))

(defun last-node (heap)
  (if (no-children-p heap)
      heap
      (last-node (tree-child heap :side (last-direction heap)))))
