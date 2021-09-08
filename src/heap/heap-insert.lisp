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

(defgeneric heap-insert (heap value priority &key order))

(defmethod heap-insert ((heap heap-leaf) value priority &key order)
  (declare (ignore order))
  (make-heap :priority priority
             :value value))

(defmethod heap-insert (heap value priority &key (order #'<))
  "A new heap, similar to the given heap, except that the priority-value
pair is inserted into the retured heap according to the standard heap
insertion algorithm."
  (let* ((side (next-direction heap))
         (other-side (other-side side))
         (h1 (heap-insert (tree-child heap :side side) value priority
                          :order order))
         (h2 (tree-child heap :side other-side)))
    (if (funcall order (bt-key h1) (bt-key heap)) ; if we need to bubble up
        (attach-heap h1 side
                     (attach-heap
                      heap
                      :left (bt-left h1)
                      :right (bt-right h1))
                     other-side h2)
        (attach-heap heap
                     side h1
                     other-side h2))))

(defun next-direction (heap)
  (path-direction (1+ (heap-weight heap))))

(defun last-direction (heap)
  (path-direction (heap-weight heap)))

(defun path-direction (n)
  (let ((lg (floor (log n 2))))
    (if (< (- n (expt 2 lg)) (expt 2 (1- lg)))
        :left
        :right)))
