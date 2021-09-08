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

(defun parent-height (t1 t2)
  (let ((h1 (tree-height t1))
        (h2 (tree-height t2)))
    (1+ (if (> h1 h2) h1 h2))))

(defun height-difference (t1 t2)
  (- (tree-height t1) (tree-height t2)))

(defun heavier-p (tree &key side)
  (funcall (if (left-p side) #'plusp #'minusp)
           (height-difference (bt-left tree)
                              (bt-right tree))))

(defun balanced-p (t1 t2)
  (< -2 (height-difference t1 t2) 2))

(defun balance (key value left right)
  (let ((height-difference (height-difference left right)))
    (if (< -2 height-difference 2)
        (stitch-avl-tree :key key :value value :left left :right right)
        (let* ((heavy-side (if (plusp height-difference) :left :right))
               (other-side (other-side heavy-side))
               (inside (if (left-p heavy-side) right left))
               (outside (if (left-p heavy-side) left right)))
          (rotate inside key value
                  (if (heavier-p outside :side other-side)
                      (rotate (tree-child outside :side heavy-side)
                              (bt-key outside) (bt-value outside)
                              (tree-child outside :side other-side)
                              :side heavy-side)
                      outside)
                  :side other-side)))))

(defun rotate (inside root-key root-value outside &key side)
  (let* ((t1 (tree-child outside :side side))
         (new-inside (stitch-avl-tree :key root-key :value root-value
                                      side inside
                                      (other-side side) t1))
         (new-outside (tree-child outside :side (other-side side))))
    (stitch-avl-tree :root outside
                     side new-inside
                     (other-side side) new-outside)))
