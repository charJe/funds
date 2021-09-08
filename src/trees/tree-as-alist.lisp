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

(defgeneric tree-as-alist (tree)
  (:documentation
   "An association list containing the key-value pairs in the given tree."))

(defmethod tree-as-alist ((tree leaf)) nil)

(defmethod tree-as-alist ((tree binary-tree))
  (append (tree-as-alist (bt-left tree))
          (cons (cons (bt-key tree) (bt-value tree))
                (tree-as-alist (bt-right tree)))))

(defmethod tree-as-pre-order-alist ((tree leaf))
  nil)

(defmethod tree-as-pre-order-alist ((tree binary-tree))
  (cons (cons (bt-key tree) (bt-value tree))
        (append (tree-as-pre-order-alist (bt-left tree))
                (tree-as-pre-order-alist (bt-right tree)))))


