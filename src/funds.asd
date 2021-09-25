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

(in-package :asdf-user)

(defsystem funds
  :serial t
  :components
  ((:file "package")
   (:file "stack")
   (:module trees
    :serial t
    :components
    ((:file "classes")
     (:file "constructors")
     (:file "bt")
     (:file "avl")
     (:file "stitch-tree")
     (:file "tree-as-alist")
     (:file "tree-empty-p")
     (:file "tree-insert")
     (:file "tree-remove")
     (:file "tree-find")
     (:file "tree-weight")
     (:file "tree-height")
     (:file "utilities")))
   (:module heap
    :serial t
    :components
    ((:file "heap")
     (:file "heap-empty-p")
     (:file "heap-insert")
     (:file "heap-remove")
     (:file "heap-first")))
   (:file "vector")
   (:file "hash")
   (:file "queue")))
