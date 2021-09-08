(in-package :funds)

(defclass heap-leaf (leaf)
  ()
  (:documentation "A leaf node of a heap."))

(defvar +heap-leaf+ (make-instance 'heap-leaf))

(defun make-heap-leaf ()
  +heap-leaf+)

(defclass heap (binary-tree)
  ((key :initarg :priority :reader heap-priority)
   (left :initform (make-heap))
   (right :initform (make-heap))
   (weight :initarg :weight :initform 1 :reader heap-weight)))

(defun make-heap (&key (priority 0 p-p) value
                    (left (make-heap-leaf)) (right (make-heap-leaf)))
  "An empty binary heap."
  (if p-p
      (make-instance
       'heap
       :priority priority
       :value value
       :left left
       :right right
       :weight (+ 1 (tree-weight left) (tree-weight right)))
      (make-heap-leaf)))

(defun attach-heap (root &key left right)
  (make-heap
   :priority (heap-priority root)
   :value (bt-value root)
   :left left
   :right right))

(defmethod tree-weight ((tree heap))
  (heap-weight tree))

(defmethod stitch-tree ((tree heap) &key (key (bt-key tree)) (value (bt-value tree)) left right)
  (make-heap
   :priority key
   :value value
   :left left
   :right right))
