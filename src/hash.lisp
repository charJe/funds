(in-package :funds)

(defstruct dict
  hash
  test
  tree)

(defun make-hash (&key (hash #'sxhash) (test #'eql))
  "An empty hash that hashes occording to the given hash function,
which defaults to #'sxhash and and tests according to the given test
function, which defaults to #'eql."
  (make-dict :hash hash :test test :tree (make-avl-tree)))

(defun hash-add (d k v)
  "A hash similar to the given hash except that k maps to
v in the returned hash."
  (let* ((h (funcall (dict-hash d) k))
         (old-alist (tree-find (dict-tree d) h))
         (new-alist (acons k v (remove (assoc k old-alist :test (dict-test d))
                                       old-alist))))
    (make-dict :hash (dict-hash d)
               :test (dict-test d)
               :tree (tree-insert (dict-tree d) h new-alist))))

(defun hash-remove (d k)
  "A hash similar to the given hash, except that k does
not map to any value in the returned hash."
  (let* ((h (funcall (dict-hash d) k))
         (old-alist (tree-find (dict-tree d) h))
         (new-alist (remove (assoc k old-alist :test (dict-test d))
                            old-alist)))
    (make-dict :hash (dict-hash d)
               :test (dict-test d)
               :tree (if (null new-alist)
                         (tree-remove (dict-tree d) h)
                         (tree-insert (dict-tree d) h new-alist)))))

(defun hash-ref (d k &optional default)
  "The value associated with the given key in the given hash.  A second
value is returned to indicate whether the key is associated with any value or
is not found."
  (let ((pair (assoc k (tree-find (dict-tree d) (funcall (dict-hash d) k))
                     :test (dict-test d))))
    (if (null pair)
        (values default nil)
        (values (cdr pair) t))))

(defun hash-as-alist (d)
  "An alist containing the same key-value pairs as the given hash."
  (labels ((f (tree)
             (if (tree-empty-p tree)
                 nil
                 (append (f (bt-left tree))
                         (bt-value tree)
                         (f (bt-right tree))))))
    (f (dict-tree d))))

(defun hash-from-alist (alist &key (test #'eql) (hash #'sxhash))
  (reduce
   (lambda (d pair)
     (hash-add d (car pair) (cdr pair)))
   alist
   :initial-value (make-hash :test test :hash hash)))

(defun map-hash (function hash)
  "A new hash where function has been applied to each key-value and the
result of the each function call is the new value."
  (hash-from-alist
   (map 'list
        (lambda (cons)
          (cons (car cons)
                (funcall function (car cons) (cdr cons))))
        (hash-as-alist hash))))

(defun hash-keys (hash)
  (mapcar #'car (hash-as-alist hash)))

(defun hash-size (hash)
  (tree-weight (dict-tree hash)))
