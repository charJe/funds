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

(defun hash-set (hash key value)
  "A hash similar to the given hash except that key maps to
value in the returned hash."
  (let* ((h (funcall (dict-hash hash) key))
         (old-alist (tree-find (dict-tree hash) h))
         (new-alist (acons key value (remove (assoc key old-alist :test (dict-test hash))
                                       old-alist))))
    (make-dict :hash (dict-hash hash)
               :test (dict-test hash)
               :tree (tree-insert (dict-tree hash) h new-alist))))

(defun hash-remove (hash key)
  "A hash similar to the given hash, except that key does
not map to any value in the returned hash."
  (let* ((h (funcall (dict-hash hash) key))
         (old-alist (tree-find (dict-tree hash) h))
         (new-alist (remove (assoc key old-alist :test (dict-test hash))
                            old-alist)))
    (make-dict :hash (dict-hash hash)
               :test (dict-test hash)
               :tree (if (null new-alist)
                         (tree-remove (dict-tree hash) h)
                         (tree-insert (dict-tree hash) h new-alist)))))

(defun hash-ref (hash key &optional default)
  "The value associated with the given key in the given hash.  A second
value is returned to indicate whether the key is associated with any value or
is not found."
  (let ((pair (assoc key (tree-find (dict-tree hash) (funcall (dict-hash hash) key))
                     :test (dict-test hash))))
    (if (null pair)
        (values default nil)
        (values (cdr pair) t))))

(defun hash-as-alist (hash)
  "An alist containing the same key-value pairs as the given hash."
  (labels ((f (tree)
             (if (tree-empty-p tree)
                 nil
                 (append (f (bt-left tree))
                         (bt-value tree)
                         (f (bt-right tree))))))
    (f (dict-tree hash))))

(defun hash-from-alist (alist &key (test #'eql) (hash #'sxhash))
  (reduce
   (lambda (hash pair)
     (hash-set hash (car pair) (cdr pair)))
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
