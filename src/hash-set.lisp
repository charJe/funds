(in-package :funds)

(defun hash-from-list (list &key (test #'eql) (hash #'sxhash))
  (hash-from-alist
   (mapcar (lambda (item) (cons item t)) list)
   :test test :hash hash))

(defun hash-as-list (hash)
  (hash-keys hash))

(defun hash-union (&rest hashes)
  "Elements of all hashes combine into one hash."
  (reduce
   (lambda (union hash)
     (reduce
      (lambda (union key-value)
        (let ((key (car key-value))
              (value (cdr key-value)))
          (hash-set union key value)))
      (hash-as-alist hash) :initial-value union))
   hashes))

(defun hash-difference (hash &rest hashes)
  "Elements of the first hash that are not in hashes."
  (reduce
   (lambda (difference hash)
     (reduce
      (lambda (difference key)
        (hash-remove difference key))
      (hash-keys hash) :initial-value difference))
   hashes :initial-value hash))

(defun hash-intersection (&rest hashes)
  "Elements that all hashes share."
  (reduce
   (lambda (intersection hash)
     (reduce
      (lambda (intersection key)
        (multiple-value-bind (value present)
            (hash-ref hash key)
          (declare (ignore value))
          (if (not present)
              (hash-remove intersection key)
              intersection)))
      (hash-keys intersection) :initial-value intersection))
   hashes))
