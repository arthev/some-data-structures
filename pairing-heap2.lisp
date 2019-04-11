;;;;I started this thinking it'd be neater than pairing-node + pairing-heap,
;;;;but having to in-use constantly track and re-setf the heap root is a big
;;;;nuisance. Scrapped.

(defclass pairing-heap2 ()
  ((left
    :accessor left
    :initform nil
    :initarg :left)
   (right
    :accessor right
    :initform nil
    :initarg :right)
   (parent
    :accessor parent
    :initform nil
    :initarg :parent)
   (key
    :accessor key
    :initform nil
    :initarg :key)
   (datum
    :accessor datum
    :initform nil
    :initarg :datum)
   (comp-fn
    :accessor comp-fn
    :initform #'<
    :initarg :comp-fn))
  (:documentation
   "An endogenuous one-class repr of pairing heaps. Several methods 
    return (values root node). Root status is not 'pointer stable' - 
    given nodes n1 and n2, with n1 as root before an operation, 
    n2 might be the root afterwards."))

(defmethod root-p ((h pairing-heap2))
  (null (parent h)))

(defmethod empty-p ((h pairing-heap2))
  (null (key h)))

(defmethod find-extrema ((h pairing-heap2))
  (if (root-p h)
      (datum h)
      (error "~S is not a heap root." h)))

(defmethod insert (key datum (h pairing-heap2))
  (let ((node (make-instance 'pairing-heap2 :key key :datum datum
                                            :comp-fn (comp-fn h))))
    (values (meld node h) node)))

(defmethod meld ((h1 pairing-heap2) (h2 pairing-heap2))
  (cond ((not (eql (comp-fn h1) (comp-fn h2)))
         (error "Attempting meld w/ heaps w/ differing comp-fns."))
        ((not (root-p h1)) (error "~S is not a heap root." h1))
        ((not (root-p h2)) (error "~S is not a heap root." h2)))
  (cond ((empty-p h1) h2)
        ((empty-p h2) h1)
        (t (if (funcall (comp-fn h1) (key h1) (key h2))
               (add-child h2 h1)
               (add-child h1 h2)))))

(defmethod add-child ((child pairing-heap2) (parent pairing-heap2))
  (setf (right child) (left parent)
        (parent child) parent
        (left parent) child)
  parent)

(defmethod update-key (new-key (n pairing-heap2) (h pairing-heap2))
  "Updates (key n) to new-key, returns root of the updated heap."
  (cond ((not (root-p n))
         (meld (update-key new-key (cut-subtree n) n) h))
        ((funcall (comp-fn n) new-key (key n))
         (setf (key n) new-key)
         n)
        (t
         (multiple-value-bind (h n)
             (delete-extrema n)
           (setf (left n) nil (right n) nil (parent n) nil (key n) new-key)
           (meld n h)))))

(defmethod cut-subtree ((n pairing-heap2))
  ...)


