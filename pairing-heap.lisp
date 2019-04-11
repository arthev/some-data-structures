(defclass pairing-node ()
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
    :initarg :datum)))

(defclass pairing-heap ()
  ((root
    :accessor root
    :initform nil
    :initarg :root)
   (comp-fn
    :accessor comp-fn
    :initform #'<
    :initarg :comp-fn)))

(defmethod empty-p ((h pairing-heap))
  (null (root h)))

(defmethod find-extrema ((h pairing-heap))
  (root h))

(defmethod node->heap ((n pairing-node) (comp-fn function))
  (make-instance 'pairing-heap :root n :comp-fn comp-fn))

(defmethod insert-node ((n pairing-node) (h pairing-heap))
  (setf (root h) (if (empty-p h) n (meld-nodes n (root h) (comp-fn h))))
  n)

(defmethod insert (key datum (h pairing-heap))
  (insert-node (make-instance 'pairing-node :key key :datum datum) h))

(defmethod meld ((h1 pairing-heap) (h2 pairing-heap))
  (when (not (eql (comp-fn h1) (comp-fn h2)))
    (error "Attempting meld w/ heaps w/ differing comp-fns."))
  (let ((parent
          (cond ((empty-p h1) h2)
                ((empty-p h2) h1)
                (t (if (funcall (comp-fn h1) (key (root h1))
                                (key (root h2)))
                       (add-child h2 h1)
                       (add-child h1 h2))))))
    (setf (root h1) (root parent) (root h2) (root parent))))

(defmethod add-child ((child pairing-heap) (parent pairing-heap))
  (add-child (root child) (root parent))
  parent)

(defmethod meld-nodes ((n1 pairing-node) (n2 pairing-node)
                       (comp-fn function))
  (if (funcall comp-fn (key n1) (key n2))
      (add-child n2 n1)
      (add-child n1 n2)))

(defmethod add-child ((child pairing-node) (parent pairing-node))
  (setf (right child) (left parent)
        (parent child) parent
        (left parent) child)
  parent)

(defmethod cut-subtree ((n pairing-node) (h pairing-heap))
  (if (eql (left (parent n)) n)
      (setf (left (parent n)) (right n))
      (do ((sibling (left (parent n)) (right sibling)))
          ((eql (right sibling) n) (setf (right sibling) (right n)))))
  (setf (right n) nil (parent n) nil)
  (node->heap n (comp-fn h)))

(defmethod delete-node ((n pairing-node) (h pairing-heap))
  (if (eql n (root h))
      (delete-extrema h)
      (let ((sub-heap (cut-subtree n h)))
        (delete-extrema sub-heap)
        (meld sub-heap h)
        n)))

(defmethod delete-extrema ((h pairing-heap))
  (when (empty-p h) (return-from delete-extrema (root h)))
  (let ((subtree-list '())
        (paired-list '())
        (comp-fn (comp-fn h))
        (extrema (root h)))
    (do* ((sub (left (root h)) next)
          (next (when sub (right sub)) (when sub (right sub))))
         ((null sub))
      (setf (parent sub) nil (right sub) nil)
      (push sub subtree-list))
    (do* ((first (pop subtree-list) (pop subtree-list))
          (second (pop subtree-list) (pop subtree-list)))
         ((and (null first) (null second)))
      (if (null second)
          (push first paired-list)
          (push (meld-nodes first second comp-fn) paired-list)))
    (let ((build-node (if (null paired-list) nil (pop paired-list))))
      (dolist (sub paired-list)
        (setf build-node (meld-nodes build-node sub comp-fn)))
      (setf (root h) build-node)
      extrema)))

(defmethod decrease-key (delta (n pairing-node) (h pairing-heap))
  (update-key (- (key n) delta) n h))

(defmethod increase-key (delta (n pairing-node) (h pairing-heap))
  (update-key (+ (key n) delta) n h))

(defmethod update-key (new-key (n pairing-node) (h pairing-heap))
  (cond ((not (eql n (root h)))
         (meld (update-key new-key n (cut-subtree n h)) h))
        ((funcall (comp-fn h) new-key (key n))
         (setf (key n) new-key))
        (t
         (delete-extrema h)
         (setf (left n) nil (right n) nil (parent n) nil (key n) new-key)
         (insert-node n h)))
  h)
