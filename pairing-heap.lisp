(in-package :some-data-structures)
;;;External interface
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

(defmethod peek-extrema ((h pairing-heap))
  (root h))

(defmethod insert (key datum (h pairing-heap))
  (insert-node (make-instance 'pairing-node :key key :datum datum) h))

(defmethod pop-extrema ((h pairing-heap))
  ;;TODO: Rewrite in more functional way. Queue-reduce?
  (when (empty-p h) (return-from pop-extrema nil))
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

(defmethod delete-node ((n pairing-node) (h pairing-heap))
  (if (eql n (root h))
      (pop-extrema h)
      (let ((sub-heap (cut-subtree n h)))
        (pop-extrema sub-heap)
        (meld sub-heap h)
        n)))

(defmethod update-key (new-key (n pairing-node) (h pairing-heap))
  (cond ((not (eql n (root h)))
         (meld (update-key new-key n (cut-subtree n h)) h))
        ((funcall (comp-fn h) new-key (key n))
         (setf (key n) new-key))
        (t
         (pop-extrema h)
         (setf (left n) nil (right n) nil (parent n) nil (key n) new-key)
         (insert-node n h)))
  h)

(defmethod meld ((h1 pairing-heap) (h2 pairing-heap))
  (when (not (eql (comp-fn h1) (comp-fn h2)))
    (error "Attempting meld w/ heaps w/ differing comp-fns."))
  (let ((parent
          (cond ((empty-p h1) h2)
                ((empty-p h2) h1)
                (t (if (funcall (comp-fn h1)
                                (key (root h1))
                                (key (root h2)))
                       (add-child h2 h1)
                       (add-child h1 h2))))))
    (setf (root h1) (root parent) (root h2) (root parent))
    parent))

;;;Internal support
(defmethod insertn-node ((n pairing-node) (h pairing-heap))
  (setf (root h) (if (empty-p h) n (meld-nodes n (root h) (comp-fn h))))
  n)

(defmethod node->heap ((n pairing-node) (comp-fn function))
  (make-instance 'pairing-heap :root n :comp-fn comp-fn))

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
  ;;Encoding parent relation as per the child-sibling tree rather than heap.
  (when (right child)
    (setf (parent (right child)) child))
  parent)

(defmethod cut-subtree ((n pairing-node) (h pairing-heap))
  (if (eql (left (parent n)) n) ;child of parent? if not, sibling.
      (setf (left (parent n)) (right n))
      (setf (right (parent n)) (right n)))
  (when (right n)
    (setf (parent (right n)) (parent n)))
  (setf (right n) nil (parent n) nil)
  (node->heap n (comp-fn h)))

;;;TODO: Make similar seq initializer for pairing-heap as for binary-heaps.

;;;For testing purposes
(defmethod verify-heap ((h pairing-heap))
  (let ((s (list (peek-extrema h))))
    (do ((p (pop s) (pop s)))
        ((null p) t)
      (do ((c (left p) (right c)))
          ((null c))
        (if (or
             (funcall (comp-fn h) (key p) (key c))
             (eql (key p) (key c)))
            (push c s)
            (return-from verify-heap nil))))))

(defmethod size ((h pairing-heap))
  (labels ((trav-count (n)
             (if (null n)
                 0
                 (+ 1 (trav-count (left n)) (trav-count (right n))))))
    (trav-count (root h))))





