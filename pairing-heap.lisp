(in-package :some-data-structures)
;;;Externals
(defclass pairing-heap ()
  ((root
    :accessor root
    :initform nil
    :initarg :root)
   (comp-fn
    :accessor comp-fn
    :initform #'<
    :initarg :comp-fn))
  (:documentation
   "A multipass pairing heap implementation per (Fredman et al., 1986)."))

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
    :initarg :datum))
  (:documentation
   "Pairing heap nodes. Parent relations are per the representing
    child-sibling binary tree, not per the heap tree."))

(defmethod empty-p ((h pairing-heap))
  (null (root h)))

(defmethod peek-extrema ((h pairing-heap))
  (root h))

(defmethod insert (key datum (h pairing-heap))
  (insert-node (make-instance 'pairing-node :key key :datum datum) h))

(defmethod pop-extrema ((h pairing-heap))
  ;;Builds resultant heap by isolating subtrees corr. to extrema's children
  ;;and then combining them in pairs, using multipass, until only one tree.
  (when (empty-p h) (return-from pop-extrema nil))
  (prog1 (root h)
    (setf
     (root h)
     (loop for subs = (loop for s = (left (root h))
                              then (prog1 (right s)
                                     (setf (parent s) nil (right s) nil))
                            until (null s)
                            collect s)
             then (loop for (s1 s2) on subs by #'cddr
                        collect (if (null s2)
                                    s1
                                    (meld-nodes s1 s2 (comp-fn h))))
           while (cdr subs)
           finally (return (car subs))))))

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
  ;;Melds the heaps by making one the child of the other,
  ;;and then updating the root pointers of the input heaps.
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

;;;Internals
(defmethod insert-node ((n pairing-node) (h pairing-heap))
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
  ;;Pointers are per the repr child-sibling binary tree, not the heap tree."
  (when (right child)
    (setf (parent (right child)) child))
  parent)

(defmethod cut-subtree ((n pairing-node) (h pairing-heap))
  (if (eql (left (parent n)) n) ;child of parent? else sibling.
      (setf (left (parent n)) (right n))
      (setf (right (parent n)) (right n)))
  (when (right n)
    (setf (parent (right n)) (parent n)))
  (setf (right n) nil (parent n) nil)
  (node->heap n (comp-fn h)))

(defmethod initialize-instance :after ((h pairing-heap)
                                       &key (seq nil) &allow-other-keys)  
  (assert (typep seq 'sequence) (seq) "~A is not a sequence." seq)
  (when (and seq (not (zerop (length seq))))
    (map nil (lambda (n)
               (if (typep n 'pairing-node)
                   (progn (setf (parent n) nil (right n) nil (left n) nil)
                          (insert-node n h))
                   (insert (key n) (datum n) h)))
         seq))
  h)

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
