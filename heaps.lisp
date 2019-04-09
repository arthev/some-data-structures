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

(defmethod find-extrema ((h pairing-heap))
  (root h))

(defmethod node->heap ((n pairing-node) (comp-fn function))
  (make-instance 'pairing-heap :root n :comp-fn comp-fn))

(defmethod insert-node ((n pairing-node) (h pairing-heap))
  (meld (node->heap n (comp-fn h)) h))

(defmethod insert (key datum (h pairing-heap))
  (insert-node (make-instance 'pairing-node :key key :datum datum) h))

(defmethod empty-p ((h pairing-heap))
  (null (root h)))

(defmethod meld ((h1 pairing-heap) (h2 pairing-heap))
  (let ((comp-fn (comp-fn h1)))
    (cond ((empty-p h1) h2)
          ((empty-p h2) h1)
          (t (if (funcall comp-fn (key (root h1)) (key (root h2)))
                 (add-child h2 h1)
                 (add-child h1 h2))))))

(defmethod add-child ((child pairing-node) (parent pairing-node))
  (setf (right child) (left parent)
        (parent child) parent
        (left parent) child))
  
(defmethod add-child ((child pairing-heap) (parent pairing-heap))
  (add-child (root child) (root parent))
  parent)

(defmethod decrease-key (delta (n pairing-node) (h pairing-heap))
  (update-key (- (key n) delta) n h))

(defmethod increase-key (delta (n pairing-node) (h pairing-heap))
  (update-key (+ (key n) delta) n h))

(defmethod update-key (new-key (n pairing-node) (h pairing-heap))
  (cond ((not (eql n (root h)))
         (cut-subtree n)
         (meld (update-key new-key n (node->heap n (comp-fn h)))
               h))
        ((funcall (comp-fn h) new-key (key n))
         (setf (key n) new-key)
         h)
        (t
         (delete-extrema h)
         (insert new-key (datum n) h))))

(defmethod cut-subtree ((n pairing-node))
  (if (eql (left (parent n)) n)
      (setf (left (parent n)) (right n))
      (do ((sibling (left (parent n)) (right sibling)))
          ((eql (right sibling) n) (setf (right sibling) (right n)))))
  (setf (right n) nil
        (parent n) nil)
  n)
  
(defmethod delete-node ((n pairing-node) (h pairing-heap))
  (if (eql n (root h))
      (delete-extrema h)
      (meld (delete-extrema (node->heap (cut-subtree n) (comp-fn h)))
            h)))

(defmethod delete-extrema ((h pairing-heap))
  (when (empty-p h) (return-from delete-extrema (root h)))
  (let ((subtree-list '())
        (paired-list '())
        (comp-fn (comp-fn h))
        (extrema (find-extrema h)))
    (do ((sub (left (root h)) (left (root h))))
        ((null sub))
      (push (cut-subtree sub) subtree-list))
    (loop for first = (pop subtree-list) then (pop subtree-list)
          for second = (pop subtree-list) then (pop subtree-list)
          until (and (null first) (null second))
          do (if (null second)
                 (push (node->heap first comp-fn) paired-list)
                 (push (meld (node->heap first comp-fn)
                             (node->heap second comp-fn)) paired-list)))
    (let ((build-heap (if (null paired-list)
                          (make-instance 'pairing-heap :comp-fn comp-fn)
                          (pop paired-list))))
      (dolist (sub paired-list)
        (setf build-heap (meld build-heap sub)))
      (setf (root h) (root build-heap))
      extrema)))
    
  
