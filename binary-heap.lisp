(cl:in-package #:some-data-structures)

;;; Externals.
(defclass binary-heap ()
  ((binary-heap-vector
    :accessor vec
    :initarg :seq
    :initform (make-array 32 :fill-pointer 0 :adjustable t))
   (comparison-function
    :reader comp-fn
    :initarg :comp-fn
    :initform #'<))
  (:documentation
   "A binary heap implementation per (Cormen et al., 2002), adjusted 
    for zero-indexed arrays. The heap is implicit: relations are
    array-positional instead of pointer-explicit."))

(defclass binary-node ()
  ((node-key
    :accessor key
    :initarg :key)
   (node-datum
    :reader datum
    :initarg :datum)
   (node-index
    :accessor index
    :initarg :index))
  (:documentation
   "Binary heap nodes. Storing the index saves searching for position
    when calling operations on arbitrarily-positioned nodes."))

(defmethod empty-p ((h binary-heap))
  (zerop (length (vec h))))

(defmethod peek-extrema ((h binary-heap))
  (aref (vec h) 0))

(defmethod insert (key datum (h binary-heap))
  (insert-node (make-instance 'binary-node :key key :datum datum) h))

(defmethod pop-extrema ((h binary-heap))
  (let ((extrema (pop-extrema-node h)))
    (if (null extrema)
        nil
        (datum extrema))))

(defmethod delete-node ((n binary-node) (h binary-heap))
  (assert (eql n (aref (vec h) (index n))) (n h) "~A is not in ~A." n h)
  (let* ((leaf (vector-pop (vec h)))
         (leaf-key (key leaf)))
    (unless (eql leaf n)
      (setf (aref (vec h) (index n)) leaf
            (index leaf) (index n)
            (key leaf) (key n))
      (update-key leaf-key leaf h))
    n))

(defmethod update-key (new-key (n binary-node) (h binary-heap))
  (let ((old-key (key n)))
    (setf (key n) new-key)
    (if (or (null old-key) (funcall (comp-fn h) new-key old-key))
        (do ((parent (parent-impl n h) (parent-impl n h)))
            ((or (funcall (comp-fn h) (key parent) new-key) (eql parent n)))
          (exchange n parent h))
        (heapify n h))
    n))

(defmethod meld ((h1 binary-heap) (h2 binary-heap))
  ;; Melding delegates to the seq initializer for binary heaps,
  ;; and then updates the vec pointers of the input heaps.
  (assert (eql (comp-fn h1) (comp-fn h2)) (h1 h2)
          "~A and ~A don't have same comp-fn." h1 h2)
  (let ((new-heap
          (cond ((empty-p h1) h2)
                ((empty-p h2) h1)
                (t (make-instance
                    'binary-heap
                    :comp-fn (comp-fn h1)
                    :seq (concatenate 'vector (vec h1) (vec h2)))))))
    (setf (vec h1) (vec new-heap) (vec h2) (vec new-heap))
    new-heap))
  
;;; Internals.
(defmethod insert-node ((n binary-node) (h binary-heap))
  (setf (index n) (length (vec h)))
  (vector-push-extend n (vec h))
  (update-key (prog1 (key n) (setf (key n) nil)) n h)) ; NIL flags as new.

(defmethod pop-extrema-node ((h binary-heap))
  (when (empty-p h) (return-from pop-extrema-node nil))
  (prog1 (peek-extrema h)
    (setf (aref (vec h) 0) (vector-pop (vec h))
          (index (aref (vec h) 0)) 0)
    (heapify (peek-extrema h) h)))

(defmethod parent-impl ((n binary-node) (h binary-heap))
  (aref (vec h) (max 0 (floor (/ (1- (index n)) 2)))))

(defmethod left-impl ((n binary-node) (h binary-heap))
  (let ((li (1+ (* 2 (index n)))))
    (when (< li (length (vec h)))
      (aref (vec h) li))))

(defmethod right-impl ((n binary-node) (h binary-heap))
  (let ((ri (+ 2 (* 2 (index n)))))
    (when (< ri (length (vec h)))
      (aref (vec h) ri))))

(defmethod heapify ((n binary-node) (h binary-heap))
  (let ((l (left-impl n h)) (r (right-impl n h))
        (comp-fn (comp-fn h)) (local n))
    (when (and l (funcall comp-fn (key l) (key local)))
      (setf local l))
    (when (and r (funcall comp-fn (key r) (key local)))
      (setf local r))
    (when (not (eql local n))
      (exchange local n h)
      (heapify n h))))

(defmethod print-heap ((h binary-heap))
  (format t "~A~%" (map 'vector (lambda (n)
                                  `(:key ,(key n) :datum ,(datum n)
                                    :index ,(index n)))
                        (vec h))))

(defmethod exchange ((n1 binary-node) (n2 binary-node) (h binary-heap))
  (psetf (index n1) (index n2)
         (index n2) (index n1)
         (aref (vec h) (index n1)) n2
         (aref (vec h) (index n2)) n1))

(defmethod initialize-instance :after ((h binary-heap)
                                       &key (seq nil) &allow-other-keys)
  (assert (typep seq 'sequence) (seq) "~A is not a sequence." seq)
  (let ((new-vec (make-array 32 :fill-pointer 0 :adjustable t)))
    (setf (vec h) new-vec)
    (unless (or (null seq) (zerop (length seq)))
      (map nil (lambda (n)
                 (vector-push-extend
                  (if (typep n 'binary-node)
                      (progn (setf (index n) (length new-vec))
                             n)
                      (make-instance 'binary-node
                                     :key (key n)
                                     :datum (datum n)
                                     :index (length new-vec)))
                  new-vec))
           seq)
      (loop for i from (floor (/ (length new-vec) 2)) downto 0
            do (heapify (aref new-vec i) h))))
  h)

;;; For testing purposes.
(defmethod verify-heap ((h binary-heap))
  (when (empty-p h) (return-from verify-heap t))
  (flet ((verify-relation (parent child)
           (unless (or (null child)
                       (funcall (comp-fn h) (key parent) (key child))
                       (eql (key parent) (key child)))
             (return-from verify-heap nil))))
    (loop for i from 0 to (floor (/ (length (vec h)) 2))
          do (let* ((n (aref (vec h) i))
                    (l (left-impl n h))
                    (r (right-impl n h)))
               (verify-relation n l)
               (verify-relation n r)))
    t))

(defmethod size ((h binary-heap))
  (length (vec h)))
