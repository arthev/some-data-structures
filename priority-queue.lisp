(cl:in-package #:some-data-structures)

(defclass priority-queue ()
  ((heap
    :accessor heap
    :initarg :heap)
   (comp-fn
    :reader comp-fn
    :initarg :comp-fn))
  (:documentation
   "A priority queue 'wrapper' around a heap."))

(defmethod initialize-instance :after ((q priority-queue)
                                       &key (heap 'pairing-heap)
                                         (comp-fn #'<))
  (if (typep heap 'symbol)
      (setf (heap q) (make-instance heap :comp-fn comp-fn)
            (comp-fn q) comp-fn)
      (setf (heap q) heap (comp-fn q) (comp-fn heap))))

(defmethod empty-p ((q priority-queue))
  (empty-p (heap q)))

(defmethod peek-extrema ((q priority-queue))
  (peek-extrema (heap q)))

(defmethod insert (key datum (q priority-queue))
  (insert key datum (heap q)))

(defmethod pop-extrema ((q priority-queue))
  (pop-extrema (heap q)))

(defmethod delete-node (n (q priority-queue))
  (delete-node n (heap q)))

(defmethod update-key (new-key n (q priority-queue))
  (update-key new-key n (heap q)))

(defmethod meld ((q1 priority-queue) (q2 priority-queue))
  (let ((meld-heap (meld (heap q1) (heap q2))))
    (setf (heap q1) meld-heap (heap q2) meld-heap)))


    
