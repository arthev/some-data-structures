(in-package :some-data-structures)

(defclass priority-queue ()
  ((heap
    :accessor heap
    :initarg :heap)
   (comp-fn
    :accessor comp-fn
    :initarg :comp-fn)))

(defmethod initialize-instance :after ((q priority-queue)
                                       &key (heap nil) (comp-fn #'<))
  (if heap
      (setf (heap q) heap)
      (setf (heap q) (make-instance 'pairing-heap :comp-fn comp-fn)))
  (setf (comp-fn q) comp-fn))

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
  (meld (heap q1) (heap q2)))