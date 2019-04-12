(defclass binary-node ()
  ((key
    :accessor key
    :initarg :key)
   (datum
    :accessor datum
    :initarg :datum)
   (index
    :accessor index
    :initarg :index)))

(defclass binary-heap ()
  ((vec
    :accessor vec
    :initform (make-array 32 :fill-pointer 0 :adjustable t))))

(defmethod parent-index ((n binary-node))
  (floor (/ (index n) 2)))

(defmethod left-index ((n binary-node))
  (* 2 (index n)))

(defmethod right-index ((n binary-node))
  (1+ (* 2 (index n))))
  
