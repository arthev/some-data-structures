(in-package :testing-some-data-structures)

(plan nil)



(defun test-empty-p (heap-type)
  (let ((h (make-instance heap-type)))
    (ok (ds:empty-p h)
        (format nil "~A is empty upon creation." h))
    (ds:insert 1 1 h)
    (ok (not (ds:empty-p h))
        (format nil "~A is not empty after insert." h))
    (ds:pop-extrema h)
    (ok (ds:empty-p h)
        (format nil "~A is empty after pop after insert." h))
    (dotimes (i 100)
      (let* ((r (random 100))
             (n (ds:insert r r h)))
        (when (zerop (random 2))
          (ds:update-key (random 100) n h))))
    (ok (not (ds:empty-p h))
        (format nil "~A is not empty after 100 inserts 
                     & prob 50 random key-updates." h))
    (dotimes (i 100)
      (ds:pop-extrema h))
    (ok (ds:empty-p h)
        (format nil "~A is empty again after popping the 100 inserts." h))))

(defun test-peek-extrema (heap-type)
  (let ((h (make-instance heap-type))
        (v (make-array 32 :fill-pointer 0 :adjustable t)))
    (dotimes (i 200)
      (let ((r (random 1000)))
        (ds:insert r r h)
        (vector-push-extend r v)))
    (sort v #'<)
    (is (ds:key (ds:peek-extrema h)) (aref v 0)
        (format nil "~A's peek-extrema returns same as the extrema of
                     a sorted vector of same key values." h))
    (loop for n = (ds:peek-extrema h) then (ds:peek-extrema h)
          until (not (= (ds:key n) (aref v 0)))
          do (ds:update-key (1+ (aref v (1- (length v)))) n h))
    (isnt (ds:key (ds:peek-extrema h)) (aref v 0)
          (format nil "~A's peek-extrema is not the same as of the sorted
                        vector of same key values after increasing keys in
                        a loop until the extreme value moves." h))))

(defun test-insert (heap-type)
  (let ((h (make-instance heap-type)))
    (ok (ds:verify-heap h)
        (format nil "~A verifies pre-insert." h))
    (dotimes (i 100)
      (let ((r (random 100)))
        (ds:insert r r h)
        (when (zerop (rem (1+ i) 10))
          (ok (ds:verify-heap h)
              (format nil "~A verifies after ~A inserts." h (1+ i))))))
    (dotimes (i 20)
      (if (zerop (rem i 2))
          (ds:update-key (random 200) (ds:peek-extrema h) h)
          (ds:pop-extrema h)))
    (dotimes (i 40)
      (let ((r (random 60)))
        (ds:insert r r h)
        (when (zerop (rem (1+ i) 10))
          (ok (ds:verify-heap h)
              (format nil "~A verifies after ~A inserts after some
                           key updates and pops." h (1+ i))))))))

(defun test-pop-extrema (heap-type)
  (let ((h (make-instance heap-type))
        (v (make-array 32 :fill-pointer 0 :adjustable t)))
    (ok (null (ds:pop-extrema h))
        (format nil "~A returns nil when popped when empty." h))
    (dotimes (i 200)
      (let ((r (random 1000)))
        (ds:insert r r h)
        (vector-push-extend r v)))
    (sort v #'>)
    (is (ds:key (ds:pop-extrema h)) (vector-pop v)
        (format nil "~A pops same key as least item in sorted vec." h))
    (dotimes (i 199)
      (let ((hp (ds:key (ds:pop-extrema h)))
            (vp (vector-pop v)))
        (when (or (not (eql hp vp)) (zerop (rem (1+ i) 10)))
            (is hp vp
                (format nil "~A pops same value as vector after ~A pops."
                        h (1+ i))))))))

(defun test-update-key (heap-type)
  (let ((h (make-instance heap-type))
        (v (make-array 200 :fill-pointer 0 :adjustable t)))
    (dotimes (i 200)
      (let ((r (random 200)))
        (vector-push-extend (ds:insert r r h) v)))
    (dotimes (i 100)
      (let ((r1 (random 200))
            (r2 (random 300)))
        (ds:update-key r2 (aref v r1) h)
        (when (or (not (ds:verify-heap h)) (zerop (rem (1+ i) 10)))
          (ok (ds:verify-heap h)
              (format nil "~A verifs after ~A key updates." h (1+ i))))))))
      

    


(defun test-suite (heap-type)
  (assert (typep heap-type 'symbol)) ;Can be made more extensive.
  (test-empty-p heap-type)
  (test-peek-extrema heap-type)
  (test-insert heap-type)
  ;;(test-pop-extrema heap-type))
  (test-update-key heap-type))

;(test-suite 'ds:pairing-heap)
;(test-suite 'ds:binary-heap)



(finalize)
