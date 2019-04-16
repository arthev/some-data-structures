(in-package :testing-some-data-structures)

;;;Utils
(defun knuth-shuffle (arr)
  (loop for i from (1- (length arr)) downto 1
        do (let ((j (random i)))
             (rotatef (aref arr i) (aref arr j))))
  arr)

(defmacro s (string &rest args)
  `(format nil ,string ,@args))



(plan nil)



(defun test-empty-p (heap-type)
  (let ((h (make-instance heap-type)))
    (ok (ds:empty-p h)
        (s "~A is empty upon creation." h))
    (ds:insert 1 1 h)
    (ok (not (ds:empty-p h))
        (s "~A is not empty after insert." h))
    (ds:pop-extrema h)
    (ok (ds:empty-p h)
        (s "~A is empty after pop after insert." h))
    (dotimes (i 100)
      (let* ((r (random 100))
             (n (ds:insert r r h)))
        (when (zerop (random 2))
          (ds:update-key (random 100) n h))))
    (ok (not (ds:empty-p h))
        (s "~A is not empty after 100 inserts 
                     & prob 50 random key-updates." h))
    (dotimes (i 100)
      (ds:pop-extrema h))
    (ok (ds:empty-p h)
        (s "~A is empty again after popping the 100 inserts." h))))

(defun test-peek-extrema (heap-type)
  (let ((h (make-instance heap-type))
        (v (make-array 32 :fill-pointer 0 :adjustable t)))
    (dotimes (i 200)
      (let ((r (random 1000)))
        (ds:insert r r h)
        (vector-push-extend r v)))
    (sort v #'<)
    (is (ds:key (ds:peek-extrema h)) (aref v 0)
        (s "~A's peek-extrema returns same as the extrema of
                     a sorted vector of same key values." h))
    (loop for n = (ds:peek-extrema h) then (ds:peek-extrema h)
          until (not (= (ds:key n) (aref v 0)))
          do (ds:update-key (1+ (aref v (1- (length v)))) n h))
    (isnt (ds:key (ds:peek-extrema h)) (aref v 0)
          (s "~A's peek-extrema is not the same as of the sorted
                        vector of same key values after increasing keys in
                        a loop until the extreme value moves." h))))

(defun test-insert (heap-type)
  (let ((h (make-instance heap-type)))
    (ok (ds:verify-heap h)
        (s "~A verifies pre-insert." h))
    (dotimes (i 100)
      (let ((r (random 100)))
        (ds:insert r r h)
        (when (zerop (rem (1+ i) 10))
          (ok (ds:verify-heap h)
              (s "~A verifies after ~A inserts." h (1+ i))))))
    (is (ds:size h)
        100
        (s "~A has size 100 after 100 inserts." h))
    (dotimes (i 50)
      (if (zerop (rem i 2))
          (ds:update-key (random 200) (ds:peek-extrema h) h)
          (ds:pop-extrema h))
      (when (not (ds:verify-heap h))
        (error "wtf")))
    (is (ds:size h)
        75
        (s "~A has size 75 after 100 inserts & 25 pops." h))
    (dotimes (i 40)
      (let ((r (random 60)))
        (ds:insert r r h)
        (when (zerop (rem (1+ i) 10))
          (ok (ds:verify-heap h)
              (s "~A verifies after ~A inserts after some
                           key updates and pops." h (1+ i))))))))

(defun test-pop-extrema (heap-type)
  (let ((h (make-instance heap-type))
        (v (make-array 32 :fill-pointer 0 :adjustable t)))
    (ok (null (ds:pop-extrema h))
        (s "~A returns nil when popped when empty." h))
    (dotimes (i 200)
      (let ((r (random 1000)))
        (ds:insert r r h)
        (vector-push-extend r v)))
    (sort v #'>)
    (is (ds:key (ds:pop-extrema h)) (vector-pop v)
        (s "~A pops same key as least item in sorted vec." h))
    (dotimes (i 199)
      (let ((hp (ds:key (ds:pop-extrema h)))
            (vp (vector-pop v)))
        (when (or (not (eql hp vp)) (zerop (rem (1+ i) 10)))
            (is hp vp
                (s "~A pops same value as vector after ~A pops."
                        h (1+ i))))))))

(defun test-delete-node (heap-type)
  (let ((h (make-instance heap-type))
        (ht (make-hash-table))
        (v (make-array 100)))
    (ds:insert 1 1 h)
    (ds:delete-node (ds:peek-extrema h) h)
    (ok (ds:empty-p h)
        (s "~A is empty after delete-node'ing the root." h))

    (dotimes (i 100)
      (let ((r (random 100)))
        (setf (gethash i ht) (ds:insert r i h))
        (setf (aref v i) i)))
    (knuth-shuffle v)
    (dotimes (i 20)
      (ds:update-key (random 100) (gethash (random 100) ht) h))
    (ok (loop for n across v
              do (ds:delete-node (gethash n ht) h)
              when (not (ds:verify-heap h))
                do (return nil)
              finally (return t))
        (s "~A verifies as heap at each step of deleting all nodes
                     in random order." h))
    (ok (ds:empty-p h)
        (s "~A is empty after randomly deleting all nodes." h))))

(defun test-update-key (heap-type)
  (let ((h (make-instance heap-type))
        (v (make-array 200 :fill-pointer 0 :adjustable t)))
    (dotimes (i 200)
      (let ((r (random 200)))
        (vector-push-extend (ds:insert r i h) v)))
    (dotimes (i 100)
      (let ((r1 (random 200))
            (r2 (random 300)))
        (ds:update-key r2 (aref v r1) h)
        (when (or (not (ds:verify-heap h)) (zerop (rem (1+ i) 10)))
          (ok (ds:verify-heap h)
              (s "~A verifs after ~A key updates." h (1+ i))))))))

(defun test-meld (heap-type)
  (let ((h1 (make-instance heap-type))
        (h2 (make-instance heap-type)))
    
    (ok (ds:empty-p (ds:meld h1 h2))
        (s "~A meld returns empty heap if given two empty ones."
                heap-type))
    
    (ds:insert 1 1 h1)
    (is (ds:meld h1 h2)
        h1
        (s "~A meld returns non-empty heap if given one 
                     empty heap." heap-type))
    (ds:pop-extrema h1)
    (ds:pop-extrema h2)

    (let ((n1 (ds:insert 3 3 h1)))
      (ds:insert 5 5 h2)
      (ds:meld h1 h2)
      (ok (and (eql (ds:peek-extrema h1) (ds:peek-extrema h2))
               (eql (ds:peek-extrema h1) n1))
          (s "~A root of heaps meld updates to correct 
                       element given two 1-node heaps." heap-type))))
  
  (let ((h1 (make-instance heap-type))
        (h2 (make-instance heap-type)))
    (dotimes (i 40)
      (ds:insert (random 100) i h1))
    (dotimes (i 60)
      (ds:insert (random 100) i h2))
    (let ((n1 (ds:peek-extrema h1))
          (n2 (ds:peek-extrema h2)))
      (ds:meld h1 h2)
      (ok (and (ds:verify-heap h1) (ds:verify-heap h2))
          (s "~A verifies both heap args after a meld." heap-type))
      (ok (or (eql n1 (ds:peek-extrema h1))
              (eql n2 (ds:peek-extrema h1)))
          (s "~A post-meld has root as expected." heap-type))
      (is (ds:size h1)
        100
        (s "~A has size 100 after melding |60| and |40|." h1))

      
  (let ((h1 (make-instance heap-type :comp-fn #'<))
        (h2 (make-instance heap-type :comp-fn #'>)))
    (ok (block error-block
          (handler-case (ds:meld h1 h2)
            (error ()
              (return-from error-block t)))
          nil)
        (s "~A meld generates error if heaps w/ diff comp-fns
                     are attempted melded." heap-type))))))

  


    
    
      

    


(defun test-suite (heap-type)
  (assert (typep heap-type 'symbol)) ;Can be made more extensive.
  (test-empty-p heap-type)
  (test-peek-extrema heap-type)
  (test-insert heap-type)
  (test-pop-extrema heap-type)
  (test-delete-node heap-type)
  (test-update-key heap-type)
  (test-meld heap-type))


(test-suite 'ds:pairing-heap)
;(test-suite 'ds:binary-heap)



(finalize)
