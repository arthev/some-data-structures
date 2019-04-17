(in-package :testing-some-data-structures)

;;;Utils
(defun knuth-shuffle (arr)
  (loop for i from (1- (length arr)) downto 1
        do (let ((j (random i)))
             (rotatef (aref arr i) (aref arr j))))
  arr)

(defmacro s (string &rest args)
  `(format nil ,string ,@args))

(defmacro repok (times str check &body body)
  (labels
      ((replace-placeholder-with-check (sexp)
         (cond ((and (atom sexp) (eql sexp :check))
                `(unless ,check (return nil)))
               ((atom sexp) sexp)
               (t (mapcar #'replace-placeholder-with-check sexp)))))
    `(ok (dotimes (i ,times t)
           ,@(replace-placeholder-with-check body))
         ,str)))

(defmacro hrepok (times str &body body)
  `(repok ,times ,str
       (ds:verify-heap h)
     ,@(append body '(:check))))

(defmacro htest<> (test)
  `(progn
     (,test heap-type #'<)
     (,test heap-type #'>)))

(defmacro hlet (bindings &body body)
  `(let ((h (make-instance heap-type :comp-fn comp-fn))
         ,@bindings)
     ,@body))

(defmacro h2let (&body body)
  `(let ((h1 (make-instance heap-type :comp-fn comp-fn))
         (h2 (make-instance heap-type :comp-fn comp-fn)))
     ,@body))

;;;Tests
(plan nil)



(defun test-empty-p (heap-type comp-fn)
  (hlet ()
    (ok (ds:empty-p h) (s "~A is empty upon creation." h))
    (ds:insert 1 1 h)
    (ok (not (ds:empty-p h)) (s "~A is not empty after an insert." h))
    (ds:pop-extrema h)
    (ok (ds:empty-p h) (s "~A is empty after pop after one insert." h))
    (dotimes (i 100)
      (ds:insert (random 100) (random 100) h))
    (ok (not (ds:empty-p h)) (s "~A is not empty after 100 inserts." h))
    (dotimes (i 100)
      (ds:pop-extrema h))
    (ok (ds:empty-p h) (s "~A is empty after popping the 100 inserts." h))))

(defun test-peek-extrema (heap-type comp-fn)
  (hlet ((v (make-array 32 :fill-pointer 0 :adjustable t)))
    (dotimes (i 200)
      (let ((r (random 1000)))
        (ds:insert r r h)
        (vector-push-extend r v)))
    (sort v comp-fn)
    (is (ds:key (ds:peek-extrema h)) (aref v 0)
        (s "~A's peek-extrema returns extrema." h))))

(defun test-insert (heap-type comp-fn)
  (hlet ()
    (ok (ds:verify-heap h) (s "~A heap-verifies pre-insert." h))
    (hrepok 100 (s "~A heap-verifies ∀ 100 inserts." h)
      (let ((r (random 100)))
        (ds:insert r i h)))
    (is (ds:size h) 100 (s "~A has size 100 after 100 inserts." h))
    (hrepok 50 (s "~A heap-verifies ∀ 25 alt pop and updkey." h)
      (if (zerop (rem i 2))
          (ds:update-key (random 200) (ds:peek-extrema h) h)
          (ds:pop-extrema h)))
    (is (ds:size h) 75 (s "~A has size 75 after 100 inserts & 25 pops." h))
    (hrepok 40 (s "~A verifies ∀ 40 inserts after pops and upds." h)
      (let ((r (random 60)))
        (ds:insert r r h)))))

(defun test-pop-extrema (heap-type comp-fn)
  (hlet ((v (make-array 32 :fill-pointer 0 :adjustable t)))
    (ok (null (ds:pop-extrema h))
        (s "~A returns nil when popped when empty." h))
    (dotimes (i 200)
      (let ((r (random 1000)))
        (ds:insert r r h)
        (vector-push-extend r v)))
    (sort v (complement comp-fn))
    (is (ds:key (ds:pop-extrema h)) (vector-pop v)
        (s "~A pops same key as least item in sorted vec." h))
    (repok 199 (s "~A pops correct vals and heap-verifies ∀ pops." h)
        (and (eql (ds:key (ds:pop-extrema h)) (vector-pop v))
             (ds:verify-heap h))
        :check)))
  
(defun test-delete-node (heap-type comp-fn)
  (hlet ((ht (make-hash-table))
         (v (make-array 100)))
    (ds:insert 1 1 h)
    (ds:delete-node (ds:peek-extrema h) h)
    (ok (ds:empty-p h) (s "~A is empty after delete-node on root." h))
    
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
        (s "~A heap-verifies ∀ node deletions in random order." h))
    (ok (ds:empty-p h) (s "~A is empty after deleting all nodes." h))))

(defun test-update-key (heap-type comp-fn)
  (hlet ((v (make-array 200 :fill-pointer 0 :adjustable t)))
    (dotimes (i 200)
      (let ((r (random 200)))
        (vector-push-extend (ds:insert r i h) v)))
    (hrepok 100 (s "~A heap-verifies ∀ 100 key updates." h)
      (ds:update-key (random 300) (aref v (random 200)) h))))

(defun test-meld (heap-type comp-fn)
  (h2let
    (ok (ds:empty-p (ds:meld h1 h2))
        (s "~A meld w/ two empty heaps returns an empty one." heap-type)))
  (h2let
    (ds:insert 1 1 h1)
    (is (ds:meld h1 h2)
        h1
        (s "~A meld returns non-empty heap if given one 
                     empty heap." heap-type)))
  (h2let
    (let ((n1 (ds:insert 3 3 h1))
          (n2 (ds:insert 5 5 h2)))
      (ds:meld h1 h2)
      (ok (and (eql (ds:peek-extrema h1) (ds:peek-extrema h2))
               (eql (ds:peek-extrema h1)
                    (if (funcall comp-fn (ds:key n1) (ds:key n2)) n1 n2)))
          (s "~A root of heaps meld updates to correct 
                       element given two 1-node heaps." heap-type))))
  (h2let
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
          (s "~A has size 100 after melding |60| and |40|." h1))))
  
  (let ((h1 (make-instance heap-type :comp-fn comp-fn))
        (h2 (make-instance heap-type :comp-fn (complement comp-fn))))
    (ok (block error-block
          (handler-case (ds:meld h1 h2)
            (error ()
              (return-from error-block t)))
          nil)
        (s "~A meld generates error if heaps w/ diff comp-fns
                     are attempted melded." heap-type))))

(defun test-seq-init (heap-type node-type comp-fn)
  (let ((v (make-array 100 :fill-pointer 0 :adjustable t)))
    (dotimes (i 100)
      (vector-push-extend (make-instance node-type
                                         :key (random 200)
                                         :datum i)
                          v))
    (let ((h (make-instance heap-type :comp-fn comp-fn :seq v)))
      (sort v (complement comp-fn) :key #'ds:key)
      (ok (ds:verify-heap h) (s "~A heap-verifies after seq init." h))
      (repok 100 (s "~A contains right items and verifies when popping
                     after seq init." h)
          (and (eql (ds:key (ds:pop-extrema h)) (ds:key (vector-pop v)))
               (ds:verify-heap h))
        :check))))


(defun test-suite (heap-type node-type)
  (assert (typep heap-type 'symbol))
  (htest<> test-empty-p)
  (htest<> test-peek-extrema)
  (htest<> test-insert)
  (htest<> test-pop-extrema)
  (htest<> test-delete-node)
  (htest<> test-update-key)
  (htest<> test-meld)
  (test-seq-init heap-type node-type #'<)
  (test-seq-init heap-type node-type #'>)
  )

(test-suite 'ds:pairing-heap 'ds:pairing-node)
(test-suite 'ds:binary-heap 'ds:binary-node)



(finalize)
