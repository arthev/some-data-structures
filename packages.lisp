(defpackage :some-data-structures
  (:use :cl)
  (:export
   #:priority-queue
   #:pairing-node
   #:pairing-heap
   #:binary-node
   #:binary-heap
   #:datum
   #:empty-p
   #:peek-extrema
   #:insert
   #:pop-extrema
   #:delete-node
   #:update-key
   #:meld
   #:verify-heap
   #:size))
   
