(defpackage :some-data-structures
  (:use :cl)
  (:nicknames :ds)
  (:export
   :priority-queue
   :pairing-node
   :pairing-heap
   :binary-node
   :binary-heap
   :key
   :datum
   :empty-p
   :size
   :peek-extrema
   :insert
   :pop-extrema
   :delete-node
   :update-key
   :meld
   :verify-heap
   ;;temp
   :vec
   :print-heap
   :comp-fn))
   
