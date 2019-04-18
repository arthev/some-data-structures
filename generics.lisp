(cl:in-package #:some-data-structures)

(defgeneric empty-p (heap)
  (:documentation
   #.(format nil "Returns whether HEAP is an empty heap.~@
                  Binary: Θ(1).~@
                  Pairing: Θ(1).")))

(defgeneric peek-extrema (heap)
  (:documentation
   #.(format nil "Returns the extrema without modifying HEAP.~@
                  Binary: Θ(1).~@
                  Pairing: Θ(1).")))

(defgeneric insert (key datum heap)
  (:documentation
   #.(format nil "Creates, inserts into HEAP and returns node~@
                  w/ key & datum. Store the resultant node in~@
                  an auxiliary lookup structure if you need to~@
                  do opereations on arbitrary nodes.~@
                  Binary: O(log n).~@
                  Pairing: Θ(1).")))

(defgeneric pop-extrema (heap)
  (:documentation
   #.(format nil "Pops the extrema from HEAP. Returns nil if~@
                  HEAP is empty, else the datum of the extrema.~@
                  Rebuilds resultant data to~@
                  keep relevant heap properties.~@
                  Binary: Θ(log n).~@
                  Pairing: O(log n) amortized.")))

(defgeneric delete-node (node heap)
  (:documentation
   #.(format nil "Deletes NODE from HEAP and returns NODE.~@
                  Efficiently keeping track of nodes requires~@
                  an auxiliary data structure and is left to~@
                  the user.~@
                  Binary: O(log n).~@
                  Pairing: O(log n) amortized?")))

(defgeneric update-key (new-key node heap)
  (:documentation
   #.(format nil "Updates key and therefore position of NODE in HEAP.~@
                  Improving a key relative to HEAP's comparison fn:~@
                  Binary: O(log n).~@
                  Pairing: Θ(1).~@
                  Retarding a key relative to HEAP's comparison fn:~@
                  Binary: O(log n).~@
                  Pairing: O(log n) amortized?")))

(defgeneric meld (heap1 heap2)
  (:documentation
   #.(format nil "Melds two heaps into one and updates internal~@
                  data so that they, after the merge, are~@
                  equivalent heaps. Returns a melded heap.~@
                  Binary: Θ(n).~@
                  Pairing: Θ(1).")))

(defgeneric verify-heap (heap)
  (:documentation
   #.(format nil "Uses an exhaustive search to check that relevant~@
                  heap properties hold at all levels of HEAP. Meant~@
                  for testing. Necessarily Θ(n).")))

(defgeneric size (heap)
  (:documentation
   #.(format nil "Computes the size of the heap in number of nodes.~@
                  Meant for testing.~@
                  Binary: Θ(1).~@
                  Pairing: Θ(n) and recursive - may well blow the stack.~@
                  Better to create counting wrappers around the heap~@
                  if you need the size of a pairing heap.")))
