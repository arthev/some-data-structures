## some-data-structures

Implementations of some data structures, available under the MIT License. 

Currently, the following data structures are implemented here:

#### Binary Heap 

An implicit binary heap - represented as an array of binary-nodes, where nodes contain keys, data and indices. 
Implementation follows (Cormen et al., 2001), adjusted for zero-indexed arrays, plus a few bells and whistles.

Implemented since implicit d-ary heaps performed well in (Larkin et al., 2014).

Bell example: update-key in general, rather than just improve-key.\
Whistle example: array-indices stored in nodes, avoiding having to search for them when doing arbitrary node operations.

#### Pairing Heap

An explicit multipass pairing heap - represented with a child-sibling binary tree of pairing-nodes, 
where nodes contain keys, data, and tree pointers. Implementation follows (Fredman et al., 1986), plus a few bells and whistles.

Implemented since pairing heaps performed well in (Larkin et al., 2014).

Bell example: arbitrary comparison functions possible.\
Whistle example: update-key in general, rather than just improve-key.

#### Priority Queue

Implemented as a wafer-thin 'wrapper' around the heaps. 



### Sources

Cormen, Thomas H., Charles E. Leiserson, Ronald L. Rivest, and Clifford Stein. "Heapsort" Chap. 6 in _Introduction to Algorithms_. MIT Press, 2001.

Fredman, Michael L., Robert Sedgewick, Daniel D. Sleator, and Robert E. Tarjan. "The Pairing Heap: A New Form of Self-Adjusting Heap" _Algorithmica_ 1 (1986): 111-129.

Larkin, Daniel H., Siddharta Sen, Robert E. Tarjan. "A Back-to-Basics Empirical Study of Priority Queues" _Proceedings of the Meeting on Algorithm Engineering & Expermiments_ (2014): 61-72.

