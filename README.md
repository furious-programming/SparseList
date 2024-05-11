# Description

This repository contains a test implementation of sparse double-linked list. This list is built from nodes that are physically located in segments that are contiguous memory blocks, which improves data locality and greatly reduces the number of necessary memory allocations and deallocations.

Segments contain a predetermined number of nodes and therefore have a fixed size, determined when the list is instantiated. When all nodes from a given segment are used to build the list and another node is needed, a new segment is allocated and connected to the existing ones, creating a linked list of segments. Unused segment nodes are available from the bank level, which is a linked list of nodes. Each new node, instead of being allocated individually, is simply pulled from the bank. When a node is no longer needed, instead of being released from memory, it is returned to the bank.

# Implementation

The implementation is made in [Free Pascal](https://www.freepascal.org) and uses a purely procedural-structural paradigm. It contains the minimum number of functions required to properly construct and use these sparse lists, and each function contains the minimum number of instructions necessary to execute. Implemented lists can be allocated on the stack and on the heap and despite not using language generics, they are generic — they can store any type of data. The source code can be easily translated into C and other low-level languages.

The structures representing lists are not opaque, so you can freely traverse them in any direction. The nodes of the actual list are available in the [NodeHead and NodeTail](https://github.com/furious-programming/SparseList/blob/master/Source/SparseList.pp#L61-L62) fields. Remember, however, that every time you need to add a node to the list, create it and attach it to the list with existing functions. If you want to detach an existing node from the list and attach it somewhere else in the list or release it, you should also use the existing functions. This will not only ensure that the list is constructed correctly, but will also ensure that the segment and node counters contain the correct numbers. However, if you know what you're doing and want it, you can manually manage segments, banks, and nodes, and create additional functions to make these lists easier to use. The existing functions are implemented to show how to properly manage segments and nodes.

# [SparseList](Source/SparseList.pp)

When a set of elements is needed in the form of a continuous block of memory, arrays are used, the memory block of which is allocated with a reserve. When the array's allocated memory runs out, it is typically reallocated and its size doubled. When elements are removed from the array, the memory block is not reallocated — its empty space is left for future use when the number of elements in the array increases again. This reduces the amount of memory allocation and relocation required, increasing performance at the expense of slightly higher resource consumption.

A basic sparse list works similarly. When new memory is needed for the list nodes, a segment with a new set of nodes is allocated. When space in a segment runs out (all segment nodes are used in the list), a new segment is allocated and added to the linked list of segments. All its nodes are connected to the bank, which contains a linked list of all unused nodes from all allocated segments. The segment list and the unused node bank are single-linked lists, used like a stack (LIFO). When a node is removed from the list, it is returned to the bank.

Segments of this list are never deallocated, even if all nodes of a given segment are not used in the list. Once allocated, a segment exists until the list is destroyed, and its nodes either reside in the general bank or are used to build the list. Unused nodes stored in the bank are not sorted in any way.

# [SparseListDyn](Source/SparseListDyn.pp)

The dynamic sparse list works similarly to the basic one, but with the difference that segments whose nodes are all unused are deallocated automatically. To make this possible, each node has a link to the owner segment, and each segment has its own bank of unused nodes. To allow to freely remove unnecessary segments, the segments form a doubly-linked list instead of a singly-linked one. Banks of unused nodes, however, still create singly-linked lists and are used like a stack (LIFO).

Since this list cannot have one bank with all unused nodes from all segments, when creating a new node, all allocated segments are searched linearly. The exception is when there is no segment yet or no segment contains an unused node, in which case a new segment is immediately allocated and its first node is returned. When a node is released, it is returned to the owner segment's bank and if all the nodes of that segment are not in use, the entire segment is released from memory. This helps to minimize resource consumption, but at the cost of slightly increased time complexity.

# Benchmarks

Since the performance of these lists depends on the specific practical application, mainly the size of the segments, the size of the data in the nodes, as well as the number of nodes used in the lists and the frequency and locations of node reorganization, the performance gain from node segmentation cannot be predicted. If you want to know what performance will be like, measure it in your project specifically. These lists are nothing groundbreaking, their performance should be higher than in the case of classic lists, but the performance gain may be negligible.

# License

Sources are unlicensed. To learn more about this, see the [LICENSE](LICENSE) file.
