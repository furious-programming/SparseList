
{
  Sparse Doubly-linked List.
  Copyleft © 2024 furious programming. All rights reversed.
  _______________________________________________________________________

  This is free and unencumbered software released into the public domain.

  Anyone is free to copy, modify, publish, use, compile, sell, or
  distribute this software, either in source code form or as a compiled
  binary, for any purpose, commercial or non-commercial, and by any
  means.

  In jurisdictions that recognize copyright laws, the author or authors
  of this software dedicate any and all copyright interest in the
  software to the public domain. We make this dedication for the benefit
  of the public at large and to the detriment of our heirs and
  successors. We intend this dedication to be an overt act of
  relinquishment in perpetuity of all present and future rights to this
  software under copyright law.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
  OTHER DEALINGS IN THE SOFTWARE.

  For more information, please refer to <http://unlicense.org/>
}

unit SparseList;

interface


type
  // Typed pointer data types.
  PSparseListSegment = ^TSparseListSegment; // A pointer to the segment node.
  PSparseListNode    = ^TSparseListNode;    // A pointer to the list node.
  PSparseList        = ^TSparseList;        // A pointer to the list structure.

  // A structure of a segment node.
  TSparseListSegment = record
    Next: PSparseListSegment; // A pointer to the next segment node.
    Data: record end;         // The beginning of the segment nodes' memory block (has name, address and zero-size).
  end;

  // A structure of a list node.
  TSparseListNode = record
    Prev: PSparseListNode; // A pointer to the previous node.
    Next: PSparseListNode; // A pointer to the next node.
    Data: record end;      // The beginning of the node's data memory block (has name, address and zero-size).
  end;

  // A structure of the list.
  TSparseList = record
    SegmentHead:    PSparseListSegment; // A pointer to the first segment node.
    BankHead:       PSparseListNode;    // A pointer to the first unused node (head of a singly-linked list of nodes).
    NodeHead:       PSparseListNode;    // A pointer to the first list node.
    NodeTail:       PSparseListNode;    // A pointer to the last list node.
    NodeNum:        Integer;            // The number of all list nodes.
    NodeNumSegment: Integer;            // The number of nodes on each segment.
    SizeData:       Integer;            // The data size of each node, in bytes.
    SizeNode:       Integer;            // The size of a single node, in bytes.
  end;

type
  // Callback comparing data of two nodes, for the purpose of sorting the list.
  TSparseListNodeCallbackCompare = function (ANodeA, ANodeB: PSparseListNode): Boolean;


  // Allocating and deallocating a list.
  function  SparseListCreate      (ASizeData, ANodeNumSegment: Integer): PSparseList; // Allocates a new list on the heap and initializes it.
  procedure SparseListDestroy     (AList: PSparseList); // Finalizes and deallocates the list from the heap.

  // Initializing and finalizing a list.
  procedure SparseListInitialize  (AList: PSparseList; ASizeData, ANodeNumSegment: Integer); // Initializes an existing list.
  procedure SparseListFinalize    (AList: PSparseList); // Finalizes an existing list.

  // Clearing the list.
  procedure SparseListClear       (AList: PSparseList); // Removes all nodes of the list.

  // Sorting the list.
  procedure SparseListSortBubble  (AList: PSparseList; ACallback: TSparseListNodeCallbackCompare); // Performs bubble sorting on the list.

  // Creating, destroying and managing nodes.
  function  SparseListNodeCreate  (AList: PSparseList): PSparseListNode; // Creates a new list node and returns it.
  procedure SparseListNodeDestroy (AList: PSparseList; ANode: PSparseListNode); // Removes a node from the list.
  procedure SparseListNodeExtract (AList: PSparseList; ANode: PSparseListNode); // Detaches the given node from the list.
  procedure SparseListNodeAppend  (AList: PSparseList; ANode: PSparseListNode); // Attaches an external node to the end of the list.
  procedure SparseListNodeInsert  (AList: PSparseList; ANode, ADest: PSparseListNode); // Inserts an external node in place of an existing one.


implementation


{
  Allocates a new list on the heap and initializes it.

  [i] If you have a list allocated on the stack, initialize it directly with the "SparseListInitialize" function.

  Parameters:
    • ASizeData       — the size of the data in each node in bytes, in range [1,n].
    • ANodeNumSegment — the number of nodes on each segment, in range [1,n].

  Result:
    • A non-nil pointer to an allocated and initialized list.
}
function SparseListCreate(ASizeData, ANodeNumSegment: Integer): PSparseList;
begin
  Result := GetMem(SizeOf(TSparseList));
  SparseListInitialize(Result, ASizeData, ANodeNumSegment);
end;


{
  Finalizes and deallocates the list from the heap.

  [i] If you have a list allocated on the stack, finalize it directly with the "SparseListFinalize" function.

  Parameters:
    • AList — a pointer to the structure of the list to finalize and deallocate.
}
procedure SparseListDestroy(AList: PSparseList);
begin
  SparseListFinalize(AList);
  FreeMem(AList);
end;


{
  Initializes an existing list.

  This function initializes the fields of the list structure. Calculates and remembers the data size of a single node and the
  size of each list node. The list is empty by default and does not contain any segments or nodes. The first segment will be
  allocated only after the first node is created using the "SparseListNodeCreate" function.

  [i] This function should be used to initialize a list allocated on the stack.

  Parameters:
    • AList           — a pointer to the structure of the list to initialize.
    • ASizeData       — the size of the data in each node in bytes, in range [1,n].
    • ANodeNumSegment — the number of nodes on each segment, in range [1,n].
}
procedure SparseListInitialize(AList: PSparseList; ASizeData, ANodeNumSegment: Integer);
begin
  AList^.SegmentHead    := nil;
  AList^.BankHead       := nil;
  AList^.NodeHead       := nil;
  AList^.NodeTail       := nil;
  AList^.NodeNum        := 0;
  AList^.NodeNumSegment := ANodeNumSegment;
  AList^.SizeData       := ASizeData;
  AList^.SizeNode       := ASizeData + SizeOf(TSparseListNode);
end;


{
  Finalizes an existing list.

  Since memory is not allocated for each node individually and all nodes physically exist within segments, freeing list data
  is limited to freeing all segments from memory. After the list segments are deallocated, pointers to all nodes become
  invalid and should not be used.

  [i] This function should be used to finalize a list allocated on the stack.

  Parameters:
    • AList — a pointer to the structure of the list to finalize.
}
procedure SparseListFinalize(AList: PSparseList);
var
  SegmentCurr: PSparseListSegment;
  SegmentNext: PSparseListSegment;
begin
  SegmentCurr := AList^.SegmentHead;

  // Deallocate all segments of the list, i.e. all dynamically allocated memory.
  while SegmentCurr <> nil do
  begin
    SegmentNext := SegmentCurr^.Next;
    FreeMem(SegmentCurr);
    SegmentCurr := SegmentNext;
  end;
end;


{
  Removes all nodes of the list.

  This function is used to remove all list nodes, which simply means moving the entire list to the bank and resetting the
  pointer and counter fields. Segments are never dynamically deallocated (they exist until the list is destroyed), so their
  unused nodes are in the bank waiting to be reused.

  Parameters:
    • AList — a pointer to the structure of the list.
}
procedure SparseListClear(AList: PSparseList);
begin
  // Do nothing if the list is empty.
  if AList^.NodeNum = 0 then exit;

  // Transfer the entire existing list to the bank and reset the fields.
  AList^.NodeTail^.Next := AList^.BankHead;
  AList^.BankHead       := AList^.NodeHead;
  AList^.NodeHead       := nil;
  AList^.NodeTail       := nil;
  AList^.NodeNum        := 0;
end;


{
  Performs bubble sorting on the list.

  This function is used to sort the contents of the list using the bubble sort algorithm. The swap only affects the data in
  the nodes, which means that the pointers in the nodes are not modified during sorting.

  [i] This function is implemented solely for the purposes of benchmarking the performance of access to nodes.

  Parameters:
    • AList     — a pointer to the structure of the list.
    • ACallback — a pointer to the callback function that compares the data of two nodes.
}
procedure SparseListSortBubble(AList: PSparseList; ACallback: TSparseListNodeCallbackCompare);
var
  NodeLast:    PSparseListNode;
  NodeCurr:    PSparseListNode;
  NodeData:    Pointer;
  NodeSwapped: Boolean;
begin
  // If there are not at least two nodes, there is nothing to sort.
  if AList^.NodeNum < 2 then exit;

  // Allocate the data block needed for node swap and set the sentinel.
  NodeData := GetMem(AList^.SizeData);
  NodeLast := AList^.NodeTail;

  repeat
    // Start a full iteration through the list always at the head node.
    NodeCurr    := AList^.NodeHead;
    NodeSwapped := False;

    // Iterate until a sentinel is encountered.
    repeat
      // Compare neighboring nodes and, if necessary, swap them (only data).
      if ACallback(NodeCurr, NodeCurr^.Next) then
      begin
        Move(NodeCurr^.Data,       NodeData^,            AList^.SizeData);
        Move(NodeCurr^.Next^.Data, NodeCurr^.Data,       AList^.SizeData);
        Move(NodeData^,            NodeCurr^.Next^.Data, AList^.SizeData);

        NodeSwapped := True;
      end;

      // Regardless of whether there was a swap or not, go to the next node.
      NodeCurr := NodeCurr^.Next;
    until NodeCurr = NodeLast;

    // Move the sentinel one node towards the head of the list to reduce the number of iterations.
    NodeLast := NodeLast^.Prev;
  until (NodeLast = AList^.NodeHead) or not NodeSwapped;

  // Free up a temporary data block for node data swap.
  FreeMem(NodeData);
end;


{
  Creates a new list node and returns it.

  This function is used to create and return a new node, which can then be attached to the list. The memory of the new node
  is not allocated, and instead it is checked whether there are already allocated and unused nodes in the bank. If the bank
  is empty, it means that all nodes from all currently allocated segments are attached to the list. In such case, a new
  segment is allocated and a chain of its nodes is built. Finally, the first node from the bank is returned.

  Since the bank is used like a stack (LIFO) and nodes are always pulled out and given back from and to the bank head,
  building a chain of nodes is limited only to setting a pointer to the next node in each segment node.

  [!] This function must be used for each new node you want to add to the list.

  [!] The node created by this function is an external node, i.e. it is neither in the bank nor attached to the list yet.
      Losing its references will not cause a memory leak, but it will not be able to be returned to the bank, so the
      segment's node pool will shrink by one node.

  Parameters:
    • AList — a pointer to the structure of the list.

  Result:
    • A non-nil pointer to the new node.
}
function SparseListNodeCreate(AList: PSparseList): PSparseListNode;
var
  SegmentHead: PSparseListSegment;
  NodeHead:    PSparseListNode;
  NodeTail:    PSparseListNode;
begin
  // Check if there are nodes in the bank and if not, allocate a new segment.
  if AList^.BankHead = nil then
  begin
    // Allocate a new node segment and connect it to the segment list.
    SegmentHead       := GetMem(SizeOf(TSparseListSegment) + AList^.SizeNode * AList^.NodeNumSegment);
    SegmentHead^.Next := AList^.SegmentHead;

    // Update the head of the segment list and set the segment bank to its first node.
    AList^.SegmentHead := SegmentHead;
    AList^.BankHead    := @SegmentHead^.Data;

    // Get a pointer to the first and last node in the segment data block.
    NodeHead := @SegmentHead^.Data;
    NodeTail := Pointer(NodeHead) + (AList^.NodeNumSegment - 1) * AList^.SizeNode;

    // For each node in the node's data memory block, initialize a link to the next node in that segment. In this way, a
    // singly-linked list of all segment nodes is created, available in the bank.
    while NodeHead < NodeTail do
    begin
      NodeHead^.Next    := Pointer(NodeHead) + AList^.SizeNode;
      Pointer(NodeHead) += AList^.SizeNode;
    end;

    // Clear the link to the next node in the last node (the next one does not exist).
    NodeHead^.Next := nil;
  end;

  // Pull out the first available node from the bank and return it.
  Result          := AList^.BankHead;
  AList^.BankHead := AList^.BankHead^.Next;
end;


{
  Removes a node from the list.

  This function is used to destroy an external node. The node is not actually freed from memory because it is part of the
  entire segment. Destroying a node means returning it to the bank.

  [!] Never destroy a node that is not external. First detach it from the list using the "SparseListNodeExtract" function
      and then release it using the function below.

  Parameters:
    • AList — a pointer to the structure of the list.
    • ANode — a pointer to the list node to destroy.
}
procedure SparseListNodeDestroy(AList: PSparseList; ANode: PSparseListNode);
begin
  // Just return the node to the bank (update the head node of the bank).
  ANode^.Next     := AList^.BankHead;
  AList^.BankHead := ANode;
end;


{
  Detaches the given node from the list.

  This function is used to detach a node from the list, turning it into an external node. The external node can then be used
  to insert it elsewhere in the list or to be destroyed.

  [!] The node detached by this function becomes an external node, i.e. it is neither in the bank nor attached to the list
      yet. Losing its references will not cause a memory leak, but it will not be able to be returned to the bank, so the
      segment's node pool will shrink by one node.

  Parameters:
    • AList — a pointer to the structure of the list.
    • ANode — a pointer to an external node to detach from the list.
}
procedure SparseListNodeExtract(AList: PSparseList; ANode: PSparseListNode);
begin
  // If the node to be detached is not the head of the list, update the link of the previous node.
  // Otherwise, update the pointer to the head of the list.
  if ANode^.Prev <> nil then
    ANode^.Prev^.Next := ANode^.Next
  else
    AList^.NodeHead := ANode^.Next;

  // If the node to be detached is not the tail of the list, update the link of the next node.
  // Otherwise, update the pointer to the tail of the list.
  if ANode^.Next <> nil then
    ANode^.Next^.Prev := ANode^.Prev
  else
    AList^.NodeTail := ANode^.Prev;

  // The node has been detached, so decrement the number of nodes in the list.
  AList^.NodeNum -= 1;
end;


{
  Attaches an external node to the end of the list.

  This function is used to connect an external node to the end of an existing list. The number of existing nodes in the list
  does not matter — it can be used to add a new node to an empty or non-empty list.

  [!] Never attempt to use this function to attach a node to a list that is already attached to it. Otherwise, the links in
      the list nodes will be broken and the list itself will no longer be coherent (generally, it will be UB).

  [i] If you need to insert a node anywhere in the list, use the "SparseListNodeInsert" function.

  Parameters:
    • AList — a pointer to the structure of the list.
    • ANode — a pointer to an external node to attach to the list.
}
procedure SparseListNodeAppend(AList: PSparseList; ANode: PSparseListNode);
begin
  // Set the links in the node to attach.
  ANode^.Prev := AList^.NodeTail;
  ANode^.Next := nil;

  // If the list is not empty, attach a node to the end of the list and update the link in the existing tail.
  // Otherwise (if the list is empty), the new node will become its head and tail.
  if AList^.NodeTail <> nil then
    AList^.NodeTail^.Next := ANode
  else
    AList^.NodeHead := ANode;

  // Update the pointer to the tail of the list and increment the number of list nodes.
  AList^.NodeTail := ANode;
  AList^.NodeNum  += 1;
end;


{
  Inserts an external node in place of an existing one.

  This function is used to insert an external node in place of an existing one, moving the dest node towards the end of the
  list. The dest node can be any node that is already attached to the list, from head to tail. Inserting a new node in place
  of the head means creating a new head (the new node will be the first node of the list). Inserting a new node in place of
  the tail does not modify the tail data (the new node will be the penultimate node in the list).

  [!] Never attempt to use this function to attach a node to a list that is already attached to it. Otherwise, the links in
      the list nodes will be broken and the list itself will no longer be coherent (generally, it will be UB).

  [i] If you need to add a node to the end of the list, use the "SparseListNodeAppend" function.

  Parameters:
    • AList — a pointer to the structure of the list.
    • ANode — a pointer to an external node to attach to the list.
}
procedure SparseListNodeInsert(AList: PSparseList; ANode, ADest: PSparseListNode);
begin
  // If the target node is not the head of the list, update its link to the next node.
  // Otherwise, the new node becomes the new head of the list.
  if ADest^.Prev <> nil then
    ADest^.Prev^.Next := ANode
  else
    AList^.NodeHead := ANode;

  // Attach the new node to the dest node and its predecessor.
  ANode^.Prev := ADest^.Prev;
  ANode^.Next := ADest;
  ADest^.Prev := ANode;

  // The new node has been attached, so increment the list node counter.
  AList^.NodeNum += 1;
end;


end.

