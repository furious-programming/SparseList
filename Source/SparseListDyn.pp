
{
  Sparse Dynamic Doubly-linked List.
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

unit SparseListDyn;

interface


type
  // Typed pointer data types.
  PSparseListDynSegment = ^TSparseListDynSegment; // A pointer to the segment node.
  PSparseListDynNode    = ^TSparseListDynNode;    // A pointer to the list node.
  PSparseListDyn        = ^TSparseListDyn;        // A pointer to the list structure.

  // A structure of a segment node.
  TSparseListDynSegment = record
    Prev: PSparseListDynSegment; // A pointer to the previous segment node.
    Next: PSparseListDynSegment; // A pointer to the next segment node.
    Bank: PSparseListDynNode;    // A pointer to the first unused node of the segment (head of a singly-linked list of nodes).
    Used: Integer;               // The number of all segment nodes currently in use.
    Data: record end;            // The beginning of the segment nodes' memory block (has name, address and zero-size).
  end;

  // A structure of a list node.
  TSparseListDynNode = record
    Segment: PSparseListDynSegment; // A pointer to the segment node to which the node belongs.
    Prev:    PSparseListDynNode;    // A pointer to the previous node.
    Next:    PSparseListDynNode;    // A pointer to the next node.
    Data:    record end;            // The beginning of the node's data memory block (has name, address and zero-size).
  end;

  // A structure of the list.
  TSparseListDyn = record
    SegmentHead:    PSparseListDynSegment; // A pointer to the first segment node.
    SegmentTail:    PSparseListDynSegment; // A pointer to the last segment node.
    SegmentNum:     Integer;               // The number of all currently allocated segments.
    NodeHead:       PSparseListDynNode;    // A pointer to the first list node.
    NodeTail:       PSparseListDynNode;    // A pointer to the last list node.
    NodeNum:        Integer;               // The number of all list nodes.
    NodeNumSegment: Integer;               // The number of nodes on each segment.
    SizeData:       Integer;               // The data size of each node, in bytes.
    SizeNode:       Integer;               // The size of a single node, in bytes.
  end;


  // Allocating and deallocating a list.
  function  SparseListDynCreate      (ASizeData, ANodeNumSegment: Integer): PSparseListDyn; // Allocates a new list on the heap and initializes it.
  procedure SparseListDynDestroy     (AList: PSparseListDyn); // Finalizes and deallocates the list from the heap.

  // Initializing and finalizing a list.
  procedure SparseListDynInitialize  (AList: PSparseListDyn; ASizeData, ANodeNumSegment: Integer); // Initializes an existing list.
  procedure SparseListDynFinalize    (AList: PSparseListDyn); // Finalizes an existing list.

  // Clearing the list.
  procedure SparseListDynClear       (AList: PSparseListDyn); // Removes all nodes of the list.

  // Creating, destroying and managing nodes.
  function  SparseListDynNodeCreate  (AList: PSparseListDyn): PSparseListDynNode; // Creates a new list node and returns it.
  procedure SparseListDynNodeDestroy (AList: PSparseListDyn; ANode: PSparseListDynNode); // Removes a node from the list.
  procedure SparseListDynNodeExtract (AList: PSparseListDyn; ANode: PSparseListDynNode); // Detaches the given node from the list.
  procedure SparseListDynNodeAppend  (AList: PSparseListDyn; ANode: PSparseListDynNode); // Attaches an external node to the end of the list.
  procedure SparseListDynNodeInsert  (AList: PSparseListDyn; ANode, ADest: PSparseListDynNode); // Inserts an external node in place of an existing one.


implementation


  // Allocating and deallocating list segments.
  function  SparseListDynSegmentCreate  (AList: PSparseListDyn): PSparseListDynSegment; forward; // Allocates a new segment and joins it to the segment list.
  procedure SparseListDynSegmentDestroy (AList: PSparseListDyn; ASegment: PSparseListDynSegment); forward; // Detaches a given segment from the segment list and deallocates it.


{
  Allocates a new list on the heap and initializes it.

  [i] If you have a list allocated on the stack, initialize it directly with the "SparseListDynInitialize" function.

  Parameters:
    • ASizeData       — the size of the data in each node in bytes, in range [1,n].
    • ANodeNumSegment — the number of nodes on each segment, in range [1,n].

  Result:
    • A non-nil pointer to an allocated and initialized list.
}
function SparseListDynCreate(ASizeData, ANodeNumSegment: Integer): PSparseListDyn;
begin
  Result := GetMem(SizeOf(TSparseListDyn));
  SparseListDynInitialize(Result, ASizeData, ANodeNumSegment);
end;


{
  Finalizes and deallocates the list from the heap.

  [i] If you have a list allocated on the stack, finalize it directly with the "SparseListDynFinalize" function.

  Parameters:
    • AList — a pointer to the structure of the list to finalize and deallocate.
}
procedure SparseListDynDestroy(AList: PSparseListDyn);
begin
  SparseListDynFinalize(AList);
  FreeMem(AList);
end;


{
  Initializes an existing list.

  This function initializes the fields of the list structure. Calculates and remembers the data size of a single node and the
  size of each list node. The list is empty by default and does not contain any segments or nodes. The first segment will be
  allocated only after the first node is created using the "SparseListDynNodeCreate" function.

  [i] This function should be used to initialize a list allocated on the stack.

  Parameters:
    • AList           — a pointer to the structure of the list to initialize.
    • ASizeData       — the size of the data in each node in bytes, in range [1,n].
    • ANodeNumSegment — the number of nodes on each segment, in range [1,n].
}
procedure SparseListDynInitialize(AList: PSparseListDyn; ASizeData, ANodeNumSegment: Integer);
begin
  AList^.SegmentHead    := nil;
  AList^.SegmentTail    := nil;
  AList^.SegmentNum     := 0;
  AList^.NodeHead       := nil;
  AList^.NodeTail       := nil;
  AList^.NodeNum        := 0;
  AList^.NodeNumSegment := ANodeNumSegment;
  AList^.SizeData       := ASizeData;
  AList^.SizeNode       := ASizeData + SizeOf(TSparseListDynNode);
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
procedure SparseListDynFinalize(AList: PSparseListDyn);
var
  SegmentCurr: PSparseListDynSegment;
  SegmentNext: PSparseListDynSegment;
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

  This function is used to remove all list nodes, which is limited to the deallocation of all existing list segments. When
  segments are destroyed, the segment and node pointers and the node counter are reset so that the list appears empty and
  ready for further use. If the list is already empty, this function does nothing.

  Parameters:
    • AList — a pointer to the structure of the list.
}
procedure SparseListDynClear(AList: PSparseListDyn);
begin
  // Do nothing if the list is empty.
  if AList^.NodeNum = 0 then exit;

  // Deallocate all existing list segments and thus the memory of all used and unused nodes.
  SparseListDynFinalize(AList);

  // Reset list fields.
  AList^.SegmentHead := nil;
  AList^.SegmentTail := nil;
  AList^.SegmentNum  := 0;
  AList^.NodeHead    := nil;
  AList^.NodeTail    := nil;
  AList^.NodeNum     := 0;
end;


{
  Creates a new list node and returns it.

  This function is used to return the first available unused node. If the list already has segments allocated, it scans them
  for an unused node, iterating from the head to the tail of the segment list. Once it finds a segment that has an unused
  node, it pulls it out of the bank, returns it, and terminates.

  If the list has no segments allocated (it is empty), or all nodes of all segments are currently in use (all of them are
  attached to the list), this function allocates and initializes the new segment, attaches it to the segment list, then pulls
  the node from its bank and returns it.

  [!] This function must be used for each new node you want to add to the list.

  [!] The node created by this function is an external node, i.e. it is neither in the segment bank nor attached to the list
      yet. Losing its references will not cause a memory leak, but it will not be able to be returned to the segment bank, so
      the segment's node pool will shrink by one node.

  Parameters:
    • AList — a pointer to the structure of the list.

  Result:
    • A non-nil pointer to the new node.
}
function SparseListDynNodeCreate(AList: PSparseListDyn): PSparseListDynNode;
var
  Segment: PSparseListDynSegment;
begin
  // Start looking for an unused node from the first segment.
  Segment := AList^.SegmentHead;

  while Segment <> nil do
    // Check if the current segment contains at least one unused node and if so, return it.
    if Segment^.Bank <> nil then
    begin
      // Pull a node out of the bank head and increment the number of nodes used.
      Result        := Segment^.Bank;
      Segment^.Bank := Segment^.Bank^.Next;
      Segment^.Used += 1;

      // An unused node was found in an existing segment, so exit.
      exit;
    end
    else
      // The current segment has no unused nodes, so go to the next segment.
      Segment := Segment^.Next;

  // There is no available segment or none of the existing segments contain unused nodes, so allocate a new one.
  Segment := SparseListDynSegmentCreate(AList);

  // Extract the first node from the new segment bank and return it.
  Result        := Segment^.Bank;
  Segment^.Bank := Segment^.Bank^.Next;
  Segment^.Used += 1;
end;


{
  Removes a node from the list.

  This function is used to destroy an external node. The node is not actually freed from memory because it is part of the
  entire segment. Destroying a node means returning it to the bank of the segment to which it belongs. If the node being
  removed was the last one used from a given segment, this segment is detached from the segment list and released from memory.

  [!] Never destroy a node that is not external. First detach it from the list using the "SparseListDynNodeExtract" function
      and then release it using the function below.

  Parameters:
    • AList — a pointer to the structure of the list.
    • ANode — a pointer to the list node to destroy.
}
procedure SparseListDynNodeDestroy(AList: PSparseListDyn; ANode: PSparseListDynNode);
begin
  // Return a node to the segment bank it belongs to and decrement the number of nodes in use for that segment.
  ANode^.Next          := ANode^.Segment^.Bank;
  ANode^.Segment^.Bank := ANode;
  ANode^.Segment^.Used -= 1;

  // If all nodes of a segment are not in use, free that segment from memory.
  if ANode^.Segment^.Used = 0 then
    SparseListDynSegmentDestroy(AList, ANode^.Segment);
end;


{
  Detaches the given node from the list.

  This function is used to detach a node from the list, turning it into an external node. The external node can then be used
  to insert it elsewhere in the list or to be destroyed.

  [!] The node detached by this function becomes an external node, i.e. it is neither in the segment bank nor attached to
      the list yet. Losing its references will not cause a memory leak, but it will not be able to be returned to the segment
      bank, so the segment's node pool will shrink by one node.

  Parameters:
    • AList — a pointer to the structure of the list.
    • ANode — a pointer to an external node to detach from the list.
}
procedure SparseListDynNodeExtract(AList: PSparseListDyn; ANode: PSparseListDynNode);
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

  [i] If you need to insert a node anywhere in the list, use the "SparseListDynNodeInsert" function.

  Parameters:
    • AList — a pointer to the structure of the list.
    • ANode — a pointer to an external node to attach to the list.
}
procedure SparseListDynNodeAppend(AList: PSparseListDyn; ANode: PSparseListDynNode);
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

  [i] If you need to add a node to the end of the list, use the "SparseListDynNodeAppend" function.

  Parameters:
    • AList — a pointer to the structure of the list.
    • ANode — a pointer to an external node to attach to the list.
}
procedure SparseListDynNodeInsert(AList: PSparseListDyn; ANode, ADest: PSparseListDynNode);
begin
  // If the target node is not the head of the list, update its link to the next node.
  if ADest^.Prev <> nil then
    ADest^.Prev^.Next := ANode;

  // Attach the new node to the dest node and its predecessor.
  ANode^.Prev := ADest^.Prev;
  ANode^.Next := ADest;
  ADest^.Prev := ANode;

  // If the new node was inserted at the beginning of the list, update the head pointer.
  if AList^.NodeHead = ADest then
    AList^.NodeHead := ANode;

  // The new node has been attached, so increment the list node counter.
  AList^.NodeNum += 1;
end;


{
  Allocates a new segment and joins it to the segment list.

  This is an internal (non-public) function used to allocate a new segment, initialize it and attach it to the end of the
  current segment list (it becomes the new tail of the segment list). Finally, the segment data block is converted into a
  chain of nodes, which will be available from the bank level.

  Since the bank is used like a stack (LIFO) and segment nodes are always pulled out and given back from and to the bank
  head, building a chain of nodes is limited only to setting a pointer to the next node in each segment node.

  Parameters:
    • AList — a pointer to the structure of the list.

  Result:
    • A non-nil pointer to the allocated segment.
}
function SparseListDynSegmentCreate(AList: PSparseListDyn): PSparseListDynSegment;
var
  NodeHead: PSparseListDynNode;
  NodeTail: PSparseListDynNode;
begin
  // Allocate a new segment and initialize its fields.
  Result       := GetMem(SizeOf(TSparseListDynSegment) + AList^.SizeNode * AList^.NodeNumSegment);
  Result^.Prev := AList^.SegmentTail;
  Result^.Next := nil;
  Result^.Bank := @Result^.Data;
  Result^.Used := 0;

  // If the list already contains segments, update the link in the predecessor.
  if Result^.Prev <> nil then
    Result^.Prev^.Next := Result;

  // The allocated segment is the new tail, so update the pointer to the tail of the segment list.
  AList^.SegmentTail := Result;
  AList^.SegmentNum  += 1;

  // If the list does not contain any segments (is empty), the allocated segment also becomes the head.
  if AList^.SegmentHead = nil then
    AList^.SegmentHead := Result;

  // Get a pointer to the first and last node in the segment data block.
  NodeHead := @Result^.Data;
  NodeTail := Pointer(NodeHead) + (AList^.NodeNumSegment - 1) * AList^.SizeNode;

  // For each node in the node's data memory block, initialize a pointer to the segment it belongs to, as well as a link to
  // the next node in that segment. In this way, a singly-linked list of all segment nodes is created, available in the bank.
  while NodeHead < NodeTail do
  begin
    NodeHead^.Segment := Result;
    NodeHead^.Next    := Pointer(NodeHead) + AList^.SizeNode;

    Pointer(NodeHead) += AList^.SizeNode;
  end;

  // Set the pointer to the segment on the last node and clear its link to the next node (the next one does not exist).
  NodeHead^.Segment := Result;
  NodeHead^.Next    := nil;
end;


{
  Detaches a given segment from the segment list and deallocates it.

  This function is used to detach a segment from the segment list and release it from memory. Depending on how many segments
  are currently allocated, deallocation may modify the head and tail data of the segment list in the list structure.

  [!] Never attempt to unload a segment whose nodes are currently attached to the list. Otherwise, the used nodes of such a
      segment will start pointing to unallocated memory, which will cause a segmentation fault when trying to read the node's
      data (link pointers or the actual node's payload).

  Parameters:
    • AList    — a pointer to the structure of the list.
    • ASegment — a pointer to the segment to deallocate.
}
procedure SparseListDynSegmentDestroy(AList: PSparseListDyn; ASegment: PSparseListDynSegment);
begin
  // If the segment to be deallocated is not the head of the segment list, update the link in the previous segment.
  // Otherwise, update the pointer to the head of the segment list.
  if ASegment^.Prev <> nil then
    ASegment^.Prev^.Next := ASegment^.Next
  else
    AList^.SegmentHead := ASegment^.Next;

  // If the segment to be deallocated is not the tail of the segment list, update the link in the next segment. Otherwise,
  // update the pointer to the tail of the segment list.
  if ASegment^.Next <> nil then
    ASegment^.Next^.Prev := ASegment^.Prev
  else
    AList^.SegmentTail := ASegment^.Prev;

  // Free the segment from memory and decrement the number of list segments.
  FreeMem(ASegment);
  AList^.SegmentNum -= 1;
end;


end.

