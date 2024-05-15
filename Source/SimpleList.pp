
{
  Simple Doubly-linked List.
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

unit SimpleList;

  // Global compiler switches.
  {$INCLUDE TestSwitches.inc}

interface


type
  // Typed pointer data types.
  PSimpleListNode = ^TSimpleListNode; // A pointer to the list node.
  PSimpleList     = ^TSimpleList;     // A pointer to the list structure.

  // A structure of a list node.
  TSimpleListNode = record
    Prev: PSimpleListNode; // A pointer to the previous node.
    Next: PSimpleListNode; // A pointer to the next node.
    Data: record end;      // The beginning of the node's data memory block (has name, address and zero-size).
  end;

  // A structure of the list.
  TSimpleList = record
    NodeHead: PSimpleListNode; // A pointer to the first list node.
    NodeTail: PSimpleListNode; // A pointer to the last list node.
    NodeNum:  Integer;         // The number of all list nodes.
    SizeData: Integer;         // The data size of each node, in bytes.
  end;

type
  // Callback comparing data of two nodes, for the purpose of sorting the list.
  TSimpleListNodeCallbackCompare = function (ANodeA, ANodeB: PSimpleListNode): Boolean;


  // Allocating and deallocating a list.
  function  SimpleListCreate      (ASizeData: Integer): PSimpleList; // Allocates a new list on the heap and initializes it.
  procedure SimpleListDestroy     (AList: PSimpleList); // Finalizes and deallocates the list from the heap.

  // Initializing and finalizing a list.
  procedure SimpleListInitialize  (AList: PSimpleList; ASizeData: Integer); // Initializes an existing list.
  procedure SimpleListFinalize    (AList: PSimpleList); // Finalizes an existing list.

  // Clearing the list.
  procedure SimpleListClear       (AList: PSimpleList); // Removes all nodes of the list.

  // Sorting the list.
  procedure SimpleListSortBubble  (AList: PSimpleList; ACallback: TSimpleListNodeCallbackCompare); // Performs bubble sorting on the list.

  // Creating, destroying and managing nodes.
  function  SimpleListNodeCreate  (AList: PSimpleList): PSimpleListNode; // Creates a new list node and returns it.
  procedure SimpleListNodeDestroy (AList: PSimpleList; ANode: PSimpleListNode); // Removes a node from the list.
  procedure SimpleListNodeExtract (AList: PSimpleList; ANode: PSimpleListNode); // Detaches the given node from the list.
  procedure SimpleListNodeAppend  (AList: PSimpleList; ANode: PSimpleListNode); // Attaches an external node to the end of the list.
  procedure SimpleListNodeInsert  (AList: PSimpleList; ANode, ADest: PSimpleListNode); // Inserts an external node in place of an existing one.


implementation


{
  Allocates a new list on the heap and initializes it.

  [i] If you have a list allocated on the stack, initialize it directly with the "SimpleListInitialize" function.

  Parameters:
    • ASizeData — the size of the data in each node in bytes, in range [1,n].

  Result:
    • A non-nil pointer to an allocated and initialized list.
}
function SimpleListCreate(ASizeData: Integer): PSimpleList;
begin
  Result := GetMem(SizeOf(TSimpleList));
  SimpleListInitialize(Result, ASizeData);
end;


{
  Finalizes and deallocates the list from the heap.

  [i] If you have a list allocated on the stack, finalize it directly with the "SimpleListFinalize" function.

  Parameters:
    • AList — a pointer to the structure of the list to finalize and deallocate.
}
procedure SimpleListDestroy(AList: PSimpleList);
begin
  SimpleListFinalize(AList);
  FreeMem(AList);
end;


{
  Initializes an existing list.

  [i] This function should be used to initialize a list allocated on the stack.

  Parameters:
    • AList     — a pointer to the structure of the list to initialize.
    • ASizeData — the size of the data in each node in bytes, in range [1,n].
}
procedure SimpleListInitialize(AList: PSimpleList; ASizeData: Integer);
begin
  AList^.NodeHead := nil;
  AList^.NodeTail := nil;
  AList^.NodeNum  := 0;
  AList^.SizeData := ASizeData;
end;


{
  Finalizes an existing list.

  Since memory is allocated for each node individually, it simply frees all nodes in the list from memory. This function does
  virtually the same thing as "SimpleListClear", except that it does not reset the fields in the list structure.

  [i] This function should be used to finalize a list allocated on the stack.

  Parameters:
    • AList — a pointer to the structure of the list to finalize.
}
procedure SimpleListFinalize(AList: PSimpleList);
var
  NodeCurr: PSimpleListNode;
  NodeNext: PSimpleListNode;
begin
  NodeCurr := AList^.NodeHead;

  // Deallocate all nodes separately.
  while NodeCurr <> nil do
  begin
    NodeNext := NodeCurr^.Next;
    FreeMem(NodeCurr);
    NodeCurr := NodeNext;
  end;
end;


{
  Removes all nodes of the list.

  This function is used to remove all existing list nodes, which means iterating through all the nodes and freeing each of
  them from memory.

  Parameters:
    • AList — a pointer to the structure of the list.
}
procedure SimpleListClear(AList: PSimpleList);
var
  NodeCurr: PSimpleListNode;
  NodeNext: PSimpleListNode;
begin
  NodeCurr := AList^.NodeHead;

  // Deallocate all nodes separately.
  while NodeCurr <> nil do
  begin
    NodeNext := NodeCurr^.Next;
    FreeMem(NodeCurr);
    NodeCurr := NodeNext;
  end;

  // Reset list fields.
  AList^.NodeHead := nil;
  AList^.NodeTail := nil;
  AList^.NodeNum  := 0;
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
procedure SimpleListSortBubble(AList: PSimpleList; ACallback: TSimpleListNodeCallbackCompare);
var
  NodeLast:    PSimpleListNode;
  NodeCurr:    PSimpleListNode;
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

  This function is used to create and return a new node, which can then be attached to the list. Because this linked list is
  implemented in a classic way, memory for each node is allocated separately.

  [!] This function must be used for each new node you want to add to the list.

  [!] The node created by this function is an external node, i.e. it is not attached to the list yet. Losing its references
      will cause a memory leak, so be aware of this.

  Parameters:
    • AList — a pointer to the structure of the list.

  Result:
    • A non-nil pointer to the new node.
}
function SimpleListNodeCreate(AList: PSimpleList): PSimpleListNode;
begin
  Result := GetMem(SizeOf(TSimpleListNode) + AList^.SizeData);
end;


{
  Removes a node from the list.

  This function is used to destroy an external node. Because this linked list is implemented in a classic way, memory for
  each node is deallocated separately.

  [!] Never destroy a node that is not external. First detach it from the list using the "SimpleListNodeExtract" function
      and then release it using the function below.

  Parameters:
    • AList — a pointer to the structure of the list.
    • ANode — a pointer to the list node to destroy.
}
procedure SimpleListNodeDestroy(AList: PSimpleList; ANode: PSimpleListNode);
begin
  FreeMem(ANode);
end;


{
  Detaches the given node from the list.

  This function is used to detach a node from the list, turning it into an external node. The external node can then be used
  to insert it elsewhere in the list or to be destroyed.

  [!] The node detached by this function becomes an external node, i.e. it is not attached to the list yet. Losing its
      reference will cause a memory leak, so be aware of this.

  Parameters:
    • AList — a pointer to the structure of the list.
    • ANode — a pointer to an external node to detach from the list.
}
procedure SimpleListNodeExtract(AList: PSimpleList; ANode: PSimpleListNode);
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

  [i] If you need to insert a node anywhere in the list, use the "SimpleListNodeInsert" function.

  Parameters:
    • AList — a pointer to the structure of the list.
    • ANode — a pointer to an external node to attach to the list.
}
procedure SimpleListNodeAppend(AList: PSimpleList; ANode: PSimpleListNode);
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

  [i] If you need to add a node to the end of the list, use the "SimpleListNodeAppend" function.

  Parameters:
    • AList — a pointer to the structure of the list.
    • ANode — a pointer to an external node to attach to the list.
}
procedure SimpleListNodeInsert(AList: PSimpleList; ANode, ADest: PSimpleListNode);
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

