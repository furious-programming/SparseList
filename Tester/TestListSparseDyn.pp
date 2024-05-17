
{
  Sparse Dynamic Doubly-linked List Tester.
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

unit TestListSparseDyn;

  // Global compiler switches.
  {$INCLUDE TestSwitches.inc}

interface

uses
  ListSparseDyn;


  // Measuring specific list functions.
  function TestListSparseDynAppend   (AList: PListSparseDyn; ANodeNum: Int32): Int64; // Measures the speed of adding a given number of nodes to a list.
  function TestListSparseDynInsert   (AList: PListSparseDyn): Int64; // Measures the speed of inserting a given number of nodes into a list.
  function TestListSparseDynChop     (AList: PListSparseDyn): Int64; // Measures the speed of removing nodes from a list.
  function TestListSparseDynSort     (AList: PListSparseDyn): Int64; // Measures the speed of sorting nodes in a list.
  function TestListSparseDynClear    (AList: PListSparseDyn): Int64; // Measures the speed of list cleaning.
  function TestListSparseDynTraverse (AList: PListSparseDyn): Int64; // Measures the speed of iterating over the nodes of a list.
  function TestListSparseDynDestroy  (AList: PListSparseDyn): Int64; // Measures the speed of cleaning and destroying the list.


implementation

uses
  TestUtils;


{
  Measures the speed of adding a given number of nodes to a list.

  This function measures the duration of the operation of adding a given number of nodes to the list. Data for the list nodes
  are generated based on the 32-bit Xorshift algorithm, so the generated numbers are relatively random and unordered for the
  later list sorting test.

  [!] For the sorting test to make sense, item data for all tested containers must be generated in the same way. This ensures
      that all sorting functions perform the same number of iterations.

  Arguments:
    • AList    — a pointer to the list structure.
    • ADataNum — the number of nodes to add to the list.

  Result:
    • The number of ticks on the hardware clock as the duration of the operation.
}
function TestListSparseDynAppend(AList: PListSparseDyn; ANodeNum: Int32): Int64;
var
  NodeSeed: UInt32 = $600D5EED;
begin
  // Remember the time when the test started.
  Result := TestGetTicks();

  while ANodeNum > 0 do
  begin
    // Add node with data to the end of the list.
    ListSparseDynNodeAppend(AList, ListSparseDynNodeCreate(AList));
    PUInt32(@AList^.NodeTail^.Data)^ := NodeSeed;

    // Generate another pseudo-random number according to the Xorshift algorithm.
    NodeSeed := NodeSeed xor (NodeSeed shl 13);
    NodeSeed := NodeSeed xor (NodeSeed shr 17);
    NodeSeed := NodeSeed xor (NodeSeed shl  5);

    ANodeNum -= 1;
  end;

  // Calculate how many ticks the operation took.
  Result := TestGetTicks() - Result;
end;


{
  Measures the speed of inserting a given number of nodes into a list.

  This function iterates over all list nodes and inserts new nodes in place of every second existing node. The result of this
  function is to increase the number of nodes by one third and to insert nodes with pseudo-random data into it for the purpose
  of a further sorting test.

  [!] For the sorting test to make sense, item data for all tested containers must be generated in the same way. This ensures
      that all sorting functions perform the same number of iterations.

  Arguments:
    • AList — a pointer to the list structure.

  Result:
    • The number of ticks on the hardware clock as the duration of the operation.
}
function TestListSparseDynInsert(AList: PListSparseDyn): Int64;
var
  NodeNew:    PListSparseDynNode;
  NodeCurr:   PListSparseDynNode;
  NodeInsert: Int32  = 0;
  NodeSeed:   UInt32 = $BAD5EED;
begin
  // Remember the time when the test started.
  Result   := TestGetTicks();
  NodeCurr := AList^.NodeHead;

  // Iterate until the end of the list.
  while NodeCurr <> nil do
  begin
    // If it's time, insert a new node into the list.
    if NodeInsert = 0 then
    begin
      NodeNew := ListSparseDynNodeCreate(AList);
      PUInt32(@NodeNew^.Data)^ := NodeSeed;

      ListSparseDynNodeInsert(AList, NodeNew, NodeCurr);
    end;

    // Generate another pseudo-random number according to the Xorshift algorithm.
    NodeSeed := NodeSeed xor (NodeSeed shl 13);
    NodeSeed := NodeSeed xor (NodeSeed shr 17);
    NodeSeed := NodeSeed xor (NodeSeed shl  5);

    // Go to the next node and update the counter that determines when to insert a node.
    NodeCurr   := NodeCurr^.Next;
    NodeInsert := (NodeInsert + 1) mod 2;
  end;

  // Calculate how many ticks the operation took.
  Result := TestGetTicks - Result;
end;


{
  Measures the speed of removing nodes from a list.

  This function iterates from the beginning to the end of the list and removes every second node from it.

  Arguments:
    • AList — a pointer to the list structure.

  Result:
    • The number of ticks on the hardware clock as the duration of the operation.
}
function TestListSparseDynChop(AList: PListSparseDyn): Int64;
var
  Node:     PListSparseDynNode;
  NodeNext: PListSparseDynNode;
  NodeChop: Int32 = 0;
begin
  // Remember the time when the test started.
  Result := TestGetTicks();
  Node   := AList^.NodeHead;

  // Iterate until the end of the list.
  while Node <> nil do
  begin
    NodeNext := Node^.Next;

    // If it's time, delete a node from the list.
    if NodeChop = 0 then
    begin
      ListSparseDynNodeExtract(AList, Node);
      ListSparseDynNodeDestroy(AList, Node);
    end;

    // Go to the next node and update the counter that determines when to delete a node.
    Node     := NodeNext;
    NodeChop := (NodeChop + 1) mod 2;
  end;

  // Calculate how many ticks the operation took.
  Result := TestGetTicks - Result;
end;


  {
    Compares data of two list nodes.

    Arguments:
      • ADataA — pointer to the first node's data.
      • ADataB — pointer to the second node's data.

    Result:
      • False — node data will not be swapped.
      • True  — node data will be swapped (will be moved towards the end of the list).
  }
  function TestSparseListDynSortNodes(ANodeA, ANodeB: PListSparseDynNode): Boolean;
  begin
    Result := PUInt32(@ANodeA^.Data)^ > PUInt32(@ANodeB^.Data)^;
  end;


{
  Measures the speed of sorting nodes in a list.

  Arguments:
    • AList — a pointer to the list structure.

  Result:
    • The number of ticks on the hardware clock as the duration of the operation.
}
function TestListSparseDynSort(AList: PListSparseDyn): Int64;
begin
  // Remember the time when the test started.
  Result := TestGetTicks();

  // Sort the list content using the bubble sort algorithm.
  ListSparseDynSortBubble(AList, @TestSparseListDynSortNodes);

  // Calculate how many ticks the operation took.
  Result := TestGetTicks() - Result;
end;


{
  Measures the speed of list cleaning.

  Arguments:
    • AList — a pointer to the list structure.

  Result:
    • The number of ticks on the hardware clock as the duration of the operation.
}
function TestListSparseDynClear(AList: PListSparseDyn): Int64;
begin
  // Remember the time when the test started.
  Result := TestGetTicks();

  // Clear the list.
  ListSparseDynClear(AList);

  // Calculate how many ticks the operation took.
  Result := TestGetTicks() - Result;
end;


{
  Measures the speed of iterating over the nodes of a list.

  This function traverses the list in both directions, testing the CPU's caching ability.

  Arguments:
    • AList — a pointer to the list structure.

  Result:
    • The number of ticks on the hardware clock as the duration of the operation.
}
function TestListSparseDynTraverse(AList: PListSparseDyn): Int64;
var
  Node: PListSparseDynNode;
  Num:  Int32 = 0;
begin
  // Remember the time when the test started.
  Result := TestGetTicks();
  Node   := AList^.NodeHead;

  // Iterate from the first to the last list node.
  while Node <> nil do
  begin
    // If an odd number is encountered, increment the dummy counter.
    if PUInt32(@Node^.Data)^ and 1 = 1 then
      Num += 1;

    // Go to the next node.
    Node := Node^.Next;
  end;

  // Now start from the last list node.
  Node := AList^.NodeTail;

  // Iterate from the last to the first list node.
  while Node <> nil do
  begin
    // If an even number is encountered, decrement the dummy counter.
    if PUInt32(@Node^.Data)^ and 1 = 0 then
      Num -= 1;

    // Go to the previous node.
    Node := Node^.Prev;
  end;

  // Calculate how many ticks the operation took.
  Result := TestGetTicks() - Result;
end;


{
  Measures the speed of cleaning and destroying the list.

  Arguments:
    • AList — a pointer to the list structure.

  Result:
    • The number of ticks on the hardware clock as the duration of the operation.
}
function TestListSparseDynDestroy(AList: PListSparseDyn): Int64;
begin
  // Remember the time when the test started.
  Result := TestGetTicks();

  // Clear and destroy the list.
  ListSparseDynDestroy(AList);

  // Calculate how many ticks the operation took.
  Result := TestGetTicks() - Result;
end;


end.

