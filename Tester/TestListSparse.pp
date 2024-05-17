
{
  Sparse Doubly-linked List Tester.
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

unit TestListSparse;

  // Global compiler switches.
  {$INCLUDE TestSwitches.inc}

interface

uses
  ListSparse;


  function TestListSparseAppend   (AList: PListSparse; ANodeNum: Int32): Int64;
  function TestListSparseInsert   (AList: PListSparse): Int64;
  function TestListSparseChop     (AList: PListSparse): Int64;
  function TestListSparseSort     (AList: PListSparse): Int64;
  function TestListSparseClear    (AList: PListSparse): Int64;
  function TestListSparseTraverse (AList: PListSparse): Int64;
  function TestListSparseDestroy  (AList: PListSparse): Int64;


implementation

uses
  TestUtils;


function TestListSparseAppend(AList: PListSparse; ANodeNum: Int32): Int64;
var
  NodeSeed: UInt32 = $600D5EED;
begin
  Result := TestGetTicks();

  while ANodeNum > 0 do
  begin
    ListSparseNodeAppend(AList, ListSparseNodeCreate(AList));
    PUInt32(@AList^.NodeTail^.Data)^ := NodeSeed;

    NodeSeed := NodeSeed xor (NodeSeed shl 13);
    NodeSeed := NodeSeed xor (NodeSeed shr 17);
    NodeSeed := NodeSeed xor (NodeSeed shl  5);

    ANodeNum -= 1;
  end;

  Result := TestGetTicks() - Result;
end;


function TestListSparseInsert(AList: PListSparse): Int64;
var
  NodeNew:    PListSparseNode;
  NodeCurr:   PListSparseNode;
  NodeInsert: Int32  = 0;
  NodeSeed:   UInt32 = $BAD5EED;
begin
  Result   := TestGetTicks();
  NodeCurr := AList^.NodeHead;

  while NodeCurr <> nil do
  begin
    if NodeInsert = 0 then
    begin
      NodeNew := ListSparseNodeCreate(AList);
      PUInt32(@NodeNew^.Data)^ := NodeSeed;

      ListSparseNodeInsert(AList, NodeNew, NodeCurr);
    end;

    NodeSeed := NodeSeed xor (NodeSeed shl 13);
    NodeSeed := NodeSeed xor (NodeSeed shr 17);
    NodeSeed := NodeSeed xor (NodeSeed shl  5);

    NodeCurr   := NodeCurr^.Next;
    NodeInsert := (NodeInsert + 1) mod 2;
  end;

  Result := TestGetTicks - Result;
end;


function TestListSparseChop(AList: PListSparse): Int64;
var
  Node:     PListSparseNode;
  NodeNext: PListSparseNode;
  NodeChop: Int32 = 0;
begin
  Result := TestGetTicks();
  Node   := AList^.NodeHead;

  while Node <> nil do
  begin
    NodeNext := Node^.Next;

    if NodeChop = 0 then
    begin
      ListSparseNodeExtract(AList, Node);
      ListSparseNodeDestroy(AList, Node);
    end;

    Node     := NodeNext;
    NodeChop := (NodeChop + 1) mod 2;
  end;

  Result := TestGetTicks - Result;
end;


  function TestSparseListSortNodes(ANodeA, ANodeB: PListSparseNode): Boolean;
  begin
    Result := PUInt32(@ANodeA^.Data)^ > PUInt32(@ANodeB^.Data)^;
  end;

function TestListSparseSort(AList: PListSparse): Int64;
begin
  Result := TestGetTicks();
  ListSparseSortBubble(AList, @TestSparseListSortNodes);
  Result := TestGetTicks() - Result;
end;


function TestListSparseClear(AList: PListSparse): Int64;
begin
  Result := TestGetTicks();
  ListSparseClear(AList);
  Result := TestGetTicks() - Result;
end;


function TestListSparseTraverse(AList: PListSparse): Int64;
var
  Node: PListSparseNode;
  Num:  Int32 = 0;
begin
  Result := TestGetTicks();
  Node   := AList^.NodeHead;

  while Node <> nil do
  begin
    if PUInt32(@Node^.Data)^ and 1 = 1 then
      Num += 1;

    Node := Node^.Next;
  end;

  Node := AList^.NodeTail;

  while Node <> nil do
  begin
    if PUInt32(@Node^.Data)^ and 1 = 0 then
      Num -= 1;

    Node := Node^.Prev;
  end;

  Result := TestGetTicks() - Result;
end;


function TestListSparseDestroy(AList: PListSparse): Int64;
begin
  Result := TestGetTicks();
  ListSparseDestroy(AList);
  Result := TestGetTicks() - Result;
end;


end.

