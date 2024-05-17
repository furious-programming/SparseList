
{
  Simple Doubly-linked List Tester.
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

unit TestListSimple;

  // Global compiler switches.
  {$INCLUDE TestSwitches.inc}

interface

uses
  ListSimple;


  function TestListSimpleAppend   (AList: PListSimple; ANodeNum: Int32): Int64;
  function TestListSimpleInsert   (AList: PListSimple): Int64;
  function TestListSimpleChop     (AList: PListSimple): Int64;
  function TestListSimpleSort     (AList: PListSimple): Int64;
  function TestListSimpleClear    (AList: PListSimple): Int64;
  function TestListSimpleTraverse (AList: PListSimple): Int64;
  function TestListSimpleDestroy  (AList: PListSimple): Int64;


implementation

uses
  TestUtils;


function TestListSimpleAppend(AList: PListSimple; ANodeNum: Int32): Int64;
var
  NodeSeed: UInt32 = $600D5EED;
begin
  Result := TestGetTicks();

  while ANodeNum > 0 do
  begin
    ListSimpleNodeAppend(AList, ListSimpleNodeCreate(AList));
    PUInt32(@AList^.NodeTail^.Data)^ := NodeSeed;

    NodeSeed := NodeSeed xor (NodeSeed shl 13);
    NodeSeed := NodeSeed xor (NodeSeed shr 17);
    NodeSeed := NodeSeed xor (NodeSeed shl  5);

    ANodeNum -= 1;
  end;

  Result := TestGetTicks() - Result;
end;


function TestListSimpleInsert(AList: PListSimple): Int64;
var
  NodeNew:    PListSimpleNode;
  NodeCurr:   PListSimpleNode;
  NodeInsert: Int32  = 0;
  NodeSeed:   UInt32 = $BAD5EED;
begin
  Result   := TestGetTicks();
  NodeCurr := AList^.NodeHead;

  while NodeCurr <> nil do
  begin
    if NodeInsert = 0 then
    begin
      NodeNew := ListSimpleNodeCreate(AList);
      PUInt32(@NodeNew^.Data)^ := NodeSeed;

      ListSimpleNodeInsert(AList, NodeNew, NodeCurr);
    end;

    NodeSeed := NodeSeed xor (NodeSeed shl 13);
    NodeSeed := NodeSeed xor (NodeSeed shr 17);
    NodeSeed := NodeSeed xor (NodeSeed shl  5);

    NodeCurr   := NodeCurr^.Next;
    NodeInsert := (NodeInsert + 1) mod 2;
  end;

  Result := TestGetTicks() - Result;
end;


function TestListSimpleChop(AList: PListSimple): Int64;
var
  Node:     PListSimpleNode;
  NodeNext: PListSimpleNode;
  NodeChop: Int32 = 0;
begin
  Result := TestGetTicks();
  Node   := AList^.NodeHead;

  while Node <> nil do
  begin
    NodeNext := Node^.Next;

    if NodeChop = 0 then
    begin
      ListSimpleNodeExtract(AList, Node);
      ListSimpleNodeDestroy(AList, Node);
    end;

    Node     := NodeNext;
    NodeChop := (NodeChop + 1) mod 2;
  end;

  Result := TestGetTicks() - Result;
end;


  function TestSimpleListSortNodes(ANodeA, ANodeB: PListSimpleNode): Boolean;
  begin
    Result := PUInt32(@ANodeA^.Data)^ > PUInt32(@ANodeB^.Data)^;
  end;

function TestListSimpleSort(AList: PListSimple): Int64;
begin
  Result := TestGetTicks();
  ListSimpleSortBubble(AList, @TestSimpleListSortNodes);
  Result := TestGetTicks() - Result;
end;


function TestListSimpleClear(AList: PListSimple): Int64;
begin
  Result := TestGetTicks();
  ListSimpleClear(AList);
  Result := TestGetTicks() - Result;
end;


function TestListSimpleTraverse(AList: PListSimple): Int64;
var
  Node: PListSimpleNode;
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


function TestListSimpleDestroy(AList: PListSimple): Int64;
begin
  Result := TestGetTicks();
  ListSimpleDestroy(AList);
  Result := TestGetTicks() - Result;
end;


end.

