program main;

uses
  SparseList,
  SparseListDyn;

  procedure SparseListPrint(AList: PSparseList);
  var
    Node: PSparseListNode;
  begin
    Node := AList^.Head;

    while Node <> nil do
    begin
      Write(PUInt64(@Node^.Data)^:2);
      Node := Node^.Next;
    end;

    WriteLn();
    Node := AList^.Tail;

    while Node <> nil do
    begin
      Write(PUInt64(@Node^.Data)^:2);
      Node := Node^.Prev;
    end;

    WriteLn();
    WriteLn();
  end;

var
  List: PSparseList;
  Node: PSparseListNode;
begin
  List := SparseListCreate(SizeOf(UInt64), 32);
  SparseListPrint(List);

  Node := SparseListNodeCreate(List);
  PUInt64(@Node^.Data)^ := 0;
  SparseListNodeAppend(List, Node);
  SparseListPrint(List);

  Node := SparseListNodeCreate(List);
  PUInt64(@Node^.Data)^ := 1;
  SparseListNodeInsert(List, Node, List^.Head);
  SparseListPrint(List);

  Node := SparseListNodeCreate(List);
  PUInt64(@Node^.Data)^ := 2;
  SparseListNodeInsert(List, Node, List^.Head);
  SparseListPrint(List);

  Node := SparseListNodeCreate(List);
  PUInt64(@Node^.Data)^ := 3;
  SparseListNodeInsert(List, Node, List^.Head^.Next);
  SparseListPrint(List);

  Node := SparseListNodeCreate(List);
  PUInt64(@Node^.Data)^ := 4;
  SparseListNodeInsert(List, Node, List^.Tail);
  SparseListPrint(List);

  Node := SparseListNodeCreate(List);
  PUInt64(@Node^.Data)^ := 5;
  SparseListNodeAppend(List, Node);
  SparseListPrint(List);

  SparseListDestroy(List);

  WriteLn();
    HeapTrc.DumpHeap();
    HeapTrc.UseHeapTrace := False;
  ReadLn();
end.

