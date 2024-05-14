
{
  Doubly-linked Lists Tester.
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

program tester;
uses
  Math,
  SimpleList,
  SparseList,
  SparseListDyn,
  TestSimpleList,
  TestSparseList,
  TestSparseListDyn,
  TestUtils;
label
  TestAgain;
var
  ListSimple:    PSimpleList;
  ListSparse:    PSparseList;
  ListSparseDyn: PSparseListDyn;
var
  ListNodeNum:        Integer;
  ListNodeNumSegment: Integer;
begin
TestAgain:
  Write('Enter the total number of nodes (0 to exit): ');
  ReadLn(ListNodeNum);

  if ListNodeNum = 0 then Exit;

  Write('Enter the number of nodes per segment:       ');
  ReadLn(ListNodeNumSegment);

  ListNodeNum        := EnsureRange(ListNodeNum, 2, 1024 * 1024);
  ListNodeNumSegment := EnsureRange(ListNodeNumSegment, 1, 8192);

  WriteLn();
  WriteLn('Staring test for ', ListNodeNum, ' nodes and ', ListNodeNumSegment, ' nodes per segment...');
  WriteLn();

  TestPrintHeader();

  ListSimple    := SimpleListCreate    (SizeOf(UInt32));
  ListSparse    := SparseListCreate    (SizeOf(UInt32), ListNodeNumSegment);
  ListSparseDyn := SparseListDynCreate (SizeOf(UInt32), ListNodeNumSegment);

  TestPrintResult('APPEND',
    TestSimpleListAppend    (ListSimple,    ListNodeNum),
    TestSparseListAppend    (ListSparse,    ListNodeNum),
    TestSparseListDynAppend (ListSparseDyn, ListNodeNum)
  );

  TestPrintResult('TRAVERSE',
    TestSimpleListTraverse    (ListSimple),
    TestSparseListTraverse    (ListSparse),
    TestSparseListDynTraverse (ListSparseDyn)
  );

  TestPrintResult('SORT',
    TestSimpleListSort    (ListSimple),
    TestSparseListSort    (ListSparse),
    TestSparseListDynSort (ListSparseDyn)
  );

  TestPrintResult('TRAVERSE',
    TestSimpleListTraverse    (ListSimple),
    TestSparseListTraverse    (ListSparse),
    TestSparseListDynTraverse (ListSparseDyn)
  );

  TestPrintResult('CLEAR',
    TestSimpleListClear    (ListSimple),
    TestSparseListClear    (ListSparse),
    TestSparseListDynClear (ListSparseDyn)
  );

  TestPrintResult('APPEND',
    TestSimpleListAppend    (ListSimple,    ListNodeNum),
    TestSparseListAppend    (ListSparse,    ListNodeNum),
    TestSparseListDynAppend (ListSparseDyn, ListNodeNum)
  );

  TestPrintResult('TRAVERSE',
    TestSimpleListTraverse    (ListSimple),
    TestSparseListTraverse    (ListSparse),
    TestSparseListDynTraverse (ListSparseDyn)
  );

  TestPrintResult('CHOP',
    TestSimpleListChop    (ListSimple),
    TestSparseListChop    (ListSparse),
    TestSparseListDynChop (ListSparseDyn)
  );

  TestPrintResult('TRAVERSE',
    TestSimpleListTraverse    (ListSimple),
    TestSparseListTraverse    (ListSparse),
    TestSparseListDynTraverse (ListSparseDyn)
  );

  TestPrintResult('INSERT',
    TestSimpleListInsert    (ListSimple),
    TestSparseListInsert    (ListSparse),
    TestSparseListDynInsert (ListSparseDyn)
  );

  TestPrintResult('TRAVERSE',
    TestSimpleListTraverse    (ListSimple),
    TestSparseListTraverse    (ListSparse),
    TestSparseListDynTraverse (ListSparseDyn)
  );

  TestPrintResult('SORT',
    TestSimpleListSort    (ListSimple),
    TestSparseListSort    (ListSparse),
    TestSparseListDynSort (ListSparseDyn)
  );

  TestPrintResult('TRAVERSE',
    TestSimpleListTraverse    (ListSimple),
    TestSparseListTraverse    (ListSparse),
    TestSparseListDynTraverse (ListSparseDyn)
  );

  TestPrintResult('CHOP',
    TestSimpleListChop    (ListSimple),
    TestSparseListChop    (ListSparse),
    TestSparseListDynChop (ListSparseDyn)
  );

  TestPrintResult('TRAVERSE',
    TestSimpleListTraverse    (ListSimple),
    TestSparseListTraverse    (ListSparse),
    TestSparseListDynTraverse (ListSparseDyn)
  );

  TestPrintResult('DESTROY',
    TestSimpleListDestroy    (ListSimple),
    TestSparseListDestroy    (ListSparse),
    TestSparseListDynDestroy (ListSparseDyn)
  );

  TestPrintFooter();
  goto TestAgain;
end.

