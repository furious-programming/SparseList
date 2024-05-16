
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

// Global compiler switches.
{$INCLUDE TestSwitches.inc}

uses
  Math,
  Vector,
  ListSimple,
  ListSparse,
  ListSparseDyn,
  TestVector,
  TestListSimple,
  TestListSparse,
  TestListSparseDyn,
  TestUtils;
label
  TestAgain;
var
  HandleVector:    PVector;
  HandleSimple:    PListSimple;
  HandleSparse:    PListSparse;
  HandleSparseDyn: PListSparseDyn;
var
  ListNodeNum:        Int32;
  ListNodeNumSegment: Int32;
begin
TestAgain:
  Write('Enter the total number of nodes (0 to exit): ');
  ReadLn(ListNodeNum);

  if ListNodeNum = 0 then Exit;

  Write('Enter the number of nodes per segment:       ');
  ReadLn(ListNodeNumSegment);

  ListNodeNum        := EnsureRange(ListNodeNum,        1, 512 * 1024);
  ListNodeNumSegment := EnsureRange(ListNodeNumSegment, 1,       8192);

  WriteLn();
  WriteLn('Staring test for ', ListNodeNum, ' nodes and ', ListNodeNumSegment, ' nodes per segment...');
  WriteLn();

  TestPrintHeader();

  HandleVector    := VectorCreate        (SizeOf(UInt32));
  HandleSimple    := ListSimpleCreate    (SizeOf(UInt32));
  HandleSparse    := ListSparseCreate    (SizeOf(UInt32), ListNodeNumSegment);
  HandleSparseDyn := ListSparseDynCreate (SizeOf(UInt32), ListNodeNumSegment);

  TestPrintResult('APPEND',
    TestVectorAppend        (HandleVector,    ListNodeNum),
    TestListSimpleAppend    (HandleSimple,    ListNodeNum),
    TestListSparseAppend    (HandleSparse,    ListNodeNum),
    TestListSparseDynAppend (HandleSparseDyn, ListNodeNum)
  );

  TestPrintResult('<->',
    TestVectorTraverse        (HandleVector),
    TestListSimpleTraverse    (HandleSimple),
    TestListSparseTraverse    (HandleSparse),
    TestListSparseDynTraverse (HandleSparseDyn)
  );
  WriteLn();

  TestPrintResult('SORT',
    TestVectorSort        (HandleVector),
    TestListSimpleSort    (HandleSimple),
    TestListSparseSort    (HandleSparse),
    TestListSparseDynSort (HandleSparseDyn)
  );

  TestPrintResult('<->',
    TestVectorTraverse        (HandleVector),
    TestListSimpleTraverse    (HandleSimple),
    TestListSparseTraverse    (HandleSparse),
    TestListSparseDynTraverse (HandleSparseDyn)
  );               
  WriteLn();

  TestPrintResult('CLEAR',
    TestVectorClear        (HandleVector),
    TestListSimpleClear    (HandleSimple),
    TestListSparseClear    (HandleSparse),
    TestListSparseDynClear (HandleSparseDyn)
  );

  TestPrintResult('APPEND',
    TestVectorAppend        (HandleVector,    ListNodeNum),
    TestListSimpleAppend    (HandleSimple,    ListNodeNum),
    TestListSparseAppend    (HandleSparse,    ListNodeNum),
    TestListSparseDynAppend (HandleSparseDyn, ListNodeNum)
  );

  TestPrintResult('<->',
    TestVectorTraverse        (HandleVector),
    TestListSimpleTraverse    (HandleSimple),
    TestListSparseTraverse    (HandleSparse),
    TestListSparseDynTraverse (HandleSparseDyn)
  );                 
  WriteLn();

  TestPrintResult('CHOP',
    TestVectorChop        (HandleVector),
    TestListSimpleChop    (HandleSimple),
    TestListSparseChop    (HandleSparse),
    TestListSparseDynChop (HandleSparseDyn)
  );

  TestPrintResult('<->',
    TestVectorTraverse        (HandleVector),
    TestListSimpleTraverse    (HandleSimple),
    TestListSparseTraverse    (HandleSparse),
    TestListSparseDynTraverse (HandleSparseDyn)
  );                       
  WriteLn();

  TestPrintResult('INSERT',
    TestVectorInsert        (HandleVector),
    TestListSimpleInsert    (HandleSimple),
    TestListSparseInsert    (HandleSparse),
    TestListSparseDynInsert (HandleSparseDyn)
  );

  TestPrintResult('<->',
    TestVectorTraverse        (HandleVector),
    TestListSimpleTraverse    (HandleSimple),
    TestListSparseTraverse    (HandleSparse),
    TestListSparseDynTraverse (HandleSparseDyn)
  );                    
  WriteLn();

  TestPrintResult('SORT',
    TestVectorSort        (HandleVector),
    TestListSimpleSort    (HandleSimple),
    TestListSparseSort    (HandleSparse),
    TestListSparseDynSort (HandleSparseDyn)
  );

  TestPrintResult('<->',
    TestVectorTraverse        (HandleVector),
    TestListSimpleTraverse    (HandleSimple),
    TestListSparseTraverse    (HandleSparse),
    TestListSparseDynTraverse (HandleSparseDyn)
  );                        
  WriteLn();

  TestPrintResult('CHOP',
    TestVectorChop        (HandleVector),
    TestListSimpleChop    (HandleSimple),
    TestListSparseChop    (HandleSparse),
    TestListSparseDynChop (HandleSparseDyn)
  );

  TestPrintResult('<->',
    TestVectorTraverse        (HandleVector),
    TestListSimpleTraverse    (HandleSimple),
    TestListSparseTraverse    (HandleSparse),
    TestListSparseDynTraverse (HandleSparseDyn)
  );                          
  WriteLn();

  TestPrintResult('DESTROY',
    TestVectorDestroy        (HandleVector),
    TestListSimpleDestroy    (HandleSimple),
    TestListSparseDestroy    (HandleSparse),
    TestListSparseDynDestroy (HandleSparseDyn)
  );

  TestPrintFooter();
  goto TestAgain;
end.

