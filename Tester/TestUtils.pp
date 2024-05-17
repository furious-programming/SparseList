
{
  Tester Utils.
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

unit TestUtils;

  // Global compiler switches.
  {$INCLUDE TestSwitches.inc}

interface

uses
  Vector,
  ListSimple,
  ListSparse,
  ListSparseDyn;


  // Getting the time.
  function  TestGetTicks    (): Int64; inline; // Returns the state of a high resolution clock.

  // Printing results.
  procedure TestPrintHeader (); // Prints the header line of the results table.
  procedure TestPrintFooter (); // Prints the footer line of the results table.
  procedure TestPrintResult (AName: String; ATimeVector, ATimeSimple, ATimeSparse, ATimeSparseDyn: Int64); // Prints a line of text with the measurement results of a specific test.

  {$IFDEF BUILD_DEBUG}

  // Testing data integrity.
  procedure TestContent     (AVector: PVector; ASimple: PListSimple; ASparse: PListSparse; ASparseDyn: PListSparseDyn); // Checks whether all containers contain the same amount of data and in the same order.

  {$ENDIF}


implementation

uses
  Windows,
  SysUtils,
  StrUtils;


const
  // The width of the result columns, in characters.
  RESULT_FIELD_SIZE_NAME = 10; // For the name of a test.
  RESULT_FIELD_SIZE_TIME = 16; // For the time and times results.


{
  Returns the state of a high resolution clock.

  This function is designed to retrieve the tick count of a high-resolution hardware counter just before and just after the
  execution of the instructions to which the measurement relates.

  Result:
    • High resolution clock status.
}
function TestGetTicks(): Int64;
begin
  Result := 0;
  QueryPerformanceCounter(Result);
end;


{
  Prints the header of the results table.
}
procedure TestPrintHeader();
begin
  WriteLn(
    '':RESULT_FIELD_SIZE_NAME,

    'VECTOR':RESULT_FIELD_SIZE_TIME,
    'SIMPLE':RESULT_FIELD_SIZE_TIME,
    'SPARSE':RESULT_FIELD_SIZE_TIME,
    'SPARSE DYN':RESULT_FIELD_SIZE_TIME,

    'VECTOR':RESULT_FIELD_SIZE_TIME,
    'SIMPLE':RESULT_FIELD_SIZE_TIME,
    'SPARSE':RESULT_FIELD_SIZE_TIME,
    'SPARSE DYN':RESULT_FIELD_SIZE_TIME
  );
  WriteLn();
end;


{
  Prints the footer line of the results table.
}
procedure TestPrintFooter();
begin
  WriteLn(StringOfChar('_', RESULT_FIELD_SIZE_NAME + RESULT_FIELD_SIZE_TIME * 8 + 10));
  WriteLn();
end;


{
  Prints a line of text with the measurement results of a specific test.

  Arguments:
    • AName          — a short name of the test.
    • ATimeVector    — test execution time on the vector.
    • ATimeSimple    — test execution time on the simple linked list.
    • ATimeSparse    — test execution time on the sprase linked list.
    • ATimeSparseDyn — test execution time on the sprase dynamic linked list.
}
procedure TestPrintResult(AName: String; ATimeVector, ATimeSimple, ATimeSparse, ATimeSparseDyn: Int64);
var
  PlaceVector:    Int64;
  PlaceSimple:    Int64;
  PlaceSparse:    Int64;
  PlaceSparseDyn: Int64;
  TimeMin:        Int64;
begin
  // If the time is "0", it is better to set it as "1" to avoid dividing by "0".
  ATimeVector    := Max(1, ATimeVector);
  ATimeSimple    := Max(1, ATimeSimple);
  ATimeSparse    := Max(1, ATimeSparse);
  ATimeSparseDyn := Max(1, ATimeSparseDyn);

  // Print the number of ticks of all measurements, with a thousand separator.
  Write(
    AName:RESULT_FIELD_SIZE_NAME,
    Format('%.0n', [ATimeVector    + 0.0]):RESULT_FIELD_SIZE_TIME,
    Format('%.0n', [ATimeSimple    + 0.0]):RESULT_FIELD_SIZE_TIME,
    Format('%.0n', [ATimeSparse    + 0.0]):RESULT_FIELD_SIZE_TIME,
    Format('%.0n', [ATimeSparseDyn + 0.0]):RESULT_FIELD_SIZE_TIME
  );

  // Determine the shortest time of the measurement.
  TimeMin := ATimeVector;

  if ATimeSimple    < TimeMin then TimeMin := ATimeSimple;
  if ATimeSparse    < TimeMin then TimeMin := ATimeSparse;
  if ATimeSparseDyn < TimeMin then TimeMin := ATimeSparseDyn;

  // Print data on how many times the execution of a given test was slower than the fastest one. The best result is displayed
  // as a dash to make it clearly visible in the table.
  Write(
    IfThen(ATimeVector    = TimeMin, '-', Format('%.2nx', [ATimeVector    / TimeMin])):RESULT_FIELD_SIZE_TIME,
    IfThen(ATimeSimple    = TimeMin, '-', Format('%.2nx', [ATimeSimple    / TimeMin])):RESULT_FIELD_SIZE_TIME,
    IfThen(ATimeSparse    = TimeMin, '-', Format('%.2nx', [ATimeSparse    / TimeMin])):RESULT_FIELD_SIZE_TIME,
    IfThen(ATimeSparseDyn = TimeMin, '-', Format('%.2nx', [ATimeSparseDyn / TimeMin])):RESULT_FIELD_SIZE_TIME
  );

  // Calculate what place each container took in the test.
  PlaceVector    := 1 + Ord(ATimeVector    > ATimeSimple) + Ord(ATimeVector    > ATimeSparse) + Ord(ATimeVector    > ATimeSparseDyn);
  PlaceSimple    := 1 + Ord(ATimeSimple    > ATimeVector) + Ord(ATimeSimple    > ATimeSparse) + Ord(ATimeSimple    > ATimeSparseDyn);
  PlaceSparse    := 1 + Ord(ATimeSparse    > ATimeVector) + Ord(ATimeSparse    > ATimeSimple) + Ord(ATimeSparse    > ATimeSparseDyn);
  PlaceSparseDyn := 1 + Ord(ATimeSparseDyn > ATimeVector) + Ord(ATimeSparseDyn > ATimeSimple) + Ord(ATimeSparseDyn > ATimeSparse);

  // Print the podium.
  WriteLn('    ',
    IfThen(PlaceVector    = 1, '-', PlaceVector.ToString()), ' ',
    IfThen(PlaceSimple    = 1, '-', PlaceSimple.ToString()), ' ',
    IfThen(PlaceSparse    = 1, '-', PlaceSparse.ToString()), ' ',
    IfThen(PlaceSparseDyn = 1, '-', PlaceSparseDyn.ToString())
  );
end;


{$IFDEF BUILD_DEBUG}

{
  Checks whether all containers contain the same amount of data and in the same order.

  This function is for debugging purposes only and is used to check whether each container has the same amount of data and,
  if so, whether the data is in the same order. If the contents of any container differ from the norm, the function generates
  a runtime error. It should be used after every test during which the contents of the containers change.

  Arguments:
    • AVector        — a pointer to the vector structure.
    • AListSimple    — a pointer to the simple linked list structure.
    • AListSparse    — a pointer to the sparse linked list structure.
    • AListSparseDyn — a pointer to the sparse dynamic linked list structure.
}
procedure TestContent(AVector: PVector; ASimple: PListSimple; ASparse: PListSparse; ASparseDyn: PListSparseDyn);
var
  DataNum:       Int32;
  DataVector:    PUInt32;
  DataSimple:    PUInt32;
  DataSparse:    PUInt32;
  DataSparseDyn: PUInt32;
  NodeSimple:    PListSimpleNode;
  NodeSparse:    PListSparseNode;
  NodeSparseDyn: PListSparseDynNode;
begin
  // Check if the number of items in the containers is the same.
  if AVector^.DataNum <> ASimple^.NodeNum    then System.Error(reAssertionFailed);
  if AVector^.DataNum <> ASparse^.NodeNum    then System.Error(reAssertionFailed);
  if AVector^.DataNum <> ASparseDyn^.NodeNum then System.Error(reAssertionFailed);

  // Get the number of items to check and set pointers to the first items in the containers.
  DataNum       := AVector^.DataNum;
  DataVector    := AVector^.Data;
  NodeSimple    := ASimple^.NodeHead;
  NodeSparse    := ASparse^.NodeHead;
  NodeSparseDyn := ASparseDyn^.NodeHead;

  // Iterate through the entire contents of the containers, checking each item in turn.
  while DataNum > 0 do
  begin
    // Check the links in the nodes in case the integrity of the lists has been compromised.
    if NodeSimple    = nil then System.Error(reAssertionFailed);
    if NodeSparse    = nil then System.Error(reAssertionFailed);
    if NodeSparseDyn = nil then System.Error(reAssertionFailed);

    // Typed pointers will come in handy.
    DataSimple    := @NodeSimple^.Data;
    DataSparse    := @NodeSparse^.Data;
    DataSparseDyn := @NodeSparseDyn^.Data;

    // Check whether the item data in each container is the same. If not, either the filling of the containers is faulty, or
    // something went wrong when inserting, deleting or sorting items.
    if DataVector^ <> DataSimple^    then System.Error(reAssertionFailed);
    if DataVector^ <> DataSparse^    then System.Error(reAssertionFailed);
    if DataVector^ <> DataSparseDyn^ then System.Error(reAssertionFailed);

    // Go to the next items.
    DataVector    += 1;
    NodeSimple    := NodeSimple^.Next;
    NodeSparse    := NodeSparse^.Next;
    NodeSparseDyn := NodeSparseDyn^.Next;

    DataNum -= 1;
  end;
end;

{$ENDIF}


end.

