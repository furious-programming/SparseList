
{
  Sparse Vector Tester.
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

unit TestVector;

  // Global compiler switches.
  {$INCLUDE TestSwitches.inc}

interface

uses
  Vector;


  function TestVectorAppend   (AVector: PVector; ADataNum: Int32): Int64;
  function TestVectorInsert   (AVector: PVector): Int64;
  function TestVectorChop     (AVector: PVector): Int64;
  function TestVectorSort     (AVector: PVector): Int64;
  function TestVectorClear    (AVector: PVector): Int64;
  function TestVectorTraverse (AVector: PVector): Int64;
  function TestVectorDestroy  (AVector: PVector): Int64;


implementation

uses
  TestUtils;


function TestVectorAppend(AVector: PVector; ADataNum: Int32): Int64;
var
  DataSeed: UInt32 = $600D5EED;
begin
  Result := TestGetTicks();

  while ADataNum > 0 do
  begin
    VectorDataAppend(AVector, @DataSeed);

    DataSeed := DataSeed xor (DataSeed shl 13);
    DataSeed := DataSeed xor (DataSeed shr 17);
    DataSeed := DataSeed xor (DataSeed shl  5);

    ADataNum -= 1;
  end;

  Result := TestGetTicks() - Result;
end;


function TestVectorInsert(AVector: PVector): Int64;
var
  DataIndex:  Int32  = 0;
  DataInsert: Int32  = 0;
  DataSeed:   UInt32 = $BAD5EED;
begin
  Result := TestGetTicks();

  while DataIndex < AVector^.DataNum do
  begin
    if DataInsert = 0 then
      VectorDataInsert(AVector, @DataSeed, DataIndex);

    DataSeed := DataSeed xor (DataSeed shl 13);
    DataSeed := DataSeed xor (DataSeed shr 17);
    DataSeed := DataSeed xor (DataSeed shl  5);

    DataIndex  += 1;
    DataInsert := (DataInsert + 1) mod 2;
  end;

  Result := TestGetTicks() - Result;
end;


function TestVectorChop(AVector: PVector): Int64;
var
  DataIndex: Int32 = 0;
  DataChop:  Int32 = 0;
begin
  Result := TestGetTicks();

  while DataIndex < AVector^.DataNum do
  begin
    if DataChop = 0 then
      VectorDataDelete(AVector, DataIndex);

    DataIndex += 1;
    DataChop  := (DataChop + 1) mod 2;
  end;

  Result := TestGetTicks() - Result;
end;


  function TestVectorSortData(ADataA, ADataB: PUInt32): Boolean;
  begin
    Result := ADataA^ > ADataB^;
  end;

function TestVectorSort(AVector: PVector): Int64;
begin
  Result := TestGetTicks();
  VectorSortBubble(AVector, @TestVectorSortData);
  Result := TestGetTicks() - Result;
end;


function TestVectorClear(AVector: PVector): Int64;
begin
  Result := TestGetTicks();
  VectorClear(AVector);
  Result := TestGetTicks() - Result;
end;


function TestVectorTraverse(AVector: PVector): Int64;
var
  DataCurr: PUInt32;
  DataStop: PUInt32;
  Num:      Int32 = 0;
begin
  Result := TestGetTicks();

  DataCurr := AVector^.Data;
  DataStop := DataCurr + AVector^.DataNum - 1;

  while DataCurr <= DataStop do
  begin
    if DataCurr^ and 1 = 1 then
      Num += 1;

    DataCurr += 1;
  end;

  DataCurr := AVector^.Data + AVector^.DataNum - 1;
  DataStop := AVector^.Data;

  while DataCurr >= DataStop do
  begin
    if DataCurr^ and 1 = 0 then
      Num -= 1;

    DataCurr -= 1;
  end;

  Result := TestGetTicks() - Result;
end;


function TestVectorDestroy(AVector: PVector): Int64;
begin
  Result := TestGetTicks();
  VectorDestroy(AVector);
  Result := TestGetTicks() - Result;
end;


end.

