
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


  // Measuring specific vector functions.
  function TestVectorAppend   (AVector: PVector; ADataNum: Int32): Int64; // Measures the speed of adding a given number of items to a vector.
  function TestVectorInsert   (AVector: PVector): Int64; // Measures the speed of inserting a given number of items into a vector.
  function TestVectorChop     (AVector: PVector): Int64; // Measures the speed of removing items from a vector.
  function TestVectorSort     (AVector: PVector): Int64; // Measures the speed of sorting items from a vector.
  function TestVectorClear    (AVector: PVector): Int64; // Measures the speed of vector cleaning.
  function TestVectorTraverse (AVector: PVector): Int64; // Measures the speed of iterating over the items of a vector.
  function TestVectorDestroy  (AVector: PVector): Int64; // Measures the speed of cleaning and destroying the vector.


implementation

uses
  TestUtils;


{
  Measures the speed of adding a given number of items to a vector.

  This function measures the duration of the operation of adding a given number of items to the vector. Data for the vector
  items are generated based on the 32-bit Xorshift algorithm, so the generated numbers are relatively random and unordered
  for the later vector sorting test.

  [!] For the sorting test to make sense, item data for all tested containers must be generated in the same way. This ensures
      that all sorting functions perform the same number of iterations.

  Arguments:
    • AVector  — a pointer to the vector structure.
    • ADataNum — the number of items to add to the vector.

  Result:
    • The number of ticks on the hardware clock as the duration of the operation.
}
function TestVectorAppend(AVector: PVector; ADataNum: Int32): Int64;
var
  DataSeed: UInt32 = $600D5EED;
begin
  // Remember the time when the test started.
  Result := TestGetTicks();

  while ADataNum > 0 do
  begin
    // Add data to the end of the vector.
    VectorDataAppend(AVector, @DataSeed);

    // Generate another pseudo-random number according to the Xorshift algorithm.
    DataSeed := DataSeed xor (DataSeed shl 13);
    DataSeed := DataSeed xor (DataSeed shr 17);
    DataSeed := DataSeed xor (DataSeed shl  5);

    ADataNum -= 1;
  end;

  // Calculate how many ticks the operation took.
  Result := TestGetTicks() - Result;
end;


{
  Measures the speed of inserting a given number of items into a vector.

  This function iterates over all items in the vector and inserts new items in place of every second existing item. The result
  of this function is to increase the number of items by one third and to insert items with pseudo-random data into it for the
  purpose of a further sorting test.

  [!] For the sorting test to make sense, item data for all tested containers must be generated in the same way. This ensures
      that all sorting functions perform the same number of iterations.

  Arguments:
    • AVector — a pointer to the vector structure.

  Result:
    • The number of ticks on the hardware clock as the duration of the operation.
}
function TestVectorInsert(AVector: PVector): Int64;
var
  DataIndex:  Int32  = 0;
  DataInsert: Int32  = 0;
  DataSeed:   UInt32 = $BAD5EED;
begin
  // Remember the time when the test started.
  Result := TestGetTicks();

  // Iterate until the end of the vector.
  while DataIndex < AVector^.DataNum do
  begin
    // If it's time, insert a new item into the vector.
    if DataInsert = 0 then
    begin
      VectorDataInsert(AVector, @DataSeed, DataIndex);

      // This needs to be so that insertion works the same as it does with lists.
      DataIndex += 1;
    end;

    // Generate another pseudo-random number according to the Xorshift algorithm.
    DataSeed := DataSeed xor (DataSeed shl 13);
    DataSeed := DataSeed xor (DataSeed shr 17);
    DataSeed := DataSeed xor (DataSeed shl  5);

    // Go to the next item and update the counter that determines when to insert an item.
    DataIndex  += 1;
    DataInsert := (DataInsert + 1) mod 2;
  end;

  // Calculate how many ticks the operation took.
  Result := TestGetTicks() - Result;
end;


{
  Measures the speed of removing items from a vector.

  This function iterates from the beginning to the end of the vector and removes every second item from it.

  Arguments:
    • AVector — a pointer to the vector structure.

  Result:
    • The number of ticks on the hardware clock as the duration of the operation.
}
function TestVectorChop(AVector: PVector): Int64;
var
  DataIndex: Int32 = 0;
  DataChop:  Int32 = 0;
begin
  // Remember the time when the test started.
  Result := TestGetTicks();

  // Iterate until the end of the vector.
  while DataIndex < AVector^.DataNum do
  begin
    // If it's time, delete an item from the vector.
    if DataChop = 0 then
    begin
      VectorDataDelete(AVector, DataIndex);

      // This needs to be so that chopping works the same as it does with lists.
      DataIndex -= 1;
    end;

    // Go to the next item and update the counter that determines when to delete an item.
    DataIndex += 1;
    DataChop  := (DataChop + 1) mod 2;
  end;

  // Calculate how many ticks the operation took.
  Result := TestGetTicks() - Result;
end;


  {
    Compares data of two items.

    Arguments:
      • ADataA — pointer to the first item's data.
      • ADataB — pointer to the second item's data.

    Result:
      • False — item data will not be swapped.
      • True  — item data will be swapped (will be moved towards the end of the vector).
  }
  function TestVectorSortData(ADataA, ADataB: PUInt32): Boolean;
  begin
    Result := ADataA^ > ADataB^;
  end;

{
  Measures the speed of sorting items from a vector.

  Arguments:
    • AVector — a pointer to the vector structure.

  Result:
    • The number of ticks on the hardware clock as the duration of the operation.
}
function TestVectorSort(AVector: PVector): Int64;
begin
  // Remember the time when the test started.
  Result := TestGetTicks();

  // Sort the vector content using the bubble sort algorithm.
  VectorSortBubble(AVector, @TestVectorSortData);

  // Calculate how many ticks the operation took.
  Result := TestGetTicks() - Result;
end;


{
  Measures the speed of vector cleaning.

  Arguments:
    • AVector — a pointer to the vector structure.

  Result:
    • The number of ticks on the hardware clock as the duration of the operation.
}
function TestVectorClear(AVector: PVector): Int64;
begin
  // Remember the time when the test started.
  Result := TestGetTicks();

  // Clear the vector.
  VectorClear(AVector);

  // Calculate how many ticks the operation took.
  Result := TestGetTicks() - Result;
end;


{
  Measures the speed of iterating over the items of a vector.

  This function traverses the vector in both directions, testing the CPU's caching ability.

  Arguments:
    • AVector — a pointer to the vector structure.

  Result:
    • The number of ticks on the hardware clock as the duration of the operation.
}
function TestVectorTraverse(AVector: PVector): Int64;
var
  DataCurr: PUInt32;
  DataStop: PUInt32;
  Num:      Int32 = 0;
begin
  // Remember the time when the test started.
  Result := TestGetTicks();

  // Calculate the address of the first and last item of the vector.
  DataCurr := AVector^.Data;
  DataStop := DataCurr + AVector^.DataNum - 1;

  // Iterate from the first to the last items vector.
  while DataCurr <= DataStop do
  begin
    // If an odd number is encountered, increment the dummy counter.
    if DataCurr^ and 1 = 1 then
      Num += 1;

    // Go to the next item.
    DataCurr += 1;
  end;

  // Again, calculate the address of the first and last item of the vector.
  DataCurr := AVector^.Data + AVector^.DataNum - 1;
  DataStop := AVector^.Data;

  // This time iterate from the last to the first item of the vector.
  while DataCurr >= DataStop do
  begin
    // If an even number is encountered, decrement the dummy counter.
    if DataCurr^ and 1 = 0 then
      Num -= 1;

    // Go to the previous item.
    DataCurr -= 1;
  end;

  // Calculate how many ticks the operation took.
  Result := TestGetTicks() - Result;
end;


{
  Measures the speed of cleaning and destroying the vector.

  Arguments:
    • AVector — a pointer to the vector structure.

  Result:
    • The number of ticks on the hardware clock as the duration of the operation.
}
function TestVectorDestroy(AVector: PVector): Int64;
begin
  // Remember the time when the test started.
  Result := TestGetTicks();

  // Clear and destroy the vector.
  VectorDestroy(AVector);

  // Calculate how many ticks the operation took.
  Result := TestGetTicks() - Result;
end;


end.

