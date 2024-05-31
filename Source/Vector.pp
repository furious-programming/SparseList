
{
  Sparse Vector.
  Copyleft (ɔ) 2024 furious programming. All rights reversed.
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

unit Vector;

  // Global compiler switches.
  {$INCLUDE TestSwitches.inc}

interface


type
  // Typed pointer data types.
  PVector = ^TVector;

  // A structure of a vector.
  TVector = record
    Data:     Pointer; // A pointer to the memory buffer.
    DataNum:  Int32;   // The number of all vector items.
    SizeData: Int32;   // The data size of each item, in bytes.
    Capacity: Int32;   // The current capacity of the vector buffer, in items.
  end;

type
  // Callback comparing data of two items, for the purpose of sorting the vector.
  TVectorCallbackCompare = function (ADataA, ADataB: Pointer): Boolean;


  // Allocating and deallocating a vector.
  function  VectorCreate     (ASizeData: Int32): PVector; // Allocates a new vector on the heap and initializes it.
  procedure VectorDestroy    (AVector: PVector); // Finalizes and deallocates the vector from the heap.

  // Initializing and finalizing a vector.
  procedure VectorInitialize (AVector: PVector; ASizeData: Int32); // Initializes an existing vector.
  procedure VectorFinalize   (AVector: PVector); // Finalizes an existing vector.

  // Clearing the vector.
  procedure VectorClear      (AVector: PVector); // Removes all items from the vector.

  // Sorting the vector.
  procedure VectorSortBubble (AVector: PVector; ACallback: TVectorCallbackCompare); // Performs bubble sorting on the vector.

  // Managing the vector items.
  procedure VectorDataAppend (AVector: PVector; AData: Pointer); // Adds an item to the end of the vector.
  procedure VectorDataInsert (AVector: PVector; AData: Pointer; AIndex: Int32); // Inserts an item into a given place in the vector.
  procedure VectorDataDelete (AVector: PVector; AIndex: Int32); // Removes an item with a given index from the vector.


implementation


{
  Allocates a new vector on the heap and initializes it.

  [i] If you have a vector allocated on the stack, initialize it directly with the "VectorInitialize" function.

  Arguments:
    • ASizeData — the size of the item data in bytes, in range [1,n].

  Result:
    • A non-nil pointer to an allocated and initialized vector.
}
function VectorCreate(ASizeData: Int32): PVector;
begin
  Result := GetMem(SizeOf(TVector));
  VectorInitialize(Result, ASizeData);
end;


{
  Finalizes and deallocates the vector from the heap.

  [i] If you have a vector allocated on the stack, finalize it directly with the "VectorFinalize" function.

  Arguments:
    • AVector — a pointer to the structure of the vector to finalize and deallocate.
}
procedure VectorDestroy(AVector: PVector);
begin
  VectorFinalize(AVector);
  FreeMem(AVector);
end;


{
  Initializes an existing vector.

  Because the vector buffer size grows exponentially, its initial capacity cannot be zero. Therefore, during vector
  initialization, the buffer is immediately allocated to several elements.

  [i] This function should be used to initialize a vector allocated on the stack.

  Arguments:
    • AVector   — a pointer to the structure of the vector to initialize.
    • ASizeData — the size of the item data in bytes, in range [1,n].
}
procedure VectorInitialize(AVector: PVector; ASizeData: Int32);
const
  // The initial capacity must be a power of two and not less than "2".
  VECTOR_CAPACITY_DEFAULT = 16;
begin
  AVector^.Data     := GetMem(VECTOR_CAPACITY_DEFAULT * ASizeData);
  AVector^.DataNum  := 0;
  AVector^.SizeData := ASizeData;
  AVector^.Capacity := VECTOR_CAPACITY_DEFAULT;
end;


{
  Finalizes an existing vector.

  [i] This function should be used to finalize a vector allocated on the stack.

  Arguments:
    • AVector — a pointer to the vector structure to finalize.
}
procedure VectorFinalize(AVector: PVector);
begin
  FreeMem(AVector^.Data);
end;


{
  Removes all items from the vector.

  Since the vector buffer is never reallocated and shrunk, clearing the list is simply a matter of resetting the counter of
  items existing in the vector. The buffer memory block remains for the future.

  Arguments:
    • AVector — a pointer to the vector structure to clear.
}
procedure VectorClear(AVector: PVector);
begin
  AVector^.DataNum := 0;
end;


{
  Performs bubble sorting on the vector.

  [i] This function is implemented solely for the purposes of benchmarking the performance of access to items.

  [!] Warning! This function uses untyped pointers to access vector items. Incrementing and decrementing untyped pointers
      concerns the number of bytes, not the size of the item, so be aware of that.

  Arguments:
    • AVector   — a pointer to the vector structure to sort.
    • ACallback — a pointer to the callback function that compares the data of two items.
}
procedure VectorSortBubble(AVector: PVector; ACallback: TVectorCallbackCompare);
var
  DataLast:    Pointer;
  DataCurr:    Pointer;
  DataNext:    Pointer;
  Data:        Pointer;
  DataSwapped: Boolean;
begin
  // If there are not at least two items, there is nothing to sort.
  if AVector^.DataNum < 2 then exit;

  // Allocate the data block needed for data swap and set the sentinel.
  Data     := GetMem(AVector^.SizeData);
  DataLast := AVector^.Data + (AVector^.DataNum - 1) * AVector^.SizeData;

  repeat
    // Start a full iteration through the vector always at the first item.
    DataCurr    := AVector^.Data;
    DataNext    := DataCurr + AVector^.SizeData;
    DataSwapped := False;

    // Iterate until a sentinel is encountered.
    repeat
      // Compare neighboring items and, if necessary, swap them.
      if ACallback(DataCurr, DataNext) then
      begin
        Move(DataCurr^, Data^,     AVector^.SizeData);
        Move(DataNext^, DataCurr^, AVector^.SizeData);
        Move(Data^,     DataNext^, AVector^.SizeData);

        DataSwapped := True;
      end;

      // Regardless of whether there was a swap or not, go to the next item.
      DataCurr += AVector^.SizeData;
      DataNext += AVector^.SizeData;
    until DataCurr = DataLast;

    // Move the sentinel one item towards the beginning of the vector to reduce the number of iterations.
    DataLast -= AVector^.SizeData;
  until (DataLast = AVector^.Data) or not DataSwapped;

  // Free up a temporary data block for item data swap.
  FreeMem(Data);
end;


{
  Adds an item to the end of the vector.

  This function adds a new item to the end of the vector. If the vector is full and there is no space for a new item, the
  buffer memory block is relocated and doubled in size. Finally, the item data is copied from the parameter to the first free
  cell of the buffer.

  [i] If you need to insert an item anywhere in the vector, use the "VectorDataInsert" function.

  Arguments:
    • AVector — a pointer to the vector structure.
    • AData   — a pointer to the item data to be inserted into the list.
}
procedure VectorDataAppend(AVector: PVector; AData: Pointer);
var
  DataLast: Pointer;
begin
  // If the buffer is full, double its size.
  if AVector^.DataNum = AVector^.Capacity then
  begin
    AVector^.Capacity *= 2;
    AVector^.Data     := ReallocMem(AVector^.Data, AVector^.Capacity * AVector^.SizeData);
  end;

  // Calculate the address of the first free item and copy the input data there.
  DataLast := AVector^.Data + AVector^.DataNum * AVector^.SizeData;
  Move(AData^, DataLast^, AVector^.SizeData);

  // Increment the number of items in the vector.
  AVector^.DataNum += 1;
end;


{
  Inserts an item into a given place in the vector.

  This function is used to insert a new item anywhere in the vector. If the vector is full and there is no space for a new
  item, the buffer memory block is relocated and doubled in size. Then, if a new item is to be inserted inside the list (as
  a non-last item), the memory block of all subsequent items is moved towards the end of the buffer, making room for the new
  one. Finally, the new item's data is copied to the prepared place.

  Arguments:
    • AVector — a pointer to the vector structure.
    • AData   — a pointer to the item data to be inserted into the list.
    • AIndex  — index of the item in which the new one will be inserted.
}
procedure VectorDataInsert(AVector: PVector; AData: Pointer; AIndex: Int32);
var
  DataAt:     Pointer;
  DataAfter:  Pointer;
  DataLength: Int32;
begin
  // If the buffer is full, double its size.
  if AVector^.DataNum = AVector^.Capacity then
  begin
    AVector^.Capacity *= 2;
    AVector^.Data     := ReallocMem(AVector^.Data, AVector^.Capacity * AVector^.SizeData);
  end;

  // Calculate the address of the item where the input item will be inserted.
  DataAt := AVector^.Data + AIndex * AVector^.SizeData;

  // If it is not the last item of the vector, it is needed to make room for it.
  if AIndex < AVector^.DataNum then
  begin
    // Calculate the size of the memory block to be moved.
    DataAfter  := DataAt + AVector^.SizeData;
    DataLength := (AVector^.DataNum - AIndex) * AVector^.SizeData;

    // Move the memory block towards the end of the vector, making room for the new item.
    Move(DataAt^, DataAfter^, DataLength);
  end;

  // Copy the input item data to the prepared place and increment the vector's item counter.
  Move(AData^, DataAt^, AVector^.SizeData);
  AVector^.DataNum += 1;
end;


{
  Removes an item with a given index from the vector.

  This function is used to remove an item with a given index from the vector. When deleting not-last item, in addition to
  decrementing the item counter, the memory block of all subsequent items is moved towards the beginning of the buffer,
  overwriting the deleted item.

  Arguments:
    • AVector — a pointer to the vector structure.
    • AIndex  — index of the item to be removed from the vector.
}
procedure VectorDataDelete(AVector: PVector; AIndex: Int32);
var
  DataAt:     Pointer;
  DataAfter:  Pointer;
  DataLength: Int32;
begin
  // First, decrement the number of items. This will make the calculations easier.
  AVector^.DataNum -= 1;

  // If the item to delete is not the last one, the memory block must be moved.
  if AIndex < AVector^.DataNum then
  begin
    // Calculate the address and the length of the memory block to be moved.
    DataAt     := AVector^.Data + AIndex * AVector^.SizeData;
    DataAfter  := DataAt + AVector^.SizeData;
    DataLength := (AVector^.DataNum - AIndex) * AVector^.SizeData;

    // Move the memory block towards the beginning of the buffer, overwriting the data of the deleted item.
    Move(DataAfter^, DataAt^, DataLength);
  end;
end;


end.

