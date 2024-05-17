
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


  function  TestGetTicks    (): Int64; inline;

  procedure TestPrintHeader ();
  procedure TestPrintFooter ();
  procedure TestPrintResult (AName: String; ATimeVector, ATimeSimple, ATimeSparse, ATimeSparseDyn: Int64);


implementation

uses
  Windows,
  SysUtils,
  StrUtils;


const
  RESULT_FIELD_SIZE_NAME = 10;
  RESULT_FIELD_SIZE_TIME = 16;


function TestGetTicks(): Int64;
begin
  Result := 0;
  QueryPerformanceCounter(Result);
end;


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


procedure TestPrintFooter();
begin
  WriteLn(StringOfChar('_', RESULT_FIELD_SIZE_NAME + RESULT_FIELD_SIZE_TIME * 8 + 10));
  WriteLn();
end;


procedure TestPrintResult(AName: String; ATimeVector, ATimeSimple, ATimeSparse, ATimeSparseDyn: Int64);
var
  PlaceVector:    Int64;
  PlaceSimple:    Int64;
  PlaceSparse:    Int64;
  PlaceSparseDyn: Int64;
  TimeMin:        Int64;
begin
  ATimeVector    := Max(1, ATimeVector);
  ATimeSimple    := Max(1, ATimeSimple);
  ATimeSparse    := Max(1, ATimeSparse);
  ATimeSparseDyn := Max(1, ATimeSparseDyn);

  Write(
    AName:RESULT_FIELD_SIZE_NAME,
    Format('%.0n', [ATimeVector    + 0.0]):RESULT_FIELD_SIZE_TIME,
    Format('%.0n', [ATimeSimple    + 0.0]):RESULT_FIELD_SIZE_TIME,
    Format('%.0n', [ATimeSparse    + 0.0]):RESULT_FIELD_SIZE_TIME,
    Format('%.0n', [ATimeSparseDyn + 0.0]):RESULT_FIELD_SIZE_TIME
  );

  TimeMin := ATimeVector;

  if ATimeSimple    < TimeMin then TimeMin := ATimeSimple;
  if ATimeSparse    < TimeMin then TimeMin := ATimeSparse;
  if ATimeSparseDyn < TimeMin then TimeMin := ATimeSparseDyn;

  Write(
    IfThen(ATimeVector    = TimeMin, '-', Format('%.2nx', [ATimeVector    / TimeMin])):RESULT_FIELD_SIZE_TIME,
    IfThen(ATimeSimple    = TimeMin, '-', Format('%.2nx', [ATimeSimple    / TimeMin])):RESULT_FIELD_SIZE_TIME,
    IfThen(ATimeSparse    = TimeMin, '-', Format('%.2nx', [ATimeSparse    / TimeMin])):RESULT_FIELD_SIZE_TIME,
    IfThen(ATimeSparseDyn = TimeMin, '-', Format('%.2nx', [ATimeSparseDyn / TimeMin])):RESULT_FIELD_SIZE_TIME
  );

  PlaceVector    := 1 + Ord(ATimeVector    > ATimeSimple) + Ord(ATimeVector    > ATimeSparse) + Ord(ATimeVector    > ATimeSparseDyn);
  PlaceSimple    := 1 + Ord(ATimeSimple    > ATimeVector) + Ord(ATimeSimple    > ATimeSparse) + Ord(ATimeSimple    > ATimeSparseDyn);
  PlaceSparse    := 1 + Ord(ATimeSparse    > ATimeVector) + Ord(ATimeSparse    > ATimeSimple) + Ord(ATimeSparse    > ATimeSparseDyn);
  PlaceSparseDyn := 1 + Ord(ATimeSparseDyn > ATimeVector) + Ord(ATimeSparseDyn > ATimeSimple) + Ord(ATimeSparseDyn > ATimeSparse);

  WriteLn('    ',
    IfThen(PlaceVector    = 1, '-', PlaceVector.ToString()), ' ',
    IfThen(PlaceSimple    = 1, '-', PlaceSimple.ToString()), ' ',
    IfThen(PlaceSparse    = 1, '-', PlaceSparse.ToString()), ' ',
    IfThen(PlaceSparseDyn = 1, '-', PlaceSparseDyn.ToString())
  );
end;


end.

