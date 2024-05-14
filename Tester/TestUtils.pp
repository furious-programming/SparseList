
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

interface


  function  TestGetTicks    (): Int64; inline;

  procedure TestPrintHeader ();
  procedure TestPrintFooter ();
  procedure TestPrintResult (AName: String; ATimeSimple, ATimeSparse, ATimeSparseDyn: Int64);


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

    'SIMPLE':RESULT_FIELD_SIZE_TIME,
    'SPARSE':RESULT_FIELD_SIZE_TIME,
    'SPARSE DYN':RESULT_FIELD_SIZE_TIME,

    'SIMPLE':RESULT_FIELD_SIZE_TIME,
    'SPARSE':RESULT_FIELD_SIZE_TIME,
    'SPARSE DYN':RESULT_FIELD_SIZE_TIME
  );
  WriteLn();
end;


procedure TestPrintFooter();
begin
  WriteLn(StringOfChar('_', RESULT_FIELD_SIZE_NAME + RESULT_FIELD_SIZE_TIME * 6 + 10));
  WriteLn();
end;


procedure TestPrintResult(AName: String; ATimeSimple, ATimeSparse, ATimeSparseDyn: Int64);
var
  PercentSimple:    Integer;
  PercentSparse:    Integer;
  PercentSparseDyn: Integer;
  TimeMin:          Integer;
begin
  Write(
    AName:RESULT_FIELD_SIZE_NAME,
    Format('%.0n', [ATimeSimple    + 0.0]):RESULT_FIELD_SIZE_TIME,
    Format('%.0n', [ATimeSparse    + 0.0]):RESULT_FIELD_SIZE_TIME,
    Format('%.0n', [ATimeSparseDyn + 0.0]):RESULT_FIELD_SIZE_TIME
  );

  TimeMin := ATimeSimple;

  if ATimeSparse    < TimeMin then TimeMin := ATimeSparse;
  if ATimeSparseDyn < TimeMin then TimeMin := ATimeSparseDyn;

  PercentSimple    := Round(ATimeSimple    * 100 / TimeMin);
  PercentSparse    := Round(ATimeSparse    * 100 / TimeMin);
  PercentSparseDyn := Round(ATimeSparseDyn * 100 / TimeMin);

  Write(
    IfThen(PercentSimple    = 100, '-', Format('%.0n%%', [PercentSimple    - 100.0])):RESULT_FIELD_SIZE_TIME,
    IfThen(PercentSparse    = 100, '-', Format('%.0n%%', [PercentSparse    - 100.0])):RESULT_FIELD_SIZE_TIME,
    IfThen(PercentSparseDyn = 100, '-', Format('%.0n%%', [PercentSparseDyn - 100.0])):RESULT_FIELD_SIZE_TIME
  );

  PercentSimple    := 1 + Ord(ATimeSimple    > ATimeSparse) + Ord(ATimeSimple    > ATimeSparseDyn);
  PercentSparse    := 1 + Ord(ATimeSparse    > ATimeSimple) + Ord(ATimeSparse    > ATimeSparseDyn);
  PercentSparseDyn := 1 + Ord(ATimeSparseDyn > ATimeSimple) + Ord(ATimeSparseDyn > ATimeSparse);

  WriteLn('    ',
    IfThen(PercentSimple    = 1, '-', PercentSimple.ToString()), ' ',
    IfThen(PercentSparse    = 1, '-', PercentSparse.ToString()), ' ',
    IfThen(PercentSparseDyn = 1, '-', PercentSparseDyn.ToString())
  );
end;


end.

