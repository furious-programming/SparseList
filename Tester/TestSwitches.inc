
{
  Tester Switches.
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


// For all builds.
{$MODE       FPC}
{$ASSERTIONS OFF}

// Enabled language features.
{$MODESWITCH ALLOWINLINE+}
{$MODESWITCH RESULT+}

// Disabled language features.
{$MODESWITCH AUTODEREF-}
{$MODESWITCH EXCEPTIONS-}
{$MODESWITCH NESTEDPROCVARS-}


// For debug build.
{$IFDEF BUILD_DEBUG}
  {$INLINE       OFF}
  {$SMARTLINK    OFF}
  {$OPTIMIZATION OFF}

  {$S+}
  {$IOCHECKS       ON}
  {$RANGECHECKS    ON}
  {$OVERFLOWCHECKS ON}
{$ENDIF}


// For release build.
{$IFDEF BUILD_RELEASE}
  {$INLINE       ON}
  {$SMARTLINK    ON}
  {$OPTIMIZATION LEVEL3}
  {$OPTIMIZATION AUTOINLINE}

  {$S-}
  {$IOCHECKS       OFF}
  {$RANGECHECKS    OFF}
  {$OVERFLOWCHECKS OFF}

  {$IMPLICITEXCEPTIONS OFF}
{$ENDIF}

