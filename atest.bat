@ECHO OFF
CLS
SET RUNT="D:\Development\Rust\untod\target\release\untod.exe"
SET RUNS=STACK exec UNTOD -- -a
SET RUNP=ECHO untod
ECHO ======================================================================================================
%RUNS% -v -v
ECHO ======================================================================================================
SET UNTOD_LZONE=0
SET UNTOD_AZONE=
SET PARM=-u 1483228800
%RUNP% %PARM%
%RUNS% %PARM%
%RUNT% %PARM%

ECHO ======================================================================================================
SET UNTOD_AZONE=-4
SET UNTOD_LZONE=1
