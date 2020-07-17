@ECHO OFF
SET RUNT="D:\Development\Rust\untod\target\release\untod.exe"
SET RUNS=STACK exec UNTOD -- -a -l
SET RUNP=ECHO Command: untod
ECHO ======================================================================================================
%RUNS% -v -v
ECHO ======================================================================================================
SET UNTOD_LZONE=0
SET UNTOD_AZONE=
SET PARM=-d
ECHO Test: Today
%RUNP% %PARM%
%RUNS% %PARM%
REM %RUNT% %PARM%
ECHO ------------------------------------------------------------------------------------------------------
ECHO Test: IETF Leap second #27
SET PARM=-s 3692217600
%RUNP% %PARM%
%RUNS% %PARM%
REM %RUNT% %PARM%
ECHO ------------------------------------------------------------------------------------------------------
ECHO Test: IETF Leap second #27 -1
SET PARM=-s 3692217599
%RUNP% %PARM%
%RUNS% %PARM%
REM %RUNT% %PARM%
ECHO ------------------------------------------------------------------------------------------------------
ECHO Test: IETF Leap second #27 from TOD
SET PARM=d1e0d68173cc0
%RUNP% %PARM%
%RUNS% %PARM%
REM %RUNT% %PARM%
ECHO ------------------------------------------------------------------------------------------------------
ECHO Test: IETF Leap second #27 from Date
SET PARM=-d 2017.001
%RUNP% %PARM%
%RUNS% %PARM%
REM %RUNT% %PARM%
ECHO ------------------------------------------------------------------------------------------------------
ECHO Test: IETF Leap second #27 from PMC
SET PARM=-m 01994340
%RUNP% %PARM%
%RUNS% %PARM%
REM %RUNT% %PARM%
ECHO ------------------------------------------------------------------------------------------------------
ECHO Test: IETF Leap second #27 from Unix
SET PARM=-u 1483228800
%RUNP% %PARM%
%RUNS% %PARM%
REM %RUNT% %PARM%

ECHO ======================================================================================================
SET UNTOD_AZONE=-4
SET UNTOD_LZONE=1
