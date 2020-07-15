@ECHO OFF
CLS
SET RUNT="D:\Development\Rust\untod\target\release\untod.exe"
SET RUNS=STACK exec UNTOD -- -a
ECHO ======================================================================================================
%RUNS% --vv 
ECHO ======================================================================================================
SET UNTOD_LZONE=1
SET UNTOD_AZONE=-4
SET PARM=-m 01AD76F3

%RUNS% %PARM% 
%RUNT% %PARM% 
ECHO ======================================================================================================
REM %RUNS% -l %PARM% 
REM %RUNT% -l %PARM% 
REM %RUNS% -t %PARM% 
REM %RUNT% -t %PARM% 
REM %RUNS% %PARM% 2
REM %RUNS% %PARM% 2020
REM %RUNS% %PARM% 2020-
REM %RUNS% %PARM% 2020-07
REM %RUNS% %PARM% 2020-07-11
REM %RUNS% %PARM% 2020-07-11@22
REM %RUNS% %PARM% 2020-07-11@22:15
REM %RUNS% %PARM% 2020-07-11@22:15:30
REM %RUNS% %PARM% 2020-07-11@22:15:30.25
REM SET PARM=-a D1E0D6A173CC0
REM STACK exec UNTOD -- --headers %PARM%%
REM STACK exec UNTOD -- -l %PARM%%
REM %RUNT% -l %PARM%
REM STACK exec UNTOD -- -t %PARM%%
REM %RUNT% -t %PARM%
REM SET UNTOD_LZONE=
REM SET UNTOD_AZONE=-8
REM SET PARM=D1E0D6A173CC0
REM STACK exec UNTOD -- --headers -a %PARM%%
REM %RUNT% -a %PARM%
REM STACK exec UNTOD -- -al %PARM%%
REM %RUNT% -al %PARM%
REM STACK exec UNTOD -- -at %PARM%%
REM %RUNT% -at %PARM%
