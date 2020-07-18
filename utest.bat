@ECHO OFF
CLS
SET RUNT="D:\Development\Rust\untod\target\release\untod.exe"
SET UNTOD="D:\Development\Haskell\untod\.stack-work\install\f9a1e984\bin\untod.exe"
SET RUNS= %UNTOD% -a
SET RUNP=ECHO untod
ECHO ======================================================================================================
%RUNS% -vvv
ECHO ------------------------------------------------------------------------------------------------------
%UNTOD% -z --azone=-8 
ECHO ======================================================================================================
SET UNTOD_LZONE=0
SET UNTOD_AZONE=
SET UNTOD_AZONE=-4
SET UNTOD_LZONE=1
SET PARM=-z --lzone=1 --azone=-8 -d 2020-07-19@01
%RUNP% %PARM% 
%RUNS% %PARM% 
REM %RUNT% %PARM% 
REM ECHO ------------------------------------------------------------------------------------------------------
%RUNP% -l %PARM% 
%RUNS% -l %PARM% 
REM %RUNT% -l %PARM% 
REM ECHO ------------------------------------------------------------------------------------------------------
%RUNP% -t %PARM% 
%RUNS% -t %PARM% 
REM %RUNT% -t %PARM% 
ECHO ======================================================================================================
REM SET PARM=-d 2017-01-01
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
