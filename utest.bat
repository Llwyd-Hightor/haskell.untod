@ECHO OFF
CLS
SET RUNT="D:\Development\Rust\untod\target\release\untod.exe"
ECHO ======================================================================================================
SET UNTOD_LZONE=0
SET UNTOD_AZONE=0
SET PARM=--csv D1E0D6A173CC0
STACK exec UNTOD -- --headers %PARM%%
%RUNT% %PARM%
STACK exec UNTOD -- -l %PARM%%
%RUNT% -l %PARM%
STACK exec UNTOD -- -t %PARM%%
%RUNT% -t %PARM%
REM SET UNTOD_LZONE=
REM SET UNTOD_AZONE=-8
REM SET PARM=D1E0D6A173CC0
REM STACK exec UNTOD -- --headers -a %PARM%%
REM %RUNT% -a %PARM%
REM STACK exec UNTOD -- -al %PARM%%
REM %RUNT% -al %PARM%
REM STACK exec UNTOD -- -at %PARM%%
REM %RUNT% -at %PARM%
ECHO ======================================================================================================
