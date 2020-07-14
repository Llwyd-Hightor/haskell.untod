@ECHO OFF
CLS
ECHO ======================================================================================================
REM SET UNTOD_LZONE=0
REM SET UNTOD_AZONE=0
REM SET PARM=D1E0D6A173CC0
REM STACK exec UNTOD -- --headers -a %PARM%%
REM UNTOD %PARM%
REM STACK exec UNTOD -- -al %PARM%%
REM UNTOD -l %PARM%
REM STACK exec UNTOD -- -at %PARM%%
REM UNTOD -t %PARM%
SET UNTOD_LZONE=
SET UNTOD_AZONE=-8
SET PARM=D1E0D6A173CC0
STACK exec UNTOD -- --headers -a %PARM%%
UNTOD %PARM%
STACK exec UNTOD -- -al %PARM%%
UNTOD -l %PARM%
STACK exec UNTOD -- -at %PARM%%
UNTOD -t %PARM%
ECHO ======================================================================================================
