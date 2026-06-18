@echo off
set FW=FWhile.exe

echo # Chequeo de nombres

for %%f in (nc-*.fw) do (
set base=%%~nf
%FW% %%f > %%~nf.sal

fc /n %%~nf.out %%~nf.sal > nul

if errorlevel 1 (
    echo %%~nf: FAIL
    fc /n %%~nf.out %%~nf.sal
) else (
    echo %%~nf: OK
)

)
