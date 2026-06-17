@echo off
echo # Pretty printing

for %%f in (pp-*.fw) do (
FWhile.exe %%f -p > %%~nf.sal

fc %%~nf.out %%~nf.sal > nul

if errorlevel 1 (
    echo.
    echo ===== %%~nf FAIL =====
    fc %%~nf.out %%~nf.sal
    echo.
) else (
    echo %%~nf: OK
)

)
