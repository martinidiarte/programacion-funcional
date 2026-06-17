@echo off
set FW=FWhile.exe

set do_pp=0
set do_nc=0
set do_ty=0
set do_eval=0
set do_evalerr=0

if "%~1"=="" (
    set do_pp=1
    set do_nc=1
    set do_ty=1
    set do_eval=1
    set do_evalerr=1
)

:parse
if "%~1"=="" goto endparse
if "%~1"=="-p" set do_pp=1
if "%~1"=="-n" set do_nc=1
if "%~1"=="-t" set do_ty=1
if "%~1"=="-e" set do_eval=1
if "%~1"=="-r" set do_evalerr=1
shift
goto parse

:endparse

if %do_pp%==1 (
    echo # Pretty printing
    for %%f in (pp-*.fw) do (
        %FW% %%f -p > %%~nf.sal
        fc /n %%~nf.out %%~nf.sal > nul
        if errorlevel 1 (
            echo %%~nf: FAIL
        ) else (
            echo %%~nf: OK
        )
    )
)

if %do_nc%==1 (
    echo # Chequeo de nombres
    for %%f in (nc-*.fw) do (
        %FW% %%f > %%~nf.sal
        fc /n %%~nf.out %%~nf.sal > nul
        if errorlevel 1 (
            echo %%~nf: FAIL
        ) else (
            echo %%~nf: OK
        )
    )
)

if %do_ty%==1 (
    echo # Chequeo de tipos
    for %%f in (ty-*.fw) do (
        %FW% %%f > %%~nf.sal
        fc /n %%~nf.out %%~nf.sal > nul
        if errorlevel 1 (
            echo %%~nf: FAIL
        ) else (
            echo %%~nf: OK
        )
    )
)

if %do_eval%==1 (
    echo # Evaluacion
    for %%f in (eval-*.fw) do (
        type nul > %%~nf.sal
        for /f "delims=" %%l in (%%~nf.in) do (
            %FW% %%f -e "%%l" >> %%~nf.sal
        )
        fc /n %%~nf.out %%~nf.sal > nul
        if errorlevel 1 (
            echo %%~nf: FAIL
        ) else (
            echo %%~nf: OK
        )
    )
)

if %do_evalerr%==1 (
    echo # Evaluacion con errores
    for %%f in (evalerr-*.fw) do (
        type nul > %%~nf.sal
        for /f "delims=" %%l in (%%~nf.in) do (
            %FW% %%f -e "%%l" >> %%~nf.sal
        )
        fc /n %%~nf.out %%~nf.sal > nul
        if errorlevel 1 (
            echo %%~nf: FAIL
        ) else (
            echo %%~nf: OK
        )
    )
)