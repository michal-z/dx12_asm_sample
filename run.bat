@echo off

set FXC=compilers\fxc.exe /Ges /O3 /nologo /WX /Qstrip_reflect /Qstrip_debug /Qstrip_priv

%FXC% /E object_vs /Fo data\shader\object_vs.cso /T vs_5_1 hlsl\object.hlsl
if errorlevel 1 goto error

%FXC% /E object_ps /Fo data\shader\object_ps.cso /T ps_5_1 hlsl\object.hlsl
if errorlevel 1 goto error

%FXC% /E mipgen_cs /Fo data\shader\mipgen_cs.cso /T cs_5_1 hlsl\mipgen_cs.hlsl
if errorlevel 1 goto error


compilers\fasm.exe eneida.asm
if errorlevel 1 goto error

eneida.exe
goto eof

:error
pause

:eof