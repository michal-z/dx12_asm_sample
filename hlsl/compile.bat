@echo off

set FXC=fxc.exe /Ges /O3 /nologo /WX /Qstrip_reflect /Qstrip_debug /Qstrip_priv

%FXC% /E object_vs /Fo object_vs.cso /T vs_5_1 object.hlsl
if errorlevel 1 goto eof

%FXC% /E object_ps /Fo object_ps.cso /T ps_5_1 object.hlsl
if errorlevel 1 goto eof

%FXC% /E mipgen_cs /Fo mipgen_cs.cso /T cs_5_1 mipgen_cs.hlsl
if errorlevel 1 goto eof

:eof
