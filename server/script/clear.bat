@echo off
title clear
del /s /q ..\ebin\*.beam
del /s /q ..\ebin\*.erl
del /s /q ..\src\gen\*.erl
del /s /q ..\include\gen\*.hrl
pause