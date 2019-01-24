@echo off
title build
cd ../ebin
erl -noshell -s make all -s init stop
pause