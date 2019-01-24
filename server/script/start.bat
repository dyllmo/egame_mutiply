@echo off
title 服务器启动
cls
set time=5
echo * * * * * * * * * * * * * * * * * * * * * * *
echo *                                           *
echo *            服务器启动开始                 *
echo *                                           *
echo * * * * * * * * * * * * * * * * * * * * * * *
echo *                                           *
echo *            logger服务器启动               *
echo *                                           *
echo * * * * * * * * * * * * * * * * * * * * * * *
start start_logger.bat 
choice /t %time% /d y /n >nul
echo *                                           *
echo *            logic1服务器启动               *
echo *                                           *
echo * * * * * * * * * * * * * * * * * * * * * * *
start start_logic1.bat
choice /t %time% /d y /n >nul
echo *                                           *
echo *            logic2服务器启动               *
echo *                                           *
echo * * * * * * * * * * * * * * * * * * * * * * *
start start_logic2.bat
choice /t %time% /d y /n >nul
echo *                                           *
echo *           login服务器启动                 *
echo *                                           *
echo * * * * * * * * * * * * * * * * * * * * * * *
start start_login.bat
choice /t %time% /d y /n >nul
echo *                                           *
echo *              gate服务器启动               *
echo *                                           *
echo * * * * * * * * * * * * * * * * * * * * * * *
start start_gate.bat
echo 服务器启动完成