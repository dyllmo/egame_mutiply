@echo off
title ����������
cls
set time=5
echo * * * * * * * * * * * * * * * * * * * * * * *
echo *                                           *
echo *            ������������ʼ                 *
echo *                                           *
echo * * * * * * * * * * * * * * * * * * * * * * *
echo *                                           *
echo *            logger����������               *
echo *                                           *
echo * * * * * * * * * * * * * * * * * * * * * * *
start start_logger.bat 
choice /t %time% /d y /n >nul
echo *                                           *
echo *            logic1����������               *
echo *                                           *
echo * * * * * * * * * * * * * * * * * * * * * * *
start start_logic1.bat
choice /t %time% /d y /n >nul
echo *                                           *
echo *            logic2����������               *
echo *                                           *
echo * * * * * * * * * * * * * * * * * * * * * * *
start start_logic2.bat
choice /t %time% /d y /n >nul
echo *                                           *
echo *           login����������                 *
echo *                                           *
echo * * * * * * * * * * * * * * * * * * * * * * *
start start_login.bat
choice /t %time% /d y /n >nul
echo *                                           *
echo *              gate����������               *
echo *                                           *
echo * * * * * * * * * * * * * * * * * * * * * * *
start start_gate.bat
echo �������������