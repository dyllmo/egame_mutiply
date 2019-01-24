@echo off
title gate
cls
erl                                                 ^
+A               10                                 ^
+P               1000000                            ^
+IOp             10                                 ^
+IOt             10                                 ^
-boot            start_sasl                         ^
-pa              ../ebin                            ^
-s               game start                         ^
-name            gate@127.0.0.1                     ^
-kernel 		 logger_sasl_compatible true        ^
-setcookie       the_cookie                         ^
-connect_all     false                              ^
-config          ../conf/gate                       ^
-env             ERL_MAX_ETS_TABLES 65535           ^
-game                                               ^
 sid             '1'                                ^
 tid             '1'                                ^
 cid             '1'                                ^
 bid             '0'                                ^
 server_port     '8000'                             ^
 mysql_host      '127.0.0.1'                        ^
 mysql_port      '3306'                             ^
 mysql_username	 'root'                             ^
 mysql_password	 'mjmjmj'                           ^
 mysql_database	 'gamedb_old'                          ^
 crt_file        '"../cert/debug.crt"'              ^
 key_file        '"../cert/debug.key"'              ^
 code_db_enable  'false'                            ^
 is_prof         'false'                            ^
 is_printf_req   'false'                            ^
 is_debug        'true'                             ^
pause