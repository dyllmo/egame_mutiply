@echo off
title login
cls
erl                                                 ^
+A               10                                 ^
+P               1000000                            ^
+IOp             10                                 ^
+IOt             10                                 ^
-boot            start_sasl                         ^
-pa              ../ebin                            ^
-s               game start                         ^
-name            login@127.0.0.1                    ^
-kernel 		 logger_sasl_compatible true        ^
-setcookie       the_cookie                         ^
-connect_all     false                              ^
-config          ../conf/login                      ^
-env             ERL_MAX_ETS_TABLES 65535           ^
-game                                               ^
 sid             '2'                                ^
 tid             '2'                                ^
 cid             '1'                                ^
 bid             '0'                                ^
 server_port     '8001'                             ^
 mysql_host      '127.0.0.1'                        ^
 mysql_port      '3306'                             ^
 mysql_username	 'root'                             ^
 mysql_password	 'mjmjmj'                           ^
 mysql_database	 'gamedb_old'                          ^
 crt_file        '"../cert/debug.crt"'              ^
 key_file        '"../cert/debug.key"'              ^
 code_db_enable  'true'                             ^
 is_prof         'false'                            ^
 is_printf_req   'false'                            ^
 is_debug        'true'                             ^
pause