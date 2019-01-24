%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 数据库日志
%%--------------------------------------------------------------------------------------------------------------
-module(database_log).

-behavior(gen_server2).

-export([
	start_link/0,
	init/1, 
	handle_call/3,	
	handle_cast/2,
	handle_info/2, 
	terminate/2, 
	code_change/3
]).

-include("game.hrl").
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 数据库日志路径
%% @end
%%--------------------------------------------------------------------------------------------------------------
data_dir () ->
	lib_misc:get_env_str(data_dir,"./data").
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 打开日志文件
%% @end
%%--------------------------------------------------------------------------------------------------------------
open_log_file () ->
	{{Y,M,D},{H,_MM,_SS}} = erlang:localtime(),
    FileName = data_dir() ++ "/" ++ integer_to_list(Y) ++ "_" ++ integer_to_list(M) ++ "_" ++ integer_to_list(D) ++ "/" ++ integer_to_list(H) ++ ".sql",
	case filelib:is_file(FileName) of
		true -> 
			file:open(FileName,[append,raw,{delayed_write,1024,1000}]);
		false -> 
			ok = filelib:ensure_dir(FileName),
			Return = {ok,File} = file:open(FileName,[append,raw,{delayed_write,1024,1000}]),
			ok = file:write(File, <<"/*!40101 SET NAMES utf8 */;\n">>),
			ok = file:write(File, <<"/*!40101 SET SQL_MODE=''*/;\n">>),
			ok = file:write(File, <<"/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;\n">>),
			ok = file:write(File, <<"/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;\n">>),
			ok = file:write(File, <<"/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;\n">>),
			ok = file:write(File, <<"/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;\n\n">>),
			Return
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% API
%% @end
%%--------------------------------------------------------------------------------------------------------------
start_link () ->
    gen_server2:start_link({local,?MODULE},?MODULE,[],[]).

init ([]) ->
	file:make_dir(data_dir()),
	{ok,File} = open_log_file(),
	LeftTime = lib_misc:hour_left_second(),
	erlang:send_after(LeftTime * 1000,self(),change_file),
    {ok,File,hibernate,?BACKOFF}.

handle_call (Msg, _From, File) ->
    {reply,{ok,Msg},File,hibernate}.
	
handle_cast (_Msg, File) ->
    {noreply,File,hibernate}.
	
handle_info ({do, SqlList}, File) ->
	case catch file:write(File,[<<"\n">>|SqlList]) of
		ok -> 
			ok;
		Result -> 
			?ERROR(
				"database_log_error:~n"
				"    sql_list => ~p~n"
				"    result   => ~p~n",
				[SqlList,Result]
			)
	end,
    {noreply,File,hibernate};
	
handle_info (change_file, File) ->
	ok = file:close(File), 
	{ok,NewFile} = open_log_file(),
	LeftTime = lib_misc:hour_left_second(),
	erlang:send_after(LeftTime * 1000,self(),change_file),
    {noreply,NewFile,hibernate};
	
handle_info (_Msg, File) ->
	ok = file:close(File), 
    {noreply,File,hibernate}.

terminate (_Reason, File) ->
    {noreply,File}.

code_change (_OldSvn, File, _Ext) ->
    {noreply,File}.