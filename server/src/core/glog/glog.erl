%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 日志进程
%%--------------------------------------------------------------------------------------------------------------
-module(glog).

-behavior(gen_server2).

-export([
	call_write/3,		%%-----同步写入
	cast_write/3,		%%-----异步写入
	write_server/4,		%%-----跨服写入
	write_local/4		%%-----本服写入
]).

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
%% 同步写入
%% @end
%%--------------------------------------------------------------------------------------------------------------
call_write (Level, Message, Arguments) ->
	write_server(call,Level,Message,Arguments).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 异步写入
%% @end
%%--------------------------------------------------------------------------------------------------------------
cast_write (Level, Message, Arguments) ->
	write_server(cast,Level,Message,Arguments).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 跨服写入
%% @end
%%--------------------------------------------------------------------------------------------------------------
write_server (Mode, Level, Message, Arguments) ->
	case Level of
		error ->
			case cluster:tid_ping(?SERVER_T_LOGGER) of
				pong ->
					cluster:tid_cast(?SERVER_T_LOGGER,?MODULE,write_local,[Mode,Level,Message,Arguments]);
				pang ->
					write_local(Mode,Level,Message,Arguments)
			end;
		_ ->
			write_local(Mode,Level,Message,Arguments)
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 本服写入
%% @end
%%--------------------------------------------------------------------------------------------------------------
write_local (Mode, Level, Message, Arguments) ->
	case Level of
		debug ->
			case ?IS_DEBUG of
				true ->
					write_log({debug,lib_misc:get_player_id(),Message,Arguments},null);
				false ->
					ok
			end;
		_ ->
			Req = lib_misc:compress({Level,lib_misc:get_player_id(),Message,Arguments}),
			case Mode of
				call ->
					gen_server2:call(?MODULE,{log,Req},?CALL_TIMEOUT);
				cast ->
					gen_server2:cast(?MODULE,{log,Req})
			end
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 日志路径
%% @end
%%--------------------------------------------------------------------------------------------------------------
log_dir () ->
	lib_misc:get_env_str(log_dir,"./log").
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 打开日志文件
%% @end
%%--------------------------------------------------------------------------------------------------------------
open_log_file ({Y, M, D}) ->
    FileName = log_dir() ++ "/" ++ integer_to_list(Y) ++ "_" ++ integer_to_list(M) ++ "_" ++ integer_to_list(D) ++ "/error.log",
	case filelib:is_file(FileName) of
		true -> 
			ok;
		false -> 
			ok = filelib:ensure_dir(FileName)
	end,
	file:open(FileName,[append,raw,{delayed_write,100 * 1024,2000}]).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 获取标题
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_log_title (PlayerId) when PlayerId =< 0 ->
    lib_misc:format_time() ++ "~n";
	
get_log_title (PlayerId) ->
	lib_misc:format_time() ++ " from player " ++ integer_to_list(PlayerId) ++ "~n".
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 尝试写入日志
%% @end
%%--------------------------------------------------------------------------------------------------------------
try_write_log (LogBin, {InDate,File} = State) ->
	Log = lib_misc:uncompress(LogBin),
	Date = erlang:date(),
	NewState = {_,NowFile} = if
		Date == InDate ->
			State;
		true ->
			ok = file:close(File),
			{ok,NewFile} = open_log_file(Date),
			{Date,NewFile}
	end,
	case catch write_log(Log,NowFile) of
		{'EXIT',Reason} ->
			io:format("log write failed: ~p~n",[Reason]);
		_ ->
			ok
	end,
	NewState.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 写入日志
%% @end
%%--------------------------------------------------------------------------------------------------------------
write_log ({error, PlayerId, Message, Arguments}, File) ->
	LogTitle = get_log_title(PlayerId),
	write_file(File,LogTitle,Message,Arguments),
	case ?IS_DEBUG of
		true ->
			if
				PlayerId > 0 ->
					io:format("[error] - from player ~s~n" ++ Message ++ "~n",[lib_misc:format_time()|Arguments]);
				true ->
					io:format("[error] - ~s~n" ++ Message ++ "~n",[lib_misc:format_time()|Arguments])
			end;
		false ->
			ok
	end;
	
write_log ({Level, PlayerId, Message, Arguments}, _) ->
	if
		PlayerId > 0 ->
			io:format("[~p] - from player " ++ integer_to_list(PlayerId) ++ " : " ++ Message,[Level|Arguments]);
		true ->
			io:format("[~p] - " ++ Message,[Level|Arguments])
	end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 写入日志文件
%% @end
%%--------------------------------------------------------------------------------------------------------------
write_file (File, LogTitle, Message, Arguments) ->
	LogContent = io_lib:format(LogTitle ++ Message ++ "~n~n", Arguments),
    LogBin = list_to_binary(LogContent),
    ok = file:write(File,LogBin),
	case file:datasync(File) of
		{error,Reason} ->
			io:format("glog write log failed: ~p~n",[Reason]);
		ok ->
			ok
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% API
%% @end
%%--------------------------------------------------------------------------------------------------------------
start_link () ->
    gen_server2:start_link({local,?MODULE},?MODULE,[],[]).

init ([]) ->
	file:make_dir(log_dir()),
	Date = erlang:date(),
	{ok,File} = open_log_file(Date),
    {ok,{Date,File},hibernate,?BACKOFF}.
	
handle_call ({log,Log}, _From, State) ->
	NewState = try_write_log(Log,State),
    {reply,ok,NewState,hibernate};
	
handle_call (Msg, _From, State) ->
    {reply,{ok,Msg,State},State,hibernate}.
	
handle_cast ({log, Log}, State) ->
	NewState = try_write_log(Log,State),
    {noreply,NewState,hibernate};
	
handle_cast (_Msg, State) ->
    {noreply,State,hibernate}.
			
handle_info (_Msg, State) ->
    {noreply,State,hibernate}.

terminate (_Reason, State) ->
    {noreply,State}.

code_change (_OldSvn, State, _Ext) ->
    {noreply,State}.