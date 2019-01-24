%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 在线玩家管理
%%--------------------------------------------------------------------------------------------------------------
-module(online).

-export([
	add_player/2,									%%-----加入在线玩家表(当前进程)
	add_player/3,									%%-----加入在线玩家表
    del_player/1,									%%-----删除在线玩家表
	is_online/1,									%%-----玩家是否在线
	get_all_sid_pid_list/0,							%%-----所有节点在线玩家{sid,pid}列表(不排除玩家)
	get_all_sid_pid_list/1,							%%-----所有节点在线玩家{sid,pid}列表(排除玩家)
	get_local_sid_pid_list/0,						%%-----当前节点在线玩家{sid,pid}列表(不排除玩家)
	get_local_sid_pid_list/1,						%%-----当前节点在线玩家{sid,pid}列表(排除玩家)
	get_all_player_id_list/0,						%%-----所有节点在线玩家id列表(不排除玩家)
	get_all_player_id_list/1,						%%-----所有节点在线玩家id列表(排除玩家)
	get_local_player_id_list/0,						%%-----当前节点在线玩家id列表(不排除玩家)
	get_local_player_id_list/1,						%%-----当前节点在线玩家id列表(排除玩家)
    send_to_player/2,								%%-----发送数据给在线玩家
    send_to_all_players/1,							%%-----发送数据给所有节点所有在线玩家(不排除玩家)
    send_to_local_players/1,						%%-----发送数据给当前节点所有在线玩家(不排除玩家)
	send_to_all_players_without_one/2,				%%-----发送数据给所有节点所有在线玩家(排除一个玩家)
	send_to_local_players_without_one/2,			%%-----发送数据给当前节点所有在线玩家(排除一个玩家)
	apply_to_player/4,								%%-----同步调用玩家进程执行
    async_apply_to_player/4,						%%-----异步调用玩家进程执行(无回调)
    async_apply_to_player/5,						%%-----异步调用玩家进程执行(有回调)
	apply_to_all_players/3,							%%-----同步调用所有节点所有玩家进程执行
	async_apply_to_all_players/3,					%%-----异步调用所有节点所有玩家进程执行
	kill_player/1,									%%-----将玩家踢下线
    kill_all_players/0,								%%-----将所有节点所有玩家踢下线
    kill_local_players/0,							%%-----将当前节点所有玩家踢下线
	wait_player_exit/0,								%%-----将当前节点所有玩家踢下线并在时间内等待(默认1800秒)
	wait_player_exit/1,								%%-----将当前节点所有玩家踢下线并在时间内等待
    count_all_player/0,								%%-----所有节点在线玩家人数
    count_local_player/0							%%-----当前节点在线玩家人数
]).

-include("game.hrl").

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 获取在线玩家信息
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_online_player (PlayerId) ->
	case cluster:player_id_call(PlayerId,lib_ets,get,[online,PlayerId]) of
		{badrpc,_} ->
			[];
		Return ->
			Return
	end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 玩家是否在线
%% @end
%%--------------------------------------------------------------------------------------------------------------
is_online (PlayerId) ->
	get_online_player(PlayerId) =/= [].
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 加入在线玩家表
%% @end
%%--------------------------------------------------------------------------------------------------------------
add_player (PlayerId, Sid) ->
    add_player(PlayerId,Sid,self()).
	
add_player (PlayerId, Sid, Pid) ->
	case cluster:player_id_call(
		PlayerId,
		lib_ets,
		insert,
		[online,#online{player_id = PlayerId,sid = Sid,pid = Pid},replace]
	) of	
		{badrpc,_} ->
			false;
		Return ->
			Return
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 删除在线玩家表
%% @end
%%--------------------------------------------------------------------------------------------------------------
del_player (PlayerId) ->
	case cluster:player_id_call(PlayerId,lib_ets,delete,[online,PlayerId]) of
		{badrpc,_} ->
			false;
		Return ->
			Return
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 所有节点在线玩家{sid,pid}列表(不排除玩家)
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_all_sid_pid_list () ->
	get_all_sid_pid_list(0).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 所有节点在线玩家{sid,pid}列表(排除玩家)
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_all_sid_pid_list (PlayerId) ->
	lists:foldl(
		fun(Sid,List) ->
			case cluster:sid_call(Sid,?MODULE,get_local_sid_pid_list,[PlayerId]) of
				{badrpc,_} ->
					List;
				Return ->
					lists:append(Return,List)
			end
		end,
		[],
		cluster:tid_get_sid_list(?SERVER_T_LOGIC)
	).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 当前节点在线玩家{sid,pid}列表(不排除玩家)
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_local_sid_pid_list () ->
	get_local_sid_pid_list(0).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 当前节点在线玩家{sid,pid}列表(排除玩家)
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_local_sid_pid_list (PlayerId) ->
	lib_ets:select(
		online,
		[{#online{player_id = '$1',sid = '$2',pid = '$3',_ = '_'},[{'=/=','$1',PlayerId}],[{{'$2','$3'}}]}]
	).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 在线玩家id列表(不排除玩家)
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_all_player_id_list () ->
	get_all_player_id_list(0).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 所有节点在线玩家id列表(排除玩家)
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_all_player_id_list (PlayerId) ->
	lists:foldl(
		fun(Sid,List) ->
			case cluster:sid_call(Sid,?MODULE,get_local_player_id_list,[PlayerId]) of
				{badrpc,_} ->
					List;
				Return ->
					lists:append(Return,List)
			end
		end,
		[],
		cluster:tid_get_sid_list(?SERVER_T_LOGIC)
	).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 当前节点在线玩家id列表(不排除玩家)
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_local_player_id_list () ->
	get_local_player_id_list(0).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 当前节点在线玩家id列表(排除玩家)
%% @end
%%--------------------------------------------------------------------------------------------------------------	
get_local_player_id_list (PlayerId) ->
	lib_ets:select(online,[{#online{player_id = '$1',_ = '_'},[{'=/=','$1',PlayerId}],['$1']}]).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 发送数据给在线玩家
%% @end
%%--------------------------------------------------------------------------------------------------------------
send_to_player (PlayerId, OutBin) ->
    case get_online_player(PlayerId) of
		[] ->
			false;
		[Online] -> 
			Sid = Online #online.sid,
			Pid = Online #online.pid,
			cluster:sid_call(Sid,wss_handler,send,[Pid,OutBin])
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 发送数据给所有节点所有在线玩家(不排除玩家)
%% @end
%%--------------------------------------------------------------------------------------------------------------
send_to_all_players (OutBin) ->
	send_to_all_players_without_one(0,OutBin).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 发送数据给当前节点所有在线玩家(不排除玩家)
%% @end
%%--------------------------------------------------------------------------------------------------------------
send_to_local_players (OutBin) ->
	send_to_local_players_without_one(0,OutBin).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 发送数据给所有节点所有在线玩家(排除一个玩家)
%% @end
%%--------------------------------------------------------------------------------------------------------------
send_to_all_players_without_one (PlayerId, OutBin) ->
	SpidList = get_all_sid_pid_list(PlayerId),
	MergeList = lib_misc:key_merge_list(SpidList),
	lists:foreach(fun({Sid,PidList}) -> cluster:sid_cast(Sid,wss_handler,send,[PidList,OutBin]) end,MergeList).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 发送数据给当前节点所有在线玩家(排除一个玩家)
%% @end
%%--------------------------------------------------------------------------------------------------------------
send_to_local_players_without_one (PlayerId, OutBin) ->
	SpidList = get_local_sid_pid_list(PlayerId),
	MergeList = lib_misc:key_merge_list(SpidList),
	lists:foreach(fun({Sid,PidList}) -> cluster:sid_cast(Sid,wss_handler,send,[PidList,OutBin]) end,MergeList).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 同步调用玩家进程执行
%% @end
%%--------------------------------------------------------------------------------------------------------------
apply_to_player (PlayerId, M, F, A) ->
    case get_online_player(PlayerId) of
		[] -> 
			false;
        [Online] -> 
			Sid = Online #online.sid,
			Pid = Online #online.pid,
			case cluster:sid_call(Sid,wss_handler,apply,[Pid,M,F,A]) of
				apply_time_out -> 
					false;
				Return -> 
					Return
			end
    end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 异步调用玩家进程执行(无回调)
%% @end
%%--------------------------------------------------------------------------------------------------------------
async_apply_to_player (PlayerId, M, F, A) ->
	async_apply_to_player(PlayerId,M,F,A,null).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 异步调用玩家进程执行(有回调)
%% @end
%%--------------------------------------------------------------------------------------------------------------
async_apply_to_player (PlayerId, M, F, A, CallBack) when
    is_number(PlayerId),
    is_atom(M),
    is_atom(F),
    is_list(A)
->
    case get_online_player(PlayerId) of
        [] -> 
			false;
		[Online] ->
			Sid = Online #online.sid,
			Pid = Online #online.pid,
			cluster:sid_cast(Sid,wss_handler,async_apply,[Pid,M,F,A,CallBack])
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 同步调用所有玩家进程执行
%% @end
%%--------------------------------------------------------------------------------------------------------------
apply_to_all_players (M, F, A) ->
    SpidList = get_all_sid_pid_list(),
	MergeList = lib_misc:key_merge_list(SpidList),
	lists:foreach(fun({Sid,PidList}) -> catch cluster:sid_call(Sid,wss_handler,apply,[PidList,M,F,A]) end,MergeList).
    
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 异步调用所有玩家进程执行
%% @end
%%--------------------------------------------------------------------------------------------------------------
async_apply_to_all_players (M, F, A) ->
	SpidList = get_all_sid_pid_list(),
	MergeList = lib_misc:key_merge_list(SpidList),
	lists:foreach(fun({Sid,PidList}) -> cluster:sid_cast(Sid,wss_handler,async_apply,[PidList,M,F,A]) end,MergeList).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 将玩家踢下线
%% @end
%%--------------------------------------------------------------------------------------------------------------
kill_player (PlayerId) ->
    case get_online_player(PlayerId) of
		[] -> 
			false;
		[Online] -> 
			Sid = Online #online.sid,
			Pid = Online #online.pid,
			del_player(PlayerId),
			cluster:sid_cast(Sid,wss_handler,kill,[Pid])
    end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 将所有节点所有玩家踢下线
%% @end
%%--------------------------------------------------------------------------------------------------------------
kill_all_players () ->
	SpidList = get_all_sid_pid_list(),
	MergeList = lib_misc:key_merge_list(SpidList),
	lists:foreach(fun({Sid,PidList}) -> cluster:sid_cast(Sid,wss_handler,kill,[PidList]) end,MergeList),
	lists:foreach(fun(Sid) -> cluster:sid_cast(Sid,lib_ets,clean,[online]) end,cluster:tid_get_sid_list(?SERVER_T_LOGIC)).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 将当前节点所有玩家踢下线
%% @end
%%--------------------------------------------------------------------------------------------------------------
kill_local_players () ->
	SpidList = get_local_sid_pid_list(),
	MergeList = lib_misc:key_merge_list(SpidList),
	lists:foreach(fun({Sid,PidList}) -> cluster:sid_cast(Sid,wss_handler,kill,[PidList]) end,MergeList),
	catch lib_ets:clean(online).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 将当前节点所有玩家踢下线并在时间内等待(默认1800秒)
%% @end
%%--------------------------------------------------------------------------------------------------------------
wait_player_exit () ->
	wait_player_exit(?WAIT_TIMEOUT).
	
wait_player_exit (TimeOut) ->
	ranch:stop_listener(websocket),
	wait_player_exit(TimeOut,TimeOut).
	
wait_player_exit (TimeOut, Time) ->
    kill_local_players(),
	case catch ranch_server:count_connections(websocket) of
		0 ->
			?INFO("wait for player exit ... done~n"),
			Time;
		{'EXIT',_} ->
			?INFO("wait for player exit ... done~n"),
			Time;
		Count ->
			timer:sleep(1000),
			?INFO("wait for player exit ... ~p~n",[Count]),
			wait_player_exit(TimeOut,Time + 1)
	end.
    
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 所有节点在线玩家人数
%% @end
%%--------------------------------------------------------------------------------------------------------------
count_all_player () ->
	lists:foldl(
		fun(Sid,Sum) ->
			case cluster:sid_call(Sid,?MODULE,count_local_player,[]) of
				{badrpc,Reason} ->
					?WARNING("count player warnning: ~p~n",[Reason]),
					Sum;
				Count ->
					Sum + Count
			end
		end,
		0,
		cluster:tid_get_sid_list(?SERVER_T_LOGIC)
	).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 当前节点在线玩家人数
%% @end
%%--------------------------------------------------------------------------------------------------------------
count_local_player () ->
	lib_ets:size(online).