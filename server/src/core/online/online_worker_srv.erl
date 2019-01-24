%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 在线玩家工作进程
%%--------------------------------------------------------------------------------------------------------------
-module(online_worker_srv).

-behaviour(gen_server2).

-export([
	apply/4,			%%-----同步委托
	async_apply/4		%%-----异步委托
]).

-export([
	start_link/1,
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
%% 玩家id转进程名
%% @end
%%--------------------------------------------------------------------------------------------------------------
player_id_to_srv_name (PlayerId) ->
	list_to_atom(atom_to_list(?MODULE) ++ integer_to_list(PlayerId)).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 同步委托
%% @end
%%--------------------------------------------------------------------------------------------------------------
apply (PlayerId, M, F, A) ->
	SrvName = player_id_to_srv_name(PlayerId),
	gen_server2:call(SrvName,{apply,M,F,A},?CALL_TIMEOUT).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 异步委托
%% @end
%%--------------------------------------------------------------------------------------------------------------
async_apply (PlayerId, M, F, A) ->
	SrvName = player_id_to_srv_name(PlayerId),
	gen_server2:cast(SrvName,{apply,M,F,A}).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 调用
%% @end
%%--------------------------------------------------------------------------------------------------------------
handle_apply (M, F, A, State) ->
	case catch apply(M, F, A) of
		{'EXIT',Reason} ->
			?ERROR("online worker error:~n"
				"    player_id => ~p~n"
				"    module    => ~p~n"
				"    function  => ~p~n"
				"    args      => ~w~n"
				"    reason    => ~p~n", 
				[State,M,F,A,Reason]
			),
			apply_failed;
		Return ->
			Return
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% API
%% @end
%%--------------------------------------------------------------------------------------------------------------
init ([Online]) ->
	erlang:process_flag(trap_exit,true),
	{ok,Online}.
	
start_link (Online) ->
	PlayerId = Online #online.player_id,
	SrvName = player_id_to_srv_name(PlayerId),
	gen_server2:start_link({global,SrvName},?MODULE,[Online],[]).
	
handle_call({apply,M,F,A}, _From, State) ->
	Result = handle_apply(M,F,A,State),
    {reply,Result,State};
	
handle_call (_Msg, _From, State) ->
    {reply,ok,State}.
	
handle_cast({apply,M,F,A}, State) ->
	handle_apply(M,F,A,State),
    {noreply,State};
	
handle_cast (_Msg, State) ->
    {noreply,State}.
	
handle_info ({apply,M,F,A}, State) ->
	handle_apply(M,F,A,State),
    {noreply,State};
	
handle_info (_Msg, State) ->
    {noreply,State}.
	
terminate (_Reason, State) ->
    {noreply,State}.

code_change (_OldSvn, State, _Ext) ->
    {noreply,State}.