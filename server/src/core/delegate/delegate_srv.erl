%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 委托进程
%%--------------------------------------------------------------------------------------------------------------
-module(delegate_srv).

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
%% 同步委托
%% @end
%%--------------------------------------------------------------------------------------------------------------
apply (SrvName, M, F, A) ->
	gen_server2:call(SrvName,{apply,M,F,A},?CALL_TIMEOUT).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 异步委托
%% @end
%%--------------------------------------------------------------------------------------------------------------
async_apply (SrvName, M, F, A) ->
	gen_server2:cast(SrvName,{apply,M,F,A}).
	
handle_apply (M, F, A, Pid) ->
	case catch apply(M, F, A) of
		{'EXIT',Reason} ->
			?ERROR(
				"delegate_apply_error:~n"
				"    pid      => ~p~n"
				"    module   => ~p~n"
				"    function => ~p~n"
				"    args     => ~w~n"
				"    reason   => ~p~n", 
				[Pid,M,F,A,Reason]
			),
			apply_failed;
		Return ->
			Return
	end.
	
init ([SrvName]) ->
	{ok,SrvName,hibernate,?BACKOFF}.
	
start_link (SrvName) ->
	gen_server2:start_link({local,SrvName},?MODULE,[SrvName],[]).
	
handle_call({apply,M,F,A}, _From, State) ->
	Result = handle_apply(M,F,A,State),
    {reply,Result,State,hibernate};
	
handle_call (_Msg, _From, State) ->
    {reply,ok,State,hibernate}.
	
handle_cast({apply,M,F,A}, State) ->
	handle_apply(M,F,A,State),
    {noreply,State,hibernate};
	
handle_cast (_Msg, State) ->
    {noreply,State,hibernate}.
	
handle_info ({apply,M,F,A}, State) ->
	handle_apply(M,F,A,State),
    {noreply,State,hibernate};
	
handle_info (_Msg, State) ->
    {noreply,State,hibernate}.
	
terminate (_Reason, State) ->
    {noreply,State}.

code_change (_OldSvn, State, _Ext) ->
    {noreply,State}.