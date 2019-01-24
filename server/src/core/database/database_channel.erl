%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 数据库数据处理通道
%%--------------------------------------------------------------------------------------------------------------
-module(database_channel).

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
	
start_link () ->
    gen_server2:start_link({local,?MODULE},?MODULE,[],[]).

init ([]) ->
	database_init:start(),
	{ok,null,hibernate,?BACKOFF}.

handle_call (Msg, _From, State) ->
    {reply,{ok,Msg},State,hibernate}.
	
handle_cast (_Msg, State) ->
    {noreply,State,hibernate}.
	
handle_info ({do,ActionList}, State) ->
	case catch database_sql:do(ActionList) of
		[] -> 
			ok;
		SqlList when is_list(SqlList) ->
			database_sync ! {do,SqlList},
			database_log ! {do,SqlList};
		{'EXIT',Error,Reason} -> 
			?ERROR(
				"database_channel_error:~n"
				"    action_list => ~p~n"
				"    error       => ~p~n"
				"    reason      => ~p~n",
				[ActionList,Error,Reason]
			)
	end,
    {noreply,State,hibernate};
	
handle_info (_Msg, State) ->
    {noreply,State,hibernate}.

terminate (_Reason, State) ->
    {noreply,State}.

code_change (_OldSvn, State, _Ext) ->
    {noreply,State}.