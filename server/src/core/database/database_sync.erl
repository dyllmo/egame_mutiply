%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 数据库数据同步
%%--------------------------------------------------------------------------------------------------------------
-module(database_sync).

-behavior(gen_server2).

-export([
	do/1,							%%-----执行同步
	count_work/0,					%%-----队列的消息数量
	wait_for_data_sync/0,			%%-----等待数据同步(默认1800秒)
	wait_for_data_sync/1			%%-----等待数据同步
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
%% 执行同步
%% @end
%%--------------------------------------------------------------------------------------------------------------
do (Sql) ->
	gen_server2:call(?MODULE,{do,Sql},?CALL_TIMEOUT).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 队列的消息数量
%% @end
%%--------------------------------------------------------------------------------------------------------------
count_work () ->
	{message_queue_len,Length} = process_info(whereis(?MODULE),message_queue_len),
    Length.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 等待数据同步
%% @end
%%--------------------------------------------------------------------------------------------------------------
wait_for_data_sync () ->
	wait_for_data_sync(?WAIT_TIMEOUT).
	
wait_for_data_sync (TimeOut) ->
	wait_for_data_sync(TimeOut,0).
	
wait_for_data_sync (TimeOut, TimeOut) ->
    case io:get_chars("wait timeout, are you continue? [Y/n] : ", 1) of
        "n" ->
			?INFO("wait for data sync ... timeout~n"),
	        timeout;
        _ ->
	        wait_for_data_sync(TimeOut,0)
    end;
	
wait_for_data_sync (TimeOut, Time) ->
	case count_work() of
		0 -> 
			?INFO("wait for data sync ... done~n");
		Count -> 
			?INFO("wait for data sync ... ~p~n",[Count]),
			timer:sleep(1000),
			wait_for_data_sync(TimeOut,Time + 1)
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% API
%% @end
%%--------------------------------------------------------------------------------------------------------------
start_link () ->
    gen_server2:start_link({local,?MODULE},?MODULE,[],[]).

init ([]) -> {ok,null,hibernate,?BACKOFF}.

handle_call ({do,Sql}, _From, State) ->
	catch lib_mysql:fetch(Sql),
    {reply,ok,State,hibernate};
	
handle_call (Msg, _From, State) ->
    {reply,{ok,Msg},State,hibernate}.
	
handle_cast (_Msg, State) ->
    {noreply,State,hibernate}.
	
handle_info ({do,Sql}, State) ->
	catch lib_mysql:fetch(Sql),
    {noreply,State,hibernate};
	
handle_info (_Msg, State) ->
    {noreply,State,hibernate}.

terminate (_Reason, State) ->
    {noreply,State,hibernate}.

code_change (_OldSvn, State, _Ext) ->
    {noreply,State}.