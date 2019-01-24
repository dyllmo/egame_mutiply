%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 消息路由
%%--------------------------------------------------------------------------------------------------------------
-module(router).

-export([
	print/1,		%%-----打印请求
	set_print/0,	%%-----设置是否打印(全开)
	set_print/1,	%%-----设置是否打印(全部)
	set_print/2,	%%-----设置是否打印(模块)
	set_print/3,	%%-----设置是否打印(模块,接口)
	set_print/4,	%%-----设置是否打印(模块,接口,玩家)
	is_print/0,		%%-----是否打印(全部)
	is_print/1,		%%-----是否打印(模块)
	is_print/2,		%%-----是否打印(模块,接口)
	is_print/3,		%%-----是否打印(模块,接口,玩家)
	mfargs/1,		%%-----{模块,函数,参数}
	request/2,		%%-----接口请求
	response/2,		%%-----接口转码(M,F)
	response/3		%%-----接口转码(M,F,A)
]).

-include("game.hrl").
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 打印请求
%% @end
%%--------------------------------------------------------------------------------------------------------------
print (Request) ->
	{M,F,Args} = router_lib:mfargs(Request),
	PlayerId = lib_misc:get_player_id(),
	case catch is_print(M,F,PlayerId) of
		true ->
			?INFO(
				"router_request:~n"
				"    player_id => ~p~n"
				"    module    => ~p~n"
				"    function  => ~p~n"
				"    args      => ~w~n"
				"    time      => ~s~n",
				[PlayerId,M,F,Args,lib_misc:format_time()]
			);
		false ->
			ok
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 确保表 
%% @end
%%--------------------------------------------------------------------------------------------------------------
ensure_table () ->
	case lib_ets:info(?MODULE) of
		undefined ->
			lib_ets:create(?MODULE,set,1,public);
		_ ->
			ok
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 设置是否打印
%% @end
%%--------------------------------------------------------------------------------------------------------------
set_print () ->
	set_print(true).
	
set_print (IsPrint) ->
	lib_misc:set_env(is_printf_request,IsPrint).
	
set_print (Module, IsPrint) ->
	set_print(Module,undefined,IsPrint).
	
set_print (Module, Action, IsPrint) ->
	set_print(Module,Action,lib_misc:get_player_id(),IsPrint).
	
set_print (Module, Action, PlayerId, IsPrint) ->
	ensure_table(),
	lib_ets:insert(?MODULE,{{Module,Action,PlayerId},IsPrint},replace).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 是否打印 
%% @end
%%--------------------------------------------------------------------------------------------------------------
is_print () ->
	is_print(undefined).
	
is_print (Module) ->
	is_print(Module,undefined).
	
is_print (Module, Action) ->
	is_print(Module,Action,lib_misc:get_player_id()).
	
is_print (Module, Action, PlayerId) ->	
	IsAll = lib_misc:get_env_atom(is_printf_request,false),
	if IsAll -> throw(IsAll); true -> false end,
	ensure_table(),
	IsModule = lib_ets:get(?MODULE,{Module,undefined,0},2,false),
	if IsModule -> throw(IsModule); true -> false end,
	IsApi = lib_ets:get(?MODULE,{Module,Action,0},2,false),
	if IsApi -> throw(IsApi); true -> false end,
	lib_ets:get(?MODULE,{Module,Action,PlayerId},2,false).
			
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% {模块,函数,参数}
%% @end
%%--------------------------------------------------------------------------------------------------------------
mfargs (Request) ->
	router_lib:mfargs(Request).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 接口请求
%% @end
%%--------------------------------------------------------------------------------------------------------------
request (Request, State) ->
	Time = prof:time(),
	print(Request),
	{M,F,OutBin,NewState} = router_lib:request(Request,State),
	prof:set_info(M,F,Time),
	{ok,OutBin,NewState}.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 接口转码(M,F)
%% @end
%%--------------------------------------------------------------------------------------------------------------
response (Module, Action) ->
	router_encode:api(Module,Action).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 接口转码(M,F,A)
%% @end
%%--------------------------------------------------------------------------------------------------------------	
response (Module, Action, Arg) ->
	router_encode:api(Module,Action,Arg).