%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 消息路由,本文件项目工具生成,无需修改
%%--------------------------------------------------------------------------------------------------------------
-module(router_lib).

-export([
	mfid/2,				%%-----模块接口id
	mfname/1,			%%-----模块接口名
	mfargs/1,			%%-----{模块,函数,参数}
	request/2			%%-----请求
]).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 模块接口id
%% @end
%%--------------------------------------------------------------------------------------------------------------
mfid (player, check_token) -> 0;
mfid (player, login) -> 1;
mfid (_, _)-> exit(nonexistent_mfid).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 模块接口名
%% @end
%%--------------------------------------------------------------------------------------------------------------
mfname (0) -> {player,check_token};
mfname (1) -> {player,login};
mfname (_)-> exit(nonexistent_mfname).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% {模块,函数,参数}
%% @end
%%--------------------------------------------------------------------------------------------------------------
mfargs (<<Api:16/unsigned,Args0/binary>>) ->
	{M,F} = mfname(Api),
	Args = router_decode:api(M,F,Args0),
	{M,F,Args}.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 请求
%% @end
%%--------------------------------------------------------------------------------------------------------------
request (<<0:16/unsigned,_Args0/binary>>, State) ->
	_Args1 = router_decode:api(player,check_token,_Args0),
	{_Result,NewState} = api_player:check_token(_Args1,State),
	OutBin = router_encode:api(player,check_token,_Result),
	{player,check_token,OutBin,NewState};

request (<<1:16/unsigned,_Args0/binary>>, State) ->
	_Args1 = router_decode:api(player,login,_Args0),
	{_Result,NewState} = api_player:login(_Args1,State),
	OutBin = router_encode:api(player,login,_Result),
	{player,login,OutBin,NewState};

request (_, _)->
	exit(nonexistent_request).
