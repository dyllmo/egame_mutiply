%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 主入口管理,启动,关闭回调
%%--------------------------------------------------------------------------------------------------------------
-module(main).

-export([
	start/1,		%%-----启动
	stop/1			%%-----停止
]).

-include("game.hrl").

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 启动
%% @end
%%--------------------------------------------------------------------------------------------------------------
start (Tid) ->
	common_start(),
	server_start(Tid),
	ok.
	 
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 所有服务器启动
%% @end
%%--------------------------------------------------------------------------------------------------------------
common_start () ->
	ok.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 专属服务器启动
%% @end
%%--------------------------------------------------------------------------------------------------------------
%%-----网关服务器
server_start (?SERVER_T_GATE) ->
	uets:start(?SERVER_T_GATE),
	game:start_child({delegate,{delegate,start,[]},supervisor}),
	game:start_child({ranch,{ranch_sup,start_link,[]},supervisor}),
	wss_init:start(),
	ok;
	
%%-----登陆服务器
server_start (?SERVER_T_LOGIN) ->
	game:start_child({database,{database,start,[]},supervisor}),
	uets:start(?SERVER_T_LOGIN),
	game:start_child({ranch,{ranch_sup,start_link,[]},supervisor}),
	https_init:start(),
	ok;
	
%%-----日志服务器
server_start (?SERVER_T_LOGGER) ->
	uets:start(?SERVER_T_LOGGER),
	ok;
	
%%-----游戏服务器
server_start (?SERVER_T_LOGIC) ->
	game:start_child({delegate,{delegate,start,[]},supervisor}),
	game:start_child({database,{database,start,[]},supervisor}),
	game:start_child({online_worker,{online_worker_sup,start_link,[]},supervisor}),
	uets:start(?SERVER_T_LOGIC),
	ok;
	 
server_start (_) -> 
	ok.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 停止
%% @end
%%--------------------------------------------------------------------------------------------------------------
%%-----网关服务器
stop (?SERVER_T_GATE) ->
	%%-----等待玩家退出
	online:wait_player_exit(),
	ok;
	
%%-----登陆服务器
stop (?SERVER_T_LOGIN) ->
	%%-----dump数据
	database_dump:do(),
	ok;
	
%%-----日志服务器
stop (?SERVER_T_LOGGER) ->
	ok;

%%-----游戏服务器
stop (?SERVER_T_LOGIC) ->
	%%-----dump数据
	database_dump:do(),
	ok;
	
stop (_) ->
	ok.