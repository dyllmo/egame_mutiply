%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 用户自定义ets管理模块
%%--------------------------------------------------------------------------------------------------------------
-module(mod_uets).

-export([
	common_tables/0,			%%-----所有服务器表
	server_tables/1,			%%-----专属服务器表
	common_init/0,				%%-----所有服务器初始化
	server_init/1				%%-----专属服务器初始化
]).

-include("game.hrl").
-include("gen/database.hrl").

%%-----所有服务器表
-define(COMMON_TABLES,[]).

%%-----网关服务器表
-define(GATE_TABLES,[]).

%%-----登陆服务器表
-define(LOGIN_TABLES,[]).

%%-----日志服务器表
-define(LOGGER_TABLES,[]).

%%-----游戏服务器表
-define(LOGIC_TABLES,[
	{online,#online.player_id},
	{player_1,#player_1.uid},
	{player_2,#player_2.nickname}
]).

%%-------------------------------------------------------------------------------------------------------------
%% @doc
%% 所有服务器表
%% @end
%%-------------------------------------------------------------------------------------------------------------
common_tables () -> 
	?COMMON_TABLES.
	
%%-------------------------------------------------------------------------------------------------------------
%% @doc
%% 专属服务器表
%% @end
%%-------------------------------------------------------------------------------------------------------------
%%-----网关服务器
server_tables (?SERVER_T_GATE) -> 
	?GATE_TABLES;
	
%%-----登陆服务器
server_tables (?SERVER_T_LOGIN) -> 
	?LOGIN_TABLES;
	
%%-----日志服务器
server_tables (?SERVER_T_LOGGER) -> 
	?LOGGER_TABLES;
	
%%-----游戏服务器
server_tables (?SERVER_T_LOGIC) -> 
	?LOGIC_TABLES;
	
%%-----其他服务器
server_tables (_) -> 
	[].
	
%%-------------------------------------------------------------------------------------------------------------
%% @doc
%% 所有服务器初始化
%% @end
%%-------------------------------------------------------------------------------------------------------------
common_init () ->
	cluster:init(),
	ok.

%%-------------------------------------------------------------------------------------------------------------
%% @doc
%% 专属服务器初始化
%% @end
%%-------------------------------------------------------------------------------------------------------------
%%-----网关服务器
server_init (?SERVER_T_GATE) ->
	ok;
	
%%-----登陆服务器
server_init (?SERVER_T_LOGIN) ->
	ok;
	
%%-----日志服务器
server_init (?SERVER_T_LOGGER) ->
	ok;
	
%%-----游戏服务器
server_init (?SERVER_T_LOGIC) ->
	% player_init(),
	ok;
	
%%-----其他服务器
server_init (_) ->
	ok.
	
%%-------------------------------------------------------------------------------------------------------------
%% @doc
%% 玩家对应关系初始化
%% @end
%%-------------------------------------------------------------------------------------------------------------
% player_init () ->
	% lists:foreach(
		% fun(Player) ->
			% PlayerId = Player #player.id,
			% Uid		 = Player #player.uid,
			% NickName = Player #player.nickname,
			% mod_player:set_player_1(Uid,PlayerId),
			% mod_player:set_player_2(NickName,PlayerId)
		% end,
		% database:tab2list(player)
	% ).