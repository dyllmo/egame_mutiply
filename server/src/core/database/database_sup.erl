%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 数据库总督程
%%--------------------------------------------------------------------------------------------------------------
-module(database_sup).

-behaviour(supervisor).

-export([
	start_link/0,
	init/1
]).

start_link () ->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

init ([])->
	%%-----Mysql进程
	MysqlSpec = {
		mysql,
		{mysql,start_link,[]},
		transient,
		infinity,
		worker,
		[mysql]
	},
	%%-----同步进程
	SyncSpec = {
		database_sync,
		{database_sync,start_link,[]},
		transient,
		infinity,
		worker,
		[database_sync]
	},
	%%-----日志进程
	LogSepc = {
		database_log,
		{database_log,start_link,[]},
		transient,
		infinity,
		worker,
		[database_log]
	},
	%%-----通道进程
	ChannelSpec = {
		database_channel,
		{database_channel,start_link,[]},
		transient,
		infinity,
		worker,
		[database_channel]
	},
    {ok,{{one_for_one,10,10},[MysqlSpec,SyncSpec,LogSepc,ChannelSpec]}}.