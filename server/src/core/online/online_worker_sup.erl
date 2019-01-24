%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 在线玩家工作督程
%%--------------------------------------------------------------------------------------------------------------
-module(online_worker_sup).

-behaviour(supervisor).

-export([start_child/1]).

-export([start_link/0,init/1]).

start_link () ->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

init ([])->
	Specs = [
		{
			?MODULE,
			{online_worker_srv,start_link,[]},
			transient,
			infinity,
			worker,
			[?MODULE]
		}
	],
    {ok,{{simple_one_for_one,0,1},Specs}}.
	
	
start_child (AAA) ->
	supervisor:start_child(?MODULE,[AAA]).