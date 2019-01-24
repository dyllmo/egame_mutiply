%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 委托督程
%%--------------------------------------------------------------------------------------------------------------
-module(delegate_sup).

-behaviour(supervisor).

-export([start_link/0,init/1]).

start_link () ->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

init (_Args)->
	Specs = lists:foldl(
		fun(Id,L) ->
			SrvName = delegate:srv_name(Id),
			[
				{
					SrvName,
					{delegate_srv,start_link,[SrvName]},
					transient,
					infinity,
					worker,
					[?MODULE]
				}|L
			]
		end,
		[],
		lists:seq(0,delegate:srv_count() - 1)
	),
    {ok,{{one_for_one,10,10},Specs}}.