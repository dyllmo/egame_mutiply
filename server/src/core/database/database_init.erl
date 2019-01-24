%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 数据库初始化
%%--------------------------------------------------------------------------------------------------------------
-module(database_init).

-export([
	start/0,			%%-----开始初始化(库)
	start/1				%%-----开始初始化(表)
]).		

-include("game.hrl").

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 开始初始化(库)
%% @end
%%--------------------------------------------------------------------------------------------------------------
start () ->
	lib_mysql:fetch([<<"SET FOREIGN_KEY_CHECKS = 0;">>]),
	lib_ets:new(auto_increment,[public,set,named_table]),
	timer:sleep(100),
	?INFOS("===================================================================~n"),
	?INFOS("database init start~n"),
	?INFOS("===================================================================~n"),
	StartTime = lib_misc:timestamp(),
	lists:foreach(fun(Table) -> start(Table) end,database_lib:table_list()),
	EndTime = lib_misc:timestamp(),
	?INFOS("database init finish~n"),
	?INFOS("database time ~s~n",[lib_misc:seconds_to_time(EndTime - StartTime)]),
	?INFOS("===================================================================~n"),
	ok.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 开始初始化(表)
%% @end
%%--------------------------------------------------------------------------------------------------------------
start (Table) ->
	?INFOS("table init : ~p start~n",[Table]),
	StartTime = lib_misc:timestamp(),
	MinIncId = case Table of
		player ->
			?CID * ?PLAYER_SRV_ID_INDEX;
		_ ->
			0
	end,
	true = case database_lib:has_increment(Table) of
		true ->
			IncColPos = database_lib:increment_column_pos(Table),
			IncColStr = database_lib:column_str(Table,IncColPos),
			Sql1 = "SELECT IFNULL(MAX(" ++ IncColStr ++ ")," ++ integer_to_list(MinIncId) ++ ") AS `max_id` FROM " ++ database_lib:table_str(Table) ++ ";",
			ResultId1 = lib_mysql:fetch([list_to_binary(Sql1)]),
			[Result1] = lib_mysql:get_rows(ResultId1),
			{max_id,MaxId} = lists:keyfind(max_id,1,Result1),
			lib_ets:insert(auto_increment,{{Table,id},MaxId});
		false ->
			lib_ets:insert(auto_increment,{{Table,id},MinIncId})
	end,
	case database_lib:is_ingore(Table) of
		true ->
			ok;
		false ->
			case database_lib:has_spilit(Table) of
				true ->
					SpilitCount = database_lib:spilit_count(),
					lists:foreach(fun(N) -> database:init_ets(Table,N) end,lists:seq(0,SpilitCount - 1));
				false ->
					database:init_ets(Table)
			end,
			Sql2 = "SELECT COUNT(1) AS `count` FROM " ++ database_lib:table_str(Table) ++ ";",
			ResultId2 = lib_mysql:fetch([list_to_binary(Sql2)]),
			[Result2] = lib_mysql:get_rows(ResultId2), 
			{count,Count} = lists:keyfind(count,1,Result2),
			lists:foreach(
				fun(Page) ->
					Sql3 = "SELECT * FROM " ++ database_lib:table_str(Table) ++ " LIMIT " ++  integer_to_list((Page - 1) * 100000) ++ ",100000;",
					ResultId3 = lib_mysql:fetch([list_to_binary(Sql3)]),
					Rows = lib_mysql:get_rows(ResultId3),
					lists:foreach(
						fun(Row) -> 
							{Last,L1} = lists:foldl(fun({_,CV},{P,L}) -> {P + 1,[{P + 1,CV}|L]} end,{2,[]},Row),
							L2 = lists:map(fun(N) -> {_,V} = lists:nth(length(L1) - N + 1,L1),V end,database_lib:pk_column_pos_list(Table)),
							Record = erlang:make_tuple(length(L1) + 3,null,[{1,Table},{2,list_to_tuple(L2)}|L1] ++ [{Last + 1,0}]),
							true = case database_lib:has_spilit(Table) of
								true ->
									SpilitPos = database_lib:spilit_pos(Table),
									SpilitVal = database:get_element(SpilitPos,Record),
									SpilitId = database:get_spilit_id(SpilitVal),
									ETable = database:ets(Table,SpilitId),
									lib_ets:insert(ETable,Record);
								false ->
									ETable = database:ets(Table),
									lib_ets:insert(ETable,Record)
							end
						end,
						Rows
					)
				end,
				lists:seq(1,lib_misc:ceil(Count / 100000))
			)
	end,
	EndTime = lib_misc:timestamp(),
	?INFOS("table init : ~p finish~n",[Table]),
	?INFOS("table time : ~s~n",[lib_misc:seconds_to_time(EndTime - StartTime)]),
	?INFOS("===================================================================~n"),
	ok.