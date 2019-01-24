%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 数据库dump相关
%%--------------------------------------------------------------------------------------------------------------
-module(database_dump).

-export([
	do/0,					%%-----dump
	start/0,				%%-----开始dump(等待数据同步完成)
	start/1					%%-----开始dump(选择是否等待数据同步完成后开始dump)
]).

-include("game.hrl").

-define(DELETE_TIME,30).	%%-----删除N天以前的dump文件

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% dump
%% @end
%%--------------------------------------------------------------------------------------------------------------
do () -> 
	case lib_misc:get_env_atom(mysql_dump,true) of
		true ->
			start(true);
		false ->
			ok
	end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 开始dump(等待数据同步完成)
%% @end
%%--------------------------------------------------------------------------------------------------------------
start () -> start(true).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 开始dump(选择是否等待数据同步完成后开始dump)
%% @end
%%--------------------------------------------------------------------------------------------------------------
start (IsWait) ->
	if IsWait -> database_sync:wait_for_data_sync(); true -> ok end,
	DumpTime		 = lib_misc:timestamp(),
	DeleteTime		 = DumpTime - ?DELETE_TIME * 86400,
	{DumpDate,_}	 = lib_misc:timestamp_to_datetime(DumpTime),
	{DeleteDate,_}	 = lib_misc:timestamp_to_datetime(DeleteTime),
	DumpFile		 = filename(DumpDate),
	DeleteFile		 = filename(DeleteDate),
	DirName			 = lib_misc:get_env_str(data_dir,"./data"),
	DumpPath		 = filename:join([DirName,"dump",DumpFile]),
	DeletePath		 = filename:join([DirName,"dump",DeleteFile]),
	catch file:delete(DeletePath),
	case filelib:is_file(DumpPath) of
		true -> ok;
		false -> ok = filelib:ensure_dir(DumpPath)
	end,
	?INFOS("===================================================================~n"),
	?INFOS("database dump start~n"),
	StartTime = lib_misc:timestamp(),
	{ok,File} = file:open(DumpPath,[write,raw]),
	ok = file:write(File,<<"/*!40101 SET NAMES utf8 */;\n">>),
	ok = file:write(File,<<"/*!40101 SET SQL_MODE=''*/;\n">>),
	ok = file:write(File,<<"/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;\n">>),
	ok = file:write(File,<<"/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;\n">>),
	ok = file:write(File,<<"/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;\n">>),
	ok = file:write(File,<<"/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;\n\n">>),
	?INFOS("===================================================================~n"),
	lists:foreach(fun(Table) -> dump(File,Table) end,database_lib:table_list()),
	ok = file:close(File),
	EndTime = lib_misc:timestamp(),
	?INFOS("database dump finish~n"),
	?INFOS("database time ~s~n",[lib_misc:seconds_to_time(EndTime - StartTime)]),
	?INFOS("===================================================================~n"),
	ok.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% dump表
%% @end
%%--------------------------------------------------------------------------------------------------------------
dump (File, Table) ->
	case database_lib:is_ingore(Table) of
		true ->
			ok;
		false ->
			?INFOS("table dump : ~p start~n",[Table]),
			StartTime = lib_misc:timestamp(),
			Sql = "DELETE FROM " ++ database_lib:table_str(Table) ++ ";\n",
			ok = file:write(File,list_to_binary(Sql)),
			case database_lib:has_spilit(Table) of
				true ->
					Size = database:count(Table),
					lists:foldl(
						fun(I,{S1,N1,L1}) ->
							ets:foldl(
								fun(Record,{S,N,L}) -> 
									dump_row(File,Table,{Record,Size,S,N,L})
								end,
								{S1,N1,L1},
								database:ets(Table,I)
							)
						end,
						{0,0,[]},
						lists:seq(0,database_lib:spilit_count() - 1)
					);
				false ->
					Etable = database:ets(Table),
					Size = ets:info(Etable,size),
					ets:foldl(
						fun(Record,{S,N,L}) ->
							dump_row(File,Table,{Record,Size,S,N,L})
						end,
						{0,0,[]},
						Etable
					)
			end,
			EndTime = lib_misc:timestamp(),
			?INFOS("table dump : ~p finish~n",[Table]),
			?INFOS("table time : ~s~n",[lib_misc:seconds_to_time(EndTime - StartTime)]),
			?INFOS("===================================================================~n"),
			ok
	end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% dump单条数据
%% @end
%%--------------------------------------------------------------------------------------------------------------
dump_row (File, Table, {Record, Size, S, N, L}) ->
	SpilitCount = database_lib:spilit_count(),
	ColPosList = lists:seq(1,database_lib:column_count(Table)),
	if
		S + 1 == Size orelse N == SpilitCount ->
			L2 = [value_sql(lists:reverse(ColPosList),Record,[");\n\n"])|L],
			ok = file:write(File,[insert_sql(Table,ColPosList)|lists:reverse(L2)]),
			{S + 1,0,[]};
		true -> 
			L2 = [value_sql(lists:reverse(ColPosList),Record,["),\n"])|L],
			{S + 1,N + 1,L2}
	end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 转换insert语句
%% @end
%%--------------------------------------------------------------------------------------------------------------
insert_sql (Table, ColPosList) ->
	insert_sql(Table,lists:reverse(ColPosList),1,length(ColPosList),[") VALUES \n"]).

insert_sql (Table, [], _, _, Sql) ->
	list_to_binary(["INSERT IGNORE INTO ",database_lib:table_str(Table),"("|Sql]);
	
insert_sql (Table, [Pos|L], N, Max, Sql) ->
	if
		N =< 1 ->
			insert_sql(Table,L,N + 1,Max,[database_lib:column_str(Table,Pos)|Sql]);
		true ->
			insert_sql(Table,L,N + 1,Max,[database_lib:column_str(Table,Pos),","|Sql])
	end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 转换value_sql语句
%% @end
%%--------------------------------------------------------------------------------------------------------------
value_sql (L, Record, Sql) ->
	value_sql(L,Record,1,length(L),Sql).
	
value_sql ([], _, _, _, Sql) ->
	list_to_binary(["("|Sql]);
	
value_sql ([Pos|L], Record, N, Max, Sql) ->
	Value = database:get_element(Pos,Record),
	ValueBin = database:val_to_bin(Value),
	if
		N =< 1 ->
			value_sql(L,Record,N + 1,Max,[ValueBin|Sql]);
		true ->
			value_sql(L,Record,N + 1,Max,[ValueBin,","|Sql])
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 文件名
%% @end
%%--------------------------------------------------------------------------------------------------------------
filename ({Y,M,D}) ->
	"database" ++ "-" ++ integer_to_list(Y) ++ "-" ++ integer_to_list(M) ++ "-" ++ integer_to_list(D) ++ ".sql".