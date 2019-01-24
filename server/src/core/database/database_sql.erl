%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : ets转换mysql语句
%%--------------------------------------------------------------------------------------------------------------
-module(database_sql).

-export([do/1]).		%%-----转换语句

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 转换语句
%% @end
%%--------------------------------------------------------------------------------------------------------------
do (Actions) -> 
	do(Actions,[]).

do ([], SqlList) -> SqlList;
do ([Action|Tail], SqlList) ->
	case to_sql(Action) of
		none -> do(Tail,SqlList);
		Sql  -> do(Tail,[Sql|SqlList])
	end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 转换insert语句
%% @end
%%--------------------------------------------------------------------------------------------------------------
to_sql ({Table, insert, Record}) ->
	ColCount = database_lib:column_count(Table),
	Sql = to_column_sql(lists:seq(1,ColCount),Record,true),
	list_to_binary(["INSERT IGNORE INTO ",database_lib:table_str(Table)," SET ",Sql]);

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 转换delete语句
%% @end
%%--------------------------------------------------------------------------------------------------------------
to_sql ({Table, delete, Record}) ->
	PKPosList = database_lib:pk_column_pos_list(Table),
	Sql = to_column_sql(PKPosList,Record,true),
	list_to_binary(["DELETE FROM ",database_lib:table_str(Table)," WHERE ",Sql]);
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 转换update语句
%% @end
%%--------------------------------------------------------------------------------------------------------------
to_sql ({_, update, _, []}) -> none;
to_sql ({Table, update, Record, Changes}) ->
	PKColPosList = database_lib:pk_column_pos_list(Table),
	Sql1 = to_column_sql(Changes,Record,false),
	Sql2 = to_column_sql(PKColPosList,Record,true),
	list_to_binary(["UPDATE ",database_lib:table_str(Table)," SET ",Sql1," WHERE ", Sql2]);

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 转换sql语句
%% @end
%%--------------------------------------------------------------------------------------------------------------
to_sql ({_, sql, Sql}) ->
	list_to_binary(Sql).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 转换字段语句
%% @end
%%--------------------------------------------------------------------------------------------------------------
to_column_sql (ColPosList, Record, IsEndLine) ->
	Table = element(1,Record),
	to_column_sql(ColPosList,Table,Record,IsEndLine,[]).

to_column_sql ([], _, _, IsEndLine, Sql) -> 
	Sql2 = case IsEndLine of
		true -> [";\n"|Sql];
		false -> Sql
	end,
	lists:reverse(Sql2);
to_column_sql ([Pos], Table,Record, IsEndLine, Sql) ->
	Value = database:get_element(Pos,Record),
	ValueBin = database:val_to_bin(Value),
	to_column_sql([],Table,Record,IsEndLine,[ValueBin," = ",database_lib:column_str(Table,Pos)|Sql]);
to_column_sql ([Pos|L], Table, Record, IsEndLine, Sql) ->
	Value = database:get_element(Pos,Record),
	ValueBin = database:val_to_bin(Value),
	to_column_sql(L,Table,Record,IsEndLine,[",",ValueBin," = ",database_lib:column_str(Table,Pos)|Sql]).