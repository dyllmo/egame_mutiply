%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : mysql库
%%--------------------------------------------------------------------------------------------------------------
-module(lib_mysql).

-export([
	database/0,						%%-----数据库(默认gamedb)
    fetch/1,						%%-----执行sql语句(默认数据库)
    fetch/2,						%%-----执行sql语句
    insert/1,						%%-----sql插入语句(默认数据库)
    insert/2,						%%-----sql插入语句
    get_rows/1,						%%-----查询语句
    get_rows/3,						%%-----查询语句
    get_field/2,					%%-----获取字段
    update_row/3,					%%-----更新语句
    condition_string/1,				%%-----条件字符串
    mysql_update_string/1,			%%-----更新字符串
	mysql_binary_to_list/1			%%-----二进制转换字符
]).

-include("game.hrl").
-include("mysql.hrl").

-define(FETCH_TIEMOUT,10).			%%-----执行超时时间(秒)
-define(RECONNECT_MYSQL,10).		%%-----重连等待时间(秒)
-define(DATABASE,database()).		%%-----数据库
		
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 数据库(默认gamedb)
%% @end
%%--------------------------------------------------------------------------------------------------------------
database () ->
	lib_misc:get_env_atom(mysql_database,gamedb).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 执行sql语句
%% @end
%%--------------------------------------------------------------------------------------------------------------
fetch (SqlList) ->
	fetch(?DATABASE,SqlList).
	
fetch (PoolId, SqlList) ->
	case catch mysql:fetch(PoolId,[SqlList],?FETCH_TIEMOUT * 1000) of
		{data,Result}->
			Result;
		{updated,Result} ->
			Result;
		{error,Result} ->
			?ERROR(
				"mysql fetch error:~n"
				"    sql_list => ~w~n"
				"    result   => ~p~n",
				[SqlList,Result]
			),
			timer:sleep(?RECONNECT_MYSQL * 1000),
			fetch(PoolId,SqlList)
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% sql插入语句
%% @end
%%--------------------------------------------------------------------------------------------------------------
insert (Sql) ->
	insert(?DATABASE,Sql).
	
insert (PoolId, Sql) ->
    case mysql:fetch(PoolId,Sql) of
        {updated,Result} ->
            Result #mysql_result.insert_id;
        _ ->
            0
    end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 查询语句
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_rows (Table, Condition, Fields) ->
	get_rows(?DATABASE,Table,Condition,Fields).
	
get_rows (PoolId, Table, Condition, Fields) ->
    BinFields = list_to_binary(Fields),
    BinTableName = list_to_binary(Table),
    BinWhere = list_to_binary(Condition),
    Sql = <<"SELECT ",BinFields/binary," FROM ",BinTableName/binary," WHERE ",BinWhere/binary >>,
    case mysql:fetch(PoolId,[Sql]) of
        {data,Result} ->
            get_rows(Result);
        _ ->
            false
    end.

get_rows (#mysql_result{fieldinfo = FieldInfo, rows=AllRows}) ->
    case AllRows of
        [] ->
            [];
        _ ->
            [readble_row(Row,FieldInfo) || Row <- AllRows]
    end.

readble_row (Row, FieldInfo) ->
     SeqList = lists:seq(1,length(Row)),
     [readble_column(Seq,Row,FieldInfo)|| Seq <- SeqList].

readble_column (N, Row, FieldInfo) ->
    {_,FieldName,_,_} = lists:nth(N,FieldInfo),
    Value = lists:nth(N,Row),
	{binary_to_atom(FieldName,utf8),Value}.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 获取字段
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_field (ReadableRow, Field) ->
    {_,Value} = lists:keyfind(Field, 1, ReadableRow),
    Value.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 更新语句
%% @end
%%--------------------------------------------------------------------------------------------------------------
update_row (Table, Key, Values) ->
	update_row(?DATABASE,Table,Key,Values).
	
update_row(PoolId, Table, Key, Values) ->
    BinTableName    = list_to_binary(Table),
    BinUpdateFields = list_to_binary(mysql_update_string(Values)),
    BinWhere        = list_to_binary(Key),
    Sql = << "UPDATE ",BinTableName/binary," SET ",BinUpdateFields/binary," WHERE ", BinWhere/binary >>,
    case mysql:fetch(PoolId,[Sql]) of
        {updated,Result} ->
            mysql:get_result_affected_rows(Result);
        _ ->
            false
    end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 条件字符串
%% @end
%%--------------------------------------------------------------------------------------------------------------
condition_string (Condition) ->
    List = [[" AND ",Field ," = ",integer_to_list(Value)] || {Field,Value} <- Condition],
    lists:nthtail(5,lists:flatten(List)).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 更新字符串
%% @end
%%--------------------------------------------------------------------------------------------------------------
mysql_update_string (Values) ->
    List = [[" , " ,Field," = " ,value_to_list(Value),"" ] || {Field,Value} <- Values],
    lists:nthtail(3,lists:flatten(List)).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 数值转换字符
%% @end
%%--------------------------------------------------------------------------------------------------------------
value_to_list (null) -> "NULL";
value_to_list (Value) when is_float(Value) -> float_to_list(Value);
value_to_list (Value) when is_integer(Value) -> integer_to_list(Value);
value_to_list (Value) -> mysql:quote(Value).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 二进制转换字符
%% @end
%%--------------------------------------------------------------------------------------------------------------
mysql_binary_to_list(<<>>)-> "";
mysql_binary_to_list(undefined)-> "";
mysql_binary_to_list(Value)-> binary_to_list(Value).