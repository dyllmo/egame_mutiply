%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 数据库curd相关
%%--------------------------------------------------------------------------------------------------------------
-module(database_curd).

-export([
	read/1,					%%-----读取
	select/2,				%%-----查询(不分表)
	select/3,				%%-----查询(分表)
	write/1,				%%-----写入
	delete/1,				%%-----删除
	delete_select/2,		%%-----条件删除(不分表)
	delete_select/3,		%%-----条件删除(分表)
	delete_all/1,			%%-----删除所有
	do/1					%%-----执行事务
]).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 读取
%% @end
%%--------------------------------------------------------------------------------------------------------------
read (Record) ->
	Time = prof:time(),
	PKName = element(1,Record),
	Table = database_lib:table(PKName),
	ensure_init(Table),
	RowKey = erlang:delete_element(1,Record),
	R = case database_lib:spilit_pos(PKName) of
		unsplit ->
			ETable = database:ets(Table),
			lib_ets:get(ETable,RowKey);
		notpk ->
			fetch_get(Table,RowKey);
		SpilitPos ->
			SpilitVal = database:get_element(SpilitPos,Record),
			SpilitId = database:get_spilit_id(SpilitVal),
			ETable = database:ets(Table,SpilitId),
			lib_ets:get(ETable,RowKey)
	end,
	prof:set_info('database.read',Table,Time),
	R.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 查询(无分表)
%% @end
%%--------------------------------------------------------------------------------------------------------------
select (Table, MatchSpec) ->
	select(Table,unsplit,MatchSpec).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 查询(分表)
%% @end
%%--------------------------------------------------------------------------------------------------------------
select (Table, ModeOrSpilitId, MatchSpec) ->
	Time = prof:time(),
	ensure_init(Table),
	R = case ModeOrSpilitId of
		unsplit ->
			ETable = database:ets(Table),
			lib_ets:select(ETable,MatchSpec);
		slow ->
			fetch_select(Table,MatchSpec);
		SpilitVal ->
			SpilitId = database:get_spilit_id(SpilitVal),
			ETable = database:ets(Table,SpilitId),
			lib_ets:select(ETable,MatchSpec)
	end,
	prof:set_info('database.select',Table,Time),
	R.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 写入
%% @end
%%--------------------------------------------------------------------------------------------------------------
write (Record) ->
	ensure_tran(),
	Time = prof:time(),
	Table = element(1,Record),
	ETable = case database_lib:spilit_pos(Table) of
		unsplit ->
			database:ets(Table);
		SpilitPos ->
			SpilitVal = database:get_element(SpilitPos,Record),
			SpilitId = database:get_spilit_id(SpilitVal),
			database:ets(Table,SpilitId)
	end,
	RowKey = get_row_key(Record),
	R = case RowKey of
		undefined ->
			TmpRecord = case database_lib:has_increment(Table) of
				true ->
					NewId = lib_ets:increment_key(auto_increment,{Table,id}),
					IncColPos = database_lib:increment_column_pos(Table),
					database:set_element(IncColPos,Record,NewId);
				false ->
					Record
			end,
			PosList = [database:get_element(N,TmpRecord) || N <- database_lib:pk_column_pos_list(Table)],
			InitRowKey = list_to_tuple(PosList),
			NewRecord = set_row_key(TmpRecord,InitRowKey),
			case database_lib:is_ingore(Table) of	
				true ->
					ok;
				false ->
					true = lib_ets:insert(ETable,NewRecord),
					add_tran_log({insert,ETable,InitRowKey})
			end,
			add_tran_action({Table,insert,NewRecord}),
			{ok,NewRecord};
		_ ->
			ColCount = database_lib:column_count(Table),
			RowVer = get_row_ver(Record,ColCount),
			[OldRecord] = lib_ets:get(ETable,RowKey),
			OldRowVer = get_row_ver(OldRecord,ColCount),
			if OldRowVer =:= RowVer -> ok; true -> exit(row_ver_error) end,
			Changes = get_changes(ColCount,Record,OldRecord),
			NewRecord = set_row_ver(Record,ColCount,RowVer + 1),
			true = lib_ets:insert(ETable,NewRecord,replace),
			add_tran_log({update,ETable,OldRecord}),
			add_tran_action({Table,update,NewRecord,Changes}),
			{ok,NewRecord}
	end,
	prof:set_info('database.write',Table,Time),
	R.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 删除
%% @end
%%--------------------------------------------------------------------------------------------------------------
delete (Record) ->
	ensure_tran(),
	Time = prof:time(),
	Table = element(1,Record),
	ensure_init(Table),
	ETable = case database_lib:spilit_pos(Table) of
		unsplit ->
			database:ets(Table);
		SpilitPos ->
			SpilitVal = database:get_element(SpilitPos,Record),
			SpilitId = database:get_spilit_id(SpilitVal),
			database:ets(Table,SpilitId)
	end,
	RowKey = get_row_key(Record),
	true = lib_ets:delete(ETable,RowKey),
	add_tran_log({delete,ETable,Record}),
	add_tran_action({Table,delete,Record}),
	prof:set_info('database.delete',Table,Time),
	ok.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 条件删除(不分表)
%% @end
%%--------------------------------------------------------------------------------------------------------------
delete_select (Table, MatchSpec) ->
	delete_select(Table,unsplit,MatchSpec).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 条件删除(分表)
%% @end
%%--------------------------------------------------------------------------------------------------------------
delete_select (Table, ModeOrSpilitId, MatchSpec) ->
	ensure_tran(),
	Time = prof:time(),
	ensure_init(Table),
	R = case select(Table,ModeOrSpilitId,MatchSpec) of
		[] ->
			{ok,0};
		Rows ->
			{ok,lists:foldl(fun(Row,Count) -> delete(Row),Count + 1 end,0,Rows)}
	end,
	prof:set_info('database.delete_select',Table,Time),
	R.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 删除所有
%% @end
%%--------------------------------------------------------------------------------------------------------------
delete_all (Table) ->
	ensure_tran(),
	Time = prof:time(),
	ensure_init(Table),
	case database_lib:spilit_pos(Table) of
		unsplit ->
			ets:delete_all_objects(database:ets(Table));
		_ ->
			SpilitCount = database_lib:spilit_count(),
			lists:foreach(fun(Id) -> ets:delete_all_objects(database:ets(Table,Id)) end,lists:seq(0,SpilitCount - 1))
	end,
	Sql = database_lib:table_str(Table),
	add_tran_action({Table,sql,"DELETE FROM " ++ Sql ++ "\n;"}),
	prof:set_info('database.delete_all',Table,Time),
	ok.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 确保可初始化
%% @end
%%--------------------------------------------------------------------------------------------------------------
ensure_init (Table) ->
	case database_lib:is_ingore(Table) of	
		true ->
			exit(table_ingored);
		false ->
			ok
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 确保事务
%% @end
%%--------------------------------------------------------------------------------------------------------------
ensure_tran () ->
	case get(tran_action_list) of
		undefined ->
			exit(need_tran);
		_ ->
			ok
	end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 获取row_key
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_row_key (Record) ->
	element(2,Record).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 设置row_key
%% @end
%%--------------------------------------------------------------------------------------------------------------
set_row_key (Record, RowKey) ->
	setelement(2,Record,RowKey).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 获取row_ver
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_row_ver (Record, ColCount) ->
	element(ColCount + 3,Record).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 设置row_ver
%% @end
%%--------------------------------------------------------------------------------------------------------------
set_row_ver (Record, ColCount, RowVer) ->
	setelement(ColCount + 3,Record,RowVer).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 所有分表读取
%% @end
%%--------------------------------------------------------------------------------------------------------------
fetch_get (Table, Key) ->
	fetch_get(Table,Key,0,database_lib:spilit_count() - 1).

fetch_get (_, _, Max, Max) -> [];
	fetch_get (Table, Key, N, Max) ->
	ETable = database:ets(Table,N),
	case lib_ets:get(ETable,Key) of
		[] -> fetch_get(Table,Key,N + 1,Max);
		R  -> R
	end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 所有分表查询
%% @end
%%--------------------------------------------------------------------------------------------------------------
fetch_select (Table, MatchSpec) ->
	fetch_select(Table,MatchSpec,0,database_lib:spilit_count(),[]).

fetch_select (_, _, Max, Max, Result) -> lists:concat(Result);
fetch_select (Table, MatchSpec, N, Max, Result) ->
	ETable = database:ets(Table,N),
	fetch_select(Table,MatchSpec,N + 1,Max,[lib_ets:select(ETable,MatchSpec)|Result]).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 添加日志
%% @end
%%--------------------------------------------------------------------------------------------------------------
add_tran_log (Data) ->
	TranLogList = get(tran_log),
	put(tran_log,[Data|TranLogList]).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 添加动作
%% @end
%%--------------------------------------------------------------------------------------------------------------
add_tran_action (TranAction) ->
	TranActionList = get(tran_action_list),
	put(tran_action_list,[TranAction|TranActionList]).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 获取改变位置列表
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_changes(ColCount, NewRecord, OldRecord) ->
	get_changes(lists:seq(1,ColCount),NewRecord,OldRecord,[]).

get_changes([], _, _, Changes) -> Changes;
get_changes([Pos|L], NewRecord, OldRecord, Changes) ->
	case database:get_element(Pos,NewRecord) =:= database:get_element(Pos,OldRecord) of
		true ->
			get_changes(L,NewRecord,OldRecord,Changes);
		false ->
			get_changes(L,NewRecord,OldRecord,[Pos|Changes])
	end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 执行事务
%% @end
%%--------------------------------------------------------------------------------------------------------------
do (Tran) ->
	case get(tran_action_list) of
		undefined ->
			put(tran_log,[]),
			put(tran_action_list,[]),
			case catch Tran() of
				{'EXIT',{aborted,Reason}} -> 
					rollback(get(tran_log)),
					erase(tran_log),
					erase(tran_action_list),
					exit(Reason);
				{'EXIT',Reason} ->
					rollback(get(tran_log)),
					erase(tran_log),
					erase(tran_action_list),
					exit(Reason);
				Result ->
					erase(tran_log),
					TranActionList = erase(tran_action_list),
					case TranActionList of
						[] -> ok;
						_ -> database_channel ! {do,TranActionList}
					end,
					{atomic,Result}
			end;
		_ ->
			{atomic,Tran()}
	end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 事务回滚
%% @end
%%--------------------------------------------------------------------------------------------------------------
rollback ([]) -> ok;
rollback ([Data|Term]) ->
	case Data of
		{insert,Table,RowKey} ->
			ets:delete(Table,RowKey);
		{update,Table,Row} ->
			ets:insert(Table,Row);
		{delete,Table,Row} ->
			ets:insert(Table,Row)
	end,
	rollback(Term).