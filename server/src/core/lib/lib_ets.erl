-module(lib_ets).

-export([
    create/1,
    create/2,
    create/3,
    create/4,
    new/2,
    clean/1,
    insert/2,
    insert/3,
    get/2,
    get/4,
    match/2,
    match/3,
    select/2,
    check_record/2,
    update/3,
    delete/2,
    drop/1,
    select_delete/2,
    tab2list/1,
    first/1,
    next/2,
	info/1,
	increment_key/2,
	memory/1,
	size/1
]).

create (TableName) ->
    create(TableName, set, 1, protected).

create (TableName, Type) ->
    create(TableName, Type, 1, protected).

create (TableName, Type, KeyPosition) ->
    create(TableName, Type, KeyPosition, protected).

create (TableName, Type, KeyPosition, Access) ->
    case catch ets:new(TableName, [Type, named_table, Access, {keypos, KeyPosition}]) of
        {'EXIT', _Reason} ->
            error;
        Success ->
            Success
    end.
	
new (TableName, Options) ->
	case ets:info(TableName) of
		undefined ->
			ets:new(TableName,Options);
		_ ->
			ets:delete_all_objects(TableName)
	end.
	
clean (TableName) ->
	case ets:info(TableName) of
		undefined ->
			ok;
		_ ->
			ets:delete_all_objects(TableName)
	end.
	
insert (TableName, Value) ->
    insert(TableName, Value, null).

insert (TableName, Value, Type) ->
     case Type of
        null ->
            case catch ets:insert_new(TableName, Value) of
                {'EXIT', _Reason} ->
                    error;
                Success ->
                    Success
            end;
        _Other ->
            case catch ets:insert(TableName, Value) of
                {'EXIT', _Reason} ->
                    error;
                Success ->
                    Success
            end
    end.

get (TableName, Key) ->
    get(TableName, Key, null, []).

get (TableName, Key, ColumnPosition, Default) ->
    case ColumnPosition of
        null ->
            case catch ets:lookup(TableName, Key) of
                {'EXIT', _Reason} ->
                    Default;
                Data ->
                    Data
            end;
        Position ->
            case catch ets:lookup_element(TableName, Key, Position) of
                {'EXIT', _Reason} ->
                    Default;
                Data ->
                    Data
            end
    end.

match (TableName, MatchExp) ->
    match(TableName, MatchExp, null).

match (TableName, MatchExp, Type) ->
    case Type of
        null ->
            case catch ets:match(TableName, MatchExp) of
                {'EXIT', _Reason} ->
                    [];
                Data ->
                    Data
            end;
        _Other ->
            case catch ets:match_object(TableName, MatchExp) of
                {'EXIT', _Reason} ->
                    [];
                Data ->
                    Data
            end
    end.

select (TableName, MatchSpec) ->
    case catch ets:select(TableName,MatchSpec) of
        {'EXIT',_} ->
			[];
        Data ->
            Data
    end.

check_record (TableName, Key) ->
    case catch ets:member(TableName,Key) of
        {'EXIT',_} ->
            error;
        Success ->
            Success
    end.

update (TableName, Key, Value) ->
     case catch ets:update_element(TableName, Key, Value) of
        {'EXIT', _Reason} ->
            error;
        Success ->
            Success
    end.
	
delete (TableName, Key) ->
    case catch ets:delete(TableName, Key) of
        {'EXIT', _Reason} ->
            error;
        Success ->
            Success
    end.

drop (TableName) ->
    case catch ets:delete(TableName) of
        {'EXIT', _Reason} ->
            error;
        Success ->
            Success
    end.

select_delete (TableName, MatchSpec) ->
    case catch ets:select_delete(TableName, MatchSpec) of
        {'EXIT', _Reason} ->
            error;
        Success ->
            Success
    end.

tab2list (TableName) ->
    case catch ets:tab2list(TableName) of
        {'EXIT', _Reason} ->
            error;
        Success ->
            Success
    end.
    
first (TableName) ->
    ets:first(TableName).
    
next (TableName, Key) ->
    ets:next(TableName, Key).
	
info (TableName) ->
	ets:info(TableName).
	
increment_key (TableName, Key) ->
	ets:update_counter(TableName,Key,1).
	
memory (TableName) ->
	{memory,Memory} = lists:keyfind(memory,1,info(TableName)),
	Memory.
	
size (TableName) ->
	case info(TableName) of
		undefined ->
			0;
		List ->
			case lists:keyfind(size,1,List) of
				{size,Size} ->
					Size;
				false ->
					0
			end
	end.