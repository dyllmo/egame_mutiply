%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 数据库转换本地文件
%%--------------------------------------------------------------------------------------------------------------
-module(codedb).

-export([
	get/2,					%%-----获取表模板数据
	get_logic/2,			%%-----获取自定义模板数据
	create/0,				%%-----生成模板文件
	create_table/1,			%%-----生成表模板文件
	create_logic/1			%%-----生成自定义模板文件
]).

-include("game.hrl").

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 获取表模板数据
%% @end
%%--------------------------------------------------------------------------------------------------------------
get (TableName, Key) when is_atom(TableName) ->
    get(atom_to_list(TableName),Key);
get (TableName, Key) ->
    apply(list_to_atom(db_prefix() ++ TableName),get,Key).
    
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 获取自定义模板数据
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_logic (TableName, Key) when is_atom(TableName) ->
    get_logic(atom_to_list(TableName), Key);
get_logic (TableName, Key) ->
    apply(list_to_atom(logic_prefix() ++ TableName),get,Key).
    
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 生成模板文件
%% @end
%%--------------------------------------------------------------------------------------------------------------
create () ->
	case lib_misc:get_env_atom(code_db_enable,false) of
		true ->
			timer:sleep(100),
			?INFOS("===================================================================~n"),
			?INFOS("code build start~n"),
			?INFOS("===================================================================~n"),
			Module = code_module(),
			Result = case catch Module:module_info() of
				{'EXIT',{undef,_}} ->
					?INFOS("~p nonexistent !!~n",[Module]),
					?INFOS("===================================================================~n"),
					{error,undef_module};
				_ ->
					create(Module:table_list(),Module:logic_list()),
					ok
			end,
			?INFOS("code build finish~n"),
			?INFOS("===================================================================~n"),
			Result;
		false ->
			ok
	end.
   
create (TableList, LogicList) ->
	lists:foreach(fun(Table) -> create_table(Table) end,TableList),
	lists:foreach(fun(Logic) -> create_logic(Logic) end,LogicList).
    
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 生成表模板文件
%% @end
%%--------------------------------------------------------------------------------------------------------------
create_table (Table) ->
    ?INFOS("build db : ~p start~n", [Table]),
    FileName = db_prefix() ++ atom_to_list(Table),
    RecordList = [{element(2,Record),Record} || Record <- lists:keysort(2,lib_ets:tab2list(database:ets(Table)))],
    if
        is_list(RecordList) ->
            Body = get_fun_code(RecordList),
            Head = get_head(FileName),
            compile_code(code_db_dir() ++ "/" ++ FileName ++ ".erl", Head ++ Body),
			?INFOS("build db : ~p finish~n", [Table]),
			?INFOS("===================================================================~n");
        true ->
            exit({invalid_table,Table})
    end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 生成自定义模板文件
%% @end
%%--------------------------------------------------------------------------------------------------------------
create_logic (Logic) ->
    ?INFOS("build logic : ~p start~n", [Logic]),
    FileName = logic_prefix() ++ atom_to_list(Logic),
	Module = code_module(),
    Body = get_fun_code(apply(Module,Logic,[])),
    Head = get_head(FileName),
    compile_code(code_db_dir() ++ "/" ++ FileName ++ ".erl", Head ++ Body),
	?INFOS("build logic : ~p finish~n", [Logic]),
	?INFOS("===================================================================~n").
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 文件
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_head (FileName) ->
    "%%--------------------------------------------------------------------------------------------------------------
	%% Author : wu_huidong@126.com
	%% Created by : 吴惠冬
	%% Description : 本文件由codedb模块管理生成,无需修改
	%%--------------------------------------------------------------------------------------------------------------\n" ++ 
    "-module(" ++ FileName ++ ").\n-compile(export_all).\n\n".

get_fun_code (RecordList) ->
    get_fun_code(RecordList, ".").
get_fun_code ([{Key, Value} | RecordList], Body) when RecordList == [] -> 
    get_fun_body_code(Key, Value) ++ Body;
get_fun_code ([{Key, Value} | RecordList], Body) ->
    Sign = if
        RecordList /= [] ->
            ";\n";
        true ->
            ""
    end,
    get_fun_code(RecordList, Sign ++ get_fun_body_code(Key, Value) ++ Body).

get_fun_body_code (Key, Value) ->
    StrKey = get_key_str(Key),
    "\nget (" ++ StrKey ++ ") ->\n\t" ++ lists:flatten(io_lib:write(Value)).

get_key_str ({null}) ->
	"";
get_key_str (Key) ->
    lists:foldl(
        fun(I, StrKey) ->
            E = element(I, Key),
            RE = if
                is_integer(E) ->
					integer_to_list(E);
				is_list(E) ->
                    lists:flatten(io_lib:write(E));
                true ->
                    E
            end,
            if
                StrKey =:= "" ->
                    RE;
                true ->
                    StrKey ++ ", " ++ RE
            end
        end,
        "",
        lists:seq(1, size(Key))
    ).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 编译文件
%% @end
%%--------------------------------------------------------------------------------------------------------------
compile_code (FileName, Data) ->
    F = fun() ->
        {ok,File} = file:open(FileName,[write]),
        file:write(File,Data),
        ok = file:close(File),
        compile:file(FileName,[{outdir,code_db_dir()}])
    end,
    case file:read_file(FileName) of
		{ok,FileData} ->
            case string:equal(binary_to_list(FileData),Data) of
                true -> ok;
                _ -> F()
            end;
		_ ->
			F()
    end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 管理模块
%% @end
%%--------------------------------------------------------------------------------------------------------------
code_module () ->
	lib_misc:get_env_atom(code_module,mod_codedb).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% db文件前缀
%% @end
%%--------------------------------------------------------------------------------------------------------------
db_prefix () ->
	lib_misc:get_env_str(code_db_prefix,"db_").
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% logic文件前缀
%% @end
%%--------------------------------------------------------------------------------------------------------------
logic_prefix () ->
	lib_misc:get_env_str(code_logic_prefix,"logic_").
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 编译路径
%% @end
%%--------------------------------------------------------------------------------------------------------------
code_db_dir () ->
	lib_misc:get_env_str(code_db_dir,"./ebin").