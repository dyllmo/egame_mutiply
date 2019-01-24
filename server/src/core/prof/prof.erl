%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 性能统计
%%--------------------------------------------------------------------------------------------------------------
-module(prof).

-behavior(gen_server2).

-export([
	time/0,			%%-----开始时间
	get_info/0,		%%-----获取性能数据(所有)
	get_info/1,		%%-----获取性能数据(模块)
	get_info/2,		%%-----获取性能数据(模块,接口)
	set_info/3,		%%-----设置性能信息
	profile/0,		%%-----打印性能数据
	profile/1,		%%-----打印性能数据
	profile/2		%%-----打印性能数据
]).

-export([
	start_link/0,
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-include("game.hrl").

-record(prof_data,{
	key = null,
	times = 0,
	runtime = 0,
	wallclock = 0
}).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 获取性能数据
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_info () ->
	gen_server2:call(?MODULE,get_info,?CALL_TIMEOUT).
	
get_info (Module) ->
	gen_server2:call(?MODULE,{get_info,Module},?CALL_TIMEOUT).
	
get_info (Module, Action) ->
	gen_server2:call(?MODULE,{get_info,Module,Action},?CALL_TIMEOUT).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 开始时间
%% @end
%%--------------------------------------------------------------------------------------------------------------
time () ->
	case lib_misc:is_prof() of
		true ->
			{Time1, _} = statistics(runtime),
			{Time2, _} = statistics(wall_clock),
			{Time1,Time2};
		false ->
			{0,0}
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 设置性能信息
%% @end
%%--------------------------------------------------------------------------------------------------------------
set_info (Module, Action, {Time1, Time2}) ->
	case lib_misc:is_prof() of
		true ->
			{Time3, _} = statistics(runtime),
			{Time4, _} = statistics(wall_clock),
			Runtime = (Time3 - Time1) / 1000.0,
			Wallclock = (Time4 - Time2) / 1000.0,
			?MODULE ! {set_info,{Module,Action},Runtime,Wallclock};
		false ->
			ok
	end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 打印性能数据
%% @end
%%--------------------------------------------------------------------------------------------------------------
profile () ->
	profile(wallclock).
	
profile (Mode) ->
	profile("profile.txt",Mode).
	
profile (FileName, Mode) ->
	{ok, File} = file:open(FileName, [write, raw]),
	List = get_info(),
	SortList = lists:sort(
		fun(A, B) -> 
			case Mode of
				wallclock ->
					RateA = A #prof_data.wallclock / A #prof_data.times,
					RateB = B #prof_data.wallclock / B #prof_data.times,
					RateA > RateB;
				runtime ->
					RateA = A #prof_data.runtime / A #prof_data.times,
					RateB = B #prof_data.runtime / B #prof_data.times,
					RateA > RateB;
				times ->
					RateA = A #prof_data.times,
					RateB = B #prof_data.times,
					RateA > RateB;
				total_wallclock ->
					RateA = A #prof_data.wallclock,
					RateB = B #prof_data.wallclock,
					RateA > RateB;
				total_runtime ->
					RateA = A #prof_data.runtime,
					RateB = B #prof_data.runtime,
					RateA > RateB
			end
		end, 
		List
	),
	file:write(File,io_lib:format("+--------------------------------------------------------+-----------+---------------------+---------------------+---------------------+---------------------+~n",[])),
	file:write(File,io_lib:format("| Module:Function.Argument                               | Times     | Total Runtime       | Total Wallclock     | Runtime             | Wallclock           |~n",[])),
	file:write(File,io_lib:format("+--------------------------------------------------------+-----------+---------------------+---------------------+---------------------+---------------------+~n",[])),
	profile_loop(File, SortList).

profile_loop(File, []) ->
    ok = file:close(File);
profile_loop(File, [#prof_data{key = {Module, Action}, times = Times, runtime = Runtime, wallclock = Wallclock} | List]) ->
	file:write(File,io_lib:format("| ~-55.s| ~-10.b| ~-20.f| ~-20.f| ~-20.f| ~-20.f|~n", [atom_to_list(Module) ++ ":" ++ atom_to_list(Action), Times, Runtime, Wallclock, Runtime / Times, Wallclock / Times])),
	file:write(File,io_lib:format("+--------------------------------------------------------+-----------+---------------------+---------------------+---------------------+---------------------+~n",[])),
	profile_loop(File,List).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% API
%% @end
%%--------------------------------------------------------------------------------------------------------------
start_link () ->
    gen_server2:start_link({local,?MODULE},?MODULE,[],[]).

init ([]) ->
	Table = lib_ets:create(prof_data,set,#prof_data.key,private),
    {ok,Table,hibernate,?BACKOFF}.
	
handle_call (get_info, _From, Table) ->
	DataList = lib_ets:tab2list(Table),
    {reply,DataList,Table,hibernate};
	
handle_call ({get_info, Module}, _From, Table) ->	
	DataList = lib_ets:select(Table,[#prof_data{key = {Module, '_'}, _ = '_' }, [], {'$_'}]),
    {reply,DataList,Table,hibernate};
	
handle_call ({get_info, Module, Action}, _From, Table) ->	
	Data = lib_ets:get(Table,{Module,Action}),
    {reply,Data,Table,hibernate}.
	
handle_cast (_Msg, Table) ->
    {noreply,Table,hibernate}.
	
handle_info ({set_info, Key, Runtime, Wallclock}, Table) ->
	case lib_ets:get(Table,Key) of
		[] -> 
			lib_ets:insert(Table,#prof_data{key = Key,times = 1,runtime = Runtime,wallclock = Wallclock});
		[Data] -> 
			lib_ets:insert(
				Table,
				Data #prof_data{
					times = Data #prof_data.times + 1,
					runtime = Data #prof_data.runtime + Runtime,
					wallclock = Data #prof_data.wallclock + Wallclock
				},
				replace
			)
	end,
	{noreply,Table,hibernate};
			
handle_info (_Msg, Table) ->
    {noreply,Table,hibernate}.

terminate (_Reason, Table) ->
    {noreply,Table}.

code_change (_OldSvn, Table, _Ext) ->
    {noreply,Table}.