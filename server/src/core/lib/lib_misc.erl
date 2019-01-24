%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 公共函数库
%%--------------------------------------------------------------------------------------------------------------
-module(lib_misc).

-export([
	app_name/0,						%%-----应用名
	version/0,						%%-----版本号
	is_debug/0,						%%-----是否DEBUG模式
	is_prof/0,						%%-----是否开启性能测试
	set_prof/1,						%%-----性能测试开关
	set_env/2,						%%-----设置环境变量
	get_env/1,						%%-----获取环境变量
	get_env/2,						%%-----获取环境变量
	get_env_int/1,					%%-----获取环境变量(int)
	get_env_int/2,					%%-----获取环境变量(int)
	get_env_str/1,					%%-----获取环境变量(string)
	get_env_str/2,					%%-----获取环境变量(string)
	get_env_atom/1,					%%-----获取环境变量(atom)
	get_env_atom/2,					%%-----获取环境变量(atom)
	sid/0,							%%-----sid
	tid/0,							%%-----tid
	cid/0,							%%-----cid
	bid/0,							%%-----bid
	get_player_id/0,				%%-----获取进程玩家id
	set_player_id/1,				%%-----设置进程玩家id
	hour_left_second/0,				%%-----小时剩余时间(秒)
	hour_left_second/1,				%%-----小时剩余时间(秒)
	today_left_second/0,			%%-----今天剩余时间(秒)
	today_left_second/1,			%%-----今天剩余时间(秒)
	is_today/1,						%%-----是否今天
	is_today/2,						%%-----是否今天
	is_yesterday/1,					%%-----是否昨天
	is_yesterday/2,					%%-----是否昨天
	is_before_yesterday/1,			%%-----是否昨天之前
	is_before_yesterday/2,			%%-----是否昨天之前
	is_now_week/1,					%%-----是否本周
	is_now_week/2,					%%-----是否本周
	is_last_week/1,					%%-----是否上周
	is_last_week/2,					%%-----是否上周
	week_time/1,					%%-----本周N时间
	week_time/2,					%%-----本周N时间
	next_week_time/1,				%%-----下周N时间
	next_week_time/2,				%%-----下周N时间
	last_week_time/1,				%%-----上周N时间
	last_week_time/2,				%%-----上周N时间
	is_now_month/1,					%%-----是否本月
	is_now_month/2,					%%-----是否本月
	day_of_month/0,					%%-----本月第N天
	day_of_month/1,					%%-----本月第N天
	day_of_month/2,					%%-----本月第N天
	timestamp/0,					%%-----当前时间戳(秒)
	timestamp2/0,					%%-----当前时间戳(毫秒)
	timestamp3/0,					%%-----当前时间戳(微秒)
	datetime_to_timestamp/1,		%%-----日期时间转换时间戳
	timestamp_to_datetime/1,		%%-----时间戳转换日期时间
	format_time/0,					%%-----格式化当前时间
	format_time/1,					%%-----格式化时间
	seconds_to_time/1,				%%-----秒转时间
	fix_zero/1,						%%-----补零
	ceil/1,							%%-----向上取整
	floor/1,						%%-----向下取整
	random_number/1,				%%-----获取一个1-N的随机数
	random_number/2,				%%-----获取一个N-M的随机数
	get_probability/1,				%%-----概率
	get_probability_item/1,			%%-----获取概率项
	get_probability_item_list/2,	%%-----获取概率项列表
	index_of/2,						%%-----获取元素在列表中的索引值
	random_list/1,					%%-----随机列表
	random_one_from_list/1,			%%-----列表中随机一个
	random_more_from_list/2,		%%-----列表中随机多个
	key_merge_list/1,				%%-----按key混合列表
	is_list_cross/2,				%%-----两个列表是否有交集
	is_sub_list/2,					%%-----是否是子列表
	list_del_list/2,				%%-----删除列表中有的值
	list_unique/1,					%%-----列表去重
	get_list_value/2,				%%-----从列表获取值(默认null)
	get_list_value/3,				%%-----从列表获取值
	str_length/1,					%%-----中文字符串长度
	pid_b/1,						%%-----<a.b.c> -> b
	md5/1,							%%-----md5加密(默认32位)
	md5/2,							%%-----md5加密
	salt/0,							%%-----slat(默认4位)
	salt/1,							%%-----slat
	check_md5/2,					%%-----检测md5加密
	tc/4,							%%-----测试函数性能
	tcp_send/2,						%%-----往socket发送数据
	try_apply/3,					%%-----过程调用
	trace_apply/3,					%%-----调用并追踪错误
	compress/1,						%%-----压缩
	uncompress/1,					%%-----解压
	peer_to_ip/1					%%-----peer转ip
]).

-include("game.hrl").
 
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 应用名
%% @end
%%--------------------------------------------------------------------------------------------------------------
app_name () -> game.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 版本号
%% @end
%%--------------------------------------------------------------------------------------------------------------
version  () ->
	get_env_int(version,2018010101).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 是否DEBUG模式
%% @end
%%--------------------------------------------------------------------------------------------------------------
is_debug () ->
	get_env_atom(is_debug,false).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 是否开启性能测试
%% @end
%%--------------------------------------------------------------------------------------------------------------
is_prof () ->
	get_env_atom(is_prof,false).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 性能测试开关
%% @end
%%--------------------------------------------------------------------------------------------------------------
set_prof (Val) ->
	set_env(is_prof,Val).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 设置环境变量
%% @end
%%--------------------------------------------------------------------------------------------------------------
set_env (Key, Val) ->	
	application:set_env(app_name(),Key,Val).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 获取环境变量
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_env (Key) ->
	get_env(Key,null).
	
get_env (Key, Default) ->
	case application:get_env(app_name(),Key) of
        {ok, Val} -> Val;
        undefined -> Default
    end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 获取环境变量(int)
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_env_int (Key) -> 
	get_env_int(Key,0).
	
get_env_int (Key, Default) ->
	Val = get_env(Key,Default),
	if 
		is_list(Val) -> list_to_integer(Val);
		is_atom(Val) -> list_to_integer(atom_to_list(Val)); 
		true -> Val 
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 获取环境变量(string)
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_env_str (Key) ->
	get_env_str(Key,"").
	
get_env_str (Key, Default) ->
	Val = get_env(Key,Default),
	if 
		is_atom(Val) -> atom_to_list(Val); 
		is_integer(Val) -> integer_to_list(Val);
		true -> Val 
	end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% sid
%% @end
%%--------------------------------------------------------------------------------------------------------------
sid () ->
	get_env_int(sid,0).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% tid
%% @end
%%--------------------------------------------------------------------------------------------------------------
tid () ->
	get_env_int(tid,0).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% cid
%% @end
%%--------------------------------------------------------------------------------------------------------------
cid () ->
	get_env_int(cid,0).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% bid
%% @end
%%--------------------------------------------------------------------------------------------------------------
bid () ->
	get_env_int(bid,0).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 获取进程玩家id
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_player_id () ->
	case get(the_player_id) of
		undefined ->
			0;
		PlayerId ->
			PlayerId
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 设置进程玩家id
%% @end
%%--------------------------------------------------------------------------------------------------------------
set_player_id (PlayerId) ->
	if
		PlayerId > 0 ->
			put(the_player_id,PlayerId);
		true ->
			ok
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 获取环境变量(atom)
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_env_atom (Key) ->
	get_env_atom(Key,null).
	
get_env_atom (Key, Default) ->
	Val = get_env(Key,Default),
	if 
		is_list(Val) -> list_to_atom(Val);
		is_integer(Val) -> list_to_atom(integer_to_list(Val)); 
		true -> Val 
	end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 小时剩余时间(秒)
%% @end
%%--------------------------------------------------------------------------------------------------------------
hour_left_second () ->
	hour_left_second({0,0}).

hour_left_second ({OffSetMinute, OffSetSecond}) ->
	{_,{_,Minute,Second}} = erlang:localtime(),
	if
		Minute > OffSetMinute orelse (Minute == OffSetMinute andalso Second >= OffSetSecond) ->
			3600 - ((Minute - OffSetMinute) * 60 + (Second - OffSetSecond));
		true ->
			(OffSetMinute - Minute) * 60 + (OffSetSecond - Second)
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 今天剩余时间(秒)
%% @end
%%--------------------------------------------------------------------------------------------------------------
today_left_second ()->
	today_left_second({0,0,0}).
    
today_left_second (Offset) ->
    StartTime = day_start_time(erlang:localtime(),Offset),
    StartTime + 86400 - timestamp().
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 是否今天
%% @end
%%--------------------------------------------------------------------------------------------------------------
is_today (TimeStamp) ->
    is_today(TimeStamp,{0,0,0}).
	
is_today (TimeStamp, Offset) ->
    NowTime = timestamp(),
	Interval = NowTime - TimeStamp,
    if 
		Interval >= 86400 orelse Interval =< -86400 ->
			false;
        true ->
            StartTime = day_start_time(erlang:localtime(),Offset),
			EndTime = StartTime + 86399,
			TimeStamp >= StartTime andalso TimeStamp =< EndTime
    end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 是否昨天
%% @end
%%--------------------------------------------------------------------------------------------------------------
is_yesterday (TimeStamp) ->
    is_yesterday(TimeStamp,{0,0,0}).
    
is_yesterday (TimeStamp, Offset) ->
    TodayStartTime = day_start_time(erlang:localtime(),Offset),
	YesterDayStartTime = TodayStartTime - 86400,
	YesterDayEndTime = YesterDayStartTime + 86399,
	TimeStamp >= YesterDayStartTime andalso TimeStamp =< YesterDayEndTime.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 是否昨天之前
%% @end
%%--------------------------------------------------------------------------------------------------------------
is_before_yesterday (TimeStamp) ->
	is_before_yesterday(TimeStamp,{0,0,0}).
	
is_before_yesterday (TimeStamp, Offset) ->
    {Date,_} = erlang:localtime(),	
	StartTime = datetime_to_timestamp({Date,Offset}) - 86400,
	TimeStamp < StartTime.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 一天开始时间
%% @end
%%--------------------------------------------------------------------------------------------------------------
day_start_time ({Date,Time}, Offset)->
	if 
		(Time >= Offset) ->
			datetime_to_timestamp({Date,Offset});
		true ->
			datetime_to_timestamp({Date,Offset}) - 86400
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 是否本周
%% @end
%%--------------------------------------------------------------------------------------------------------------
is_now_week (TimeStamp) ->
	is_now_week(TimeStamp,{0,0,0}).
	
is_now_week (TimeStamp, Offset) ->
	WeekStartTime = week_time(1,Offset),
	WeekEndTime = WeekStartTime + 604799,
	TimeStamp >= WeekStartTime andalso TimeStamp =< WeekEndTime.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 是否上周
%% @end
%%--------------------------------------------------------------------------------------------------------------
is_last_week (TimeStamp) ->
	is_last_week(TimeStamp,{0,0,0}).
	
is_last_week (TimeStamp, Offset) ->
	WeekStartTime = week_time(1,Offset) - 604800,
	WeekEndTime = WeekStartTime + 604799,
	TimeStamp >= WeekStartTime andalso TimeStamp =< WeekEndTime.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 本周N时间
%% @end
%%--------------------------------------------------------------------------------------------------------------
week_time (Day) ->
	week_time(Day,{0,0,0}).
	
week_time (Day, {Hour, Minute, Second} = Offset) ->
	if
		Day >= 1 andalso Day =< 7 andalso
		Hour >= 0 andalso Hour =< 23 andalso
		Minute >= 0 andalso Minute =< 59 andalso
		Second >= 0 andalso Second =< 59 ->
			ok;
		true ->
			erlang:error(badarg,[Day,Offset])
	end,
	Date = date(),
	DayNum = calendar:day_of_the_week(Date),
	WeekStartTime = datetime_to_timestamp({Date,{0,0,0}}) - (DayNum -1) * 86400,
	OffsetTime = (Day -1) * 86400 + Hour * 3600 + Minute * 60 + Second,
	WeekStartTime + OffsetTime.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 下周N时间
%% @end
%%--------------------------------------------------------------------------------------------------------------
next_week_time (Num) ->
	next_week_time(Num,{0,0,0}).
	
next_week_time (Num, Offset) ->
	week_time(Num,Offset) + 604800.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 上周N时间
%% @end
%%--------------------------------------------------------------------------------------------------------------
last_week_time (Num) ->
	last_week_time(Num,{0,0,0}).
	
last_week_time (Num, Offset) ->
	week_time(Num,Offset) - 604800.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 是否本月
%% @end
%%--------------------------------------------------------------------------------------------------------------
is_now_month (TimeStamp) ->
	{{Year,Month,_},_} = timestamp_to_datetime(TimeStamp),
	{NowYear,NowMonth,_} = erlang:date(),
	Year == NowYear andalso Month == NowMonth.
	
is_now_month (TimeStamp, Offset) ->
	LocalTime = {{Y,M,D},NowTime} = erlang:localtime(),
	{NY,NM} = if
		D == 1 andalso NowTime < Offset ->
			YearToday = datetime_to_timestamp(LocalTime) - 86400,
			{{YY,YM,_},_} = timestamp_to_datetime(YearToday),
			{YY,YM};
		true ->
			{Y,M} 
	end,
	LD = calendar:last_day_of_the_month(NY,NM),
	StartTime = datetime_to_timestamp({{NY,NM,1},Offset}),
	EndTime = datetime_to_timestamp({{NY,NM,LD},Offset}) + 86400 - 1,
	TimeStamp >= StartTime andalso TimeStamp =< EndTime.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 本月第N天
%% @end
%%--------------------------------------------------------------------------------------------------------------
day_of_month () ->
	day_of_month(timestamp(),{0,0,0}).
	
day_of_month (Offset) ->
	day_of_month(timestamp(),Offset).
	
day_of_month (TimeStamp, Offset) ->
	{{Y,M,D},Time} = timestamp_to_datetime(TimeStamp),
	{NY,NM} = if
		D == 1 andalso Time < Offset ->
			YearToday = TimeStamp - 86400,
			{{YY,YM,_},_} = timestamp_to_datetime(YearToday),
			{YY,YM};
		true ->
			{Y,M} 
	end,
	StartTime = datetime_to_timestamp({{NY,NM,1},Offset}),
	Interval = TimeStamp - StartTime,
	Interval div 86400 + 1.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 当前时间戳(秒)
%% @end
%%--------------------------------------------------------------------------------------------------------------
timestamp () ->
	{MegaSecs,Secs,_} = erlang:timestamp(),
	MegaSecs * 1000000 + Secs.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 当前时间戳(毫秒)
%% @end
%%--------------------------------------------------------------------------------------------------------------
timestamp2 () ->
	timestamp3() div 1000.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 当前时间戳(微秒)
%% @end
%%--------------------------------------------------------------------------------------------------------------
timestamp3 () ->
	{MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
	MegaSecs * 1000000000000 + Secs * 1000000 + MicroSecs.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 日期时间转换时间戳
%% @end
%%--------------------------------------------------------------------------------------------------------------
datetime_to_timestamp (DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167248000.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 时间戳转换日期时间
%% @end
%%--------------------------------------------------------------------------------------------------------------
timestamp_to_datetime(TimeStamp) ->
    calendar:gregorian_seconds_to_datetime(TimeStamp + 62167248000).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 格式化时间
%% @end
%%--------------------------------------------------------------------------------------------------------------
format_time () ->
	format_time(timestamp()).
	
format_time (TimeStamp) ->
	{{Y,M,D},{HH,MM,SS}} = timestamp_to_datetime(TimeStamp),
	integer_to_list(Y) ++ "-" 
	++ integer_to_list(M) ++ "-" 
	++ integer_to_list(D) ++ " " 
	++ integer_to_list(HH) ++ ":"
	++ integer_to_list(MM) ++ ":"
	++ integer_to_list(SS).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 秒转时间
%% @end
%%--------------------------------------------------------------------------------------------------------------
seconds_to_time (Seconds) ->
	{H,M,S} = calendar:seconds_to_time(Seconds),
	fix_zero(H) ++ ":" ++ fix_zero(M) ++ ":" ++ fix_zero(S).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 补零
%% @end
%%--------------------------------------------------------------------------------------------------------------
fix_zero (Num) ->
	if
		Num < 10 ->
			integer_to_list(0) ++ integer_to_list(Num);
		true ->
			integer_to_list(Num)
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 向上取整
%% @end
%%--------------------------------------------------------------------------------------------------------------
ceil (X) when X < 0 ->
    trunc(X);
ceil (X) ->
    T = trunc(X),
    case X - T < 0.00000001 of
        true  -> T;
        false -> T + 1
    end.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 向下取整
%% @end
%%--------------------------------------------------------------------------------------------------------------
floor (X) when X < 0 ->
    T = trunc(X),
    case X - T < 0.00000001 of
        true  -> T;
        false -> T - 1
    end;
floor (X) ->
    trunc(X).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 获取一个1-N的随机数
%% @end
%%--------------------------------------------------------------------------------------------------------------
random_number (Range) when Range =< 1 -> 
	Range;
random_number (Range) ->
    rand:uniform(Range).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 获取一个N-M的随机数
%% @end
%%--------------------------------------------------------------------------------------------------------------
random_number (1, Max) ->
	random_number(Max);
	
random_number (Min, Max) ->
    NewRange   = Max - Min + 1 ,
    random_number(NewRange) + Min - 1.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 概率
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_probability (Probability) when Probability =< 0 -> false;
get_probability (Probability) when Probability >= 100 -> true;
get_probability (Probability) ->
    IntegerProbability = integral(Probability),
    Rate = erlang:ceil(IntegerProbability / Probability * 100),
    IntegerProbability >= random_number(Rate).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 获取概率项
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_probability_item (List) ->
    Sum = lists:sum([Value||{_,Value} <- List]),
    RealList = [{Item,Value}||{Item,Value} <- List,Value > 0],
    RealRate = lists:foldl(fun({_,Value},RRate) -> max(RRate,erlang:ceil(integral(Value) / Value)) end,1,RealList),
    RandomNumber = random_number(trunc(Sum * RealRate)),
    {_,Random} = lists:foldl(
        fun({Item, Value}, {Start, Random}) ->
            Number = Value * RealRate,
            NextStart = Start + Number,
            if
                Random =:= null andalso 
				RandomNumber >= Start andalso 
				RandomNumber < NextStart ->
                    {NextStart,Item};
                true ->
                    {NextStart,Random}
            end
        end,
        {1,null},
        RealList
    ),
    Random.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 获取概率项列表
%% @end
%%--------------------------------------------------------------------------------------------------------------
get_probability_item_list (List, Count)->
	Length = length(List),
	if
		Count >= Length ->
			lists:map(fun({T,_P}) -> T end,List);
		true ->
			{Result,_} = lists:foldl(
				fun(_,{List2,List3}) ->
					Item = get_probability_item(List3),
					{[Item|List2],lists:keydelete(Item,1,List3)}
				end,
				{[],List},
				lists:seq(1,Count)
			),
			Result
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 整数
%% @end
%%--------------------------------------------------------------------------------------------------------------
integral (Number) ->
    UNumber = erlang:ceil(Number),
    if
        UNumber - Number < 0.00000001 ->
            UNumber;
        true ->
            integral(Number * 10)
    end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 获取元素在列表中的索引值
%% @end
%%--------------------------------------------------------------------------------------------------------------
index_of (Item, List) -> index_of(Item,List,1).
index_of (_, [], _) -> 0;
index_of (Item, [Item|_], Index) -> Index;
index_of (Item, [_ |T], Index) -> index_of(Item,T,Index + 1).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 随机列表
%% @end
%%--------------------------------------------------------------------------------------------------------------
random_list (List) ->
	{R,_} = lists:foldl(
		fun(_,{NewList,OldList})->
			Index = random_number(1,length(OldList)),
			Value = lists:nth(Index,OldList),
			{[Value|NewList],lists:delete(Value,OldList)}
		end,
		{[],List},
		lists:seq(1,List)
	),
	R.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 列表中随机一个
%% @end
%%--------------------------------------------------------------------------------------------------------------
random_one_from_list (List) ->
	Index = random_number(1,length(List)),
	lists:nth(Index,List).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 列表中随机多个
%% @end
%%--------------------------------------------------------------------------------------------------------------
random_more_from_list (List, Count)->
	Length = length(List),
	if
		Count >= Length ->
			random_list(List);
		true ->
			{Result,_} = lists:foldl(
				fun(_,{List2,List3})->
					Lengt2 = length(List3),
					Index = random_number(1,Lengt2),
					Value = lists:nth(Index,List3),
					{[Value|List2],lists:delete(Value,List3)}
				end,
				{[],List},
				lists:seq(1,Count)
			),
			Result
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 按key混合列表
%% @end
%%--------------------------------------------------------------------------------------------------------------
key_merge_list (List) ->
	lists:foldl(
		fun({Key,Value},L) ->
			case lists:keyfind(Key,1,L) of
				{Key,LL} ->
					lists:keyreplace(Key,1,L,{Key,[Value|LL]});
				false ->
					[{Key,[Value]}|L]
			end
		end,
		[],
		List
	).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 两个列表是否有交集
%% @end
%%--------------------------------------------------------------------------------------------------------------
is_list_cross (L1, L2) ->
    is_list_cross(L1,L2,false).
is_list_cross (_, _, true) -> true;
is_list_cross ([], _, Result) -> Result;
is_list_cross ([E|L1], L2, _) ->
	is_list_cross(L1,L2,lists:member(E,L2)).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 是否是子列表
%% @end
%%--------------------------------------------------------------------------------------------------------------
is_sub_list (L1, L2) ->
    is_sub_list(L1,L2,0,length(L2)).
is_sub_list ([], _, CC, SC) -> CC >= SC;
is_sub_list (_, [], CC, SC) -> CC >= SC;
is_sub_list (_, _, SC, SC) -> true;
is_sub_list ([E1|T1], [E2|T2], CC, SC) ->
	if	
		E1 == E2 ->
			is_sub_list(T1,T2,CC + 1,SC);
		true ->
			is_sub_list(T1,T2,CC,SC)
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 删除列表中有的值
%% @end
%%--------------------------------------------------------------------------------------------------------------
list_del_list (L1, L2)->
	lists:foldl(fun(X,L) -> lists:delete(X,L) end,L1,L2).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 列表去重
%% @end
%%--------------------------------------------------------------------------------------------------------------
list_unique (List) ->
    {_,Sum} = lists:foldl(
        fun(Item, {I,L})->
			if
				Item =/= I ->
					{Item,[Item|L]};
				true ->
					{I,L}
			end
        end,
        {null,[]},
        lists:sort(List)
    ),
	Sum.
	

%%-------------------------------------------------------------------------------------------------------------
%% @doc
%% 从列表获取值
%% @end
%%-------------------------------------------------------------------------------------------------------------
get_list_value (Key, List) ->
	get_list_value(Key,List,null).
	
get_list_value (Key, List, Default) ->
	case lists:keyfind(Key,1,List) of
		{Key,Value} ->
			Value;
		false ->
			if
				Default =/= null ->
					Default;
				true ->
					throw(key_error)
			end
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 中文字符串长度
%% @end
%%--------------------------------------------------------------------------------------------------------------
str_length([]) -> 0;
str_length([Char1 | String]) ->
	str_length(Char1, String).
str_length(Char1, String) when Char1 < 16#80 ->
	str_length(String) + 1;
str_length(Char1, String) when Char1 < 16#E0 ->
	[_Char2 | String2] = String,
	str_length(String2) + 2;
str_length(Char1, String) when Char1 < 16#F0 ->
	[_Char2, _Char3 | String2] = String,
	str_length(String2) + 2;
str_length(Char1, String) when Char1 < 16#F8 ->
	[_Char2, _Char3, _Char4 | String2] = String,
	str_length(String2) + 2;
str_length(Char1, String) when Char1 < 16#FC->
	[_Char2, _Char3, _Char4, _Char5 | String2] = String,
	str_length(String2) + 2;
str_length(Char1, String) when Char1 < 16#FE->
	[_Char2, _Char3, _Char4, _Char5, _Char6 | String2] = String,
	str_length(String2) + 2.

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% <a.b.c> -> b
%% @end
%%--------------------------------------------------------------------------------------------------------------
pid_b (Pid) ->
	PidStr = erlang:pid_to_list(Pid),
	MpidStr = lists:nth(2,string:tokens(PidStr,".")),
	list_to_integer(MpidStr).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% md5加密
%% @end
%%--------------------------------------------------------------------------------------------------------------
md5 (Str) ->
	md5(Str,32).
	
md5 (Str, Mode) ->
	Bin = erlang:md5(Str),
	NewBin = case Mode of
		16 ->
			binary:part(Bin,4,8);
		32 ->	
			Bin
	end,
	L = binary_to_list(NewBin),
	lists:concat([lists:flatten(io_lib:format("~2.16.0b",[A]))||A <- L]).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% salt
%% @end
%%--------------------------------------------------------------------------------------------------------------
salt () ->
	salt(4).
	
salt (Length) ->
	String = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
	MaxLen = length(String),
	lists:foldl(
		fun(_,S) ->
			I = random_number(MaxLen),
			S ++ string:sub_string(String,I,I)
		end,
		"",
		lists:seq(1,Length)
	).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 检测md5加密
%% @end
%%--------------------------------------------------------------------------------------------------------------
check_md5 (Str, CheckStr) ->
	HashCode1 = string:to_lower(CheckStr),
	HashCode2 = md5(Str),
	string:equal(HashCode1,HashCode2).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 测试函数性能
%% @end
%%--------------------------------------------------------------------------------------------------------------
tc (M, F, A, N) when N > 0 ->
	T1 = timestamp3(),
    {Min,Max,Sum} = tc_loop(M,F,A,N,{0,0,0}),
	Avg = Sum / N,
	?INFO(
		"test cprof finish:~n"
		"    module   => ~p~n"
		"    function => ~p~n"
		"    args     => ~w~n"
		"    range    => ~p - ~ps~n"
		"    average  => ~ps~n"
		"    total    => ~ps~n",
		[M,F,A,Min / 1000000,Max / 1000000,Avg / 1000000,Sum / 1000000]
	),
	T2 = timestamp3(),
	(T2 - T1) /1000000.
	
tc_loop (_, _, _, 0, R) -> R;  
tc_loop (M, F, A, N, {Min,Max,Sum}) ->  
    {T,_} = timer:tc(M,F,A),  
    tc_loop(M,F,A,N - 1,{min(T,Min),max(T,Max),T + Sum}).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 往socket发送数据
%% @end
%%--------------------------------------------------------------------------------------------------------------
tcp_send (Socket, Bin) ->
    case gen_tcp:send(Socket, Bin) of
       ok ->
            true;
       {error, Reason} ->
            exit({tcp_send_error,Reason})
    end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 过程调用
%% @end
%%--------------------------------------------------------------------------------------------------------------
try_apply (M, F, A) ->
    try apply(M,F,A) of
		Result ->
			Result
	catch
		Error : Reason ->
			?ERROR(
				"try apply error:~n"
				"    module   => ~p~n"
				"    function => ~p~n"
				"    args     => ~w~n"
				"    error    => ~p~n"
				"    reason   => ~p~n",
				[M,F,A,Error,Reason]
			),
			apply_failed
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 调用并追踪错误
%% @end
%%--------------------------------------------------------------------------------------------------------------
trace_apply (M, F, A) ->
	try apply(M,F,A) of
		Result ->
			Result
	catch
		Error : Reason : Stack ->
			?ERROR(
				"trace apply error:~n"
				"    module   => ~p~n"
				"    function => ~p~n"
				"    args     => ~w~n"
				"    error    => ~p~n"
				"    reason   => ~p~n"
				"    stack    => ~p~n",
				[M,F,A,Error,Reason,Stack]
			),
			apply_failed
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 压缩
%% @end
%%--------------------------------------------------------------------------------------------------------------
compress (Data) ->
	zlib:compress(term_to_binary(Data)).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 解压
%% @end
%%--------------------------------------------------------------------------------------------------------------
uncompress (Bin) ->
	binary_to_term(zlib:uncompress(Bin)).
	
%%-------------------------------------------------------------------------------------------------------------
%% @doc
%% peer转ip
%% @end
%%-------------------------------------------------------------------------------------------------------------
peer_to_ip ({{I1,I2,I3,I4},_} = _Peer) ->
	integer_to_list(I1) ++ "." ++
	integer_to_list(I2) ++ "." ++
	integer_to_list(I3) ++ "." ++
	integer_to_list(I4).