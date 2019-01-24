%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 消息反序列化,本文件项目工具生成,无需修改
%%--------------------------------------------------------------------------------------------------------------
-module(router_decode).

-export([api/3]).				%%-----Api解码
-export([class/4]).				%%-----Class解码
-export([value/2]).				%%-----Value解码

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% API解码
%% @end
%%--------------------------------------------------------------------------------------------------------------
api(player, check_token, _Bin0) ->
	{String_uid,_Bin1} = value(string,_Bin0),
	{String_token,_Bin2} = value(string,_Bin1),
	[String_uid,String_token];

api(player, login, _Bin0) ->
	{String_uid,_Bin1} = value(string,_Bin0),
	{Int_time,_Bin2} = value(int,_Bin1),
	{String_sign,_Bin3} = value(string,_Bin2),
	[String_uid,Int_time,String_sign];

api (_, _, _) -> 
	exit(nonexistent_api).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% Class解码
%% @end
%%--------------------------------------------------------------------------------------------------------------
class (Module, ClassName, Arg, typeof) ->
	{[Result],_Bin1} = class(Module,ClassName,Arg,1,[]),
	{Result,_Bin1};
class (Module, ClassName, Arg, list) ->
	<<Length:16/unsigned,Arg1/binary>> = Arg,
	class(Module,ClassName,Arg1,Length,[]).

class (_, _, _, _, _) -> 
	exit(nonexistent_class).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% Value解码
%% @end
%%--------------------------------------------------------------------------------------------------------------
value (enum,Bin0) ->
	value(byte,Bin0);

value (bool, Bin0) ->
	<<Arg:8/unsigned,Bin1/binary>> = Bin0,
	{Arg > 0,Bin1};

value (byte, Bin0) ->
	<<Arg:8/unsigned,Bin1/binary>> = Bin0,
	{Arg,Bin1};

value (sbyte, Bin0) ->
	<<Arg:8/signed,Bin1/binary>> = Bin0,
	{Arg,Bin1};

value (short, Bin0) ->
	<<Arg:16/signed,Bin1/binary>> = Bin0,
	{Arg,Bin1};

value (ushort, Bin0) ->
	<<Arg:16/unsigned,Bin1/binary>> = Bin0,
	{Arg,Bin1};

value (int, Bin0) ->
	<<Arg:32/signed,Bin1/binary>> = Bin0,
	{Arg,Bin1};

value (uint, Bin0) ->
	<<Arg:32/unsigned,Bin1/binary>> = Bin0,
	{Arg,Bin1};

value (long, Bin0) ->
	<<Arg:64/signed,Bin1/binary>> = Bin0,
	{Arg,Bin1};

value (ulong, Bin0) ->
	<<Arg:64/unsigned,Bin1/binary>> = Bin0,
	{Arg,Bin1};

value (float, Bin0) ->
	{Str,Bin1} = value(string,8,Bin0),
	{list_to_float(Str),Bin1};

value (double, Bin0) ->
	{Str,Bin1} = value(string,8,Bin0),
	{list_to_float(Str),Bin1};

value (string, Bin0) ->
	value(string,16,Bin0).

value (string, Bit, Bin0) ->
	<<ArgLen:Bit/unsigned,ArgStr:ArgLen/binary,Bin1/binary>> = Bin0,
	Arg = binary_to_list(ArgStr),
	{Arg,Bin1}.
