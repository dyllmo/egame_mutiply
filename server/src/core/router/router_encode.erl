%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 消息序列化,本文件项目工具生成,无需修改
%%--------------------------------------------------------------------------------------------------------------
-module(router_encode).

-export([api/2]).				%%-----Api转码
-export([api/3]).				%%-----Api转码
-export([class/4]).				%%-----Class转码
-export([value/2]).				%%-----Value转码

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% API转码
%% @end
%%--------------------------------------------------------------------------------------------------------------
api (_, _) -> 
	exit(nonexistent_api).

api (player, check_token, Arg) ->
	{Enum_result} = Arg,
	<<
		0:16/unsigned,
		Enum_result:8/unsigned
	>>;

api (player, login, Arg) ->
	{Enum_result,Long_player_id,String_nickname} = Arg,
	{StringBin_nickname,StringLen_nickname} = value(string,String_nickname),
	<<
		1:16/unsigned,
		Enum_result:8/unsigned,
		Long_player_id:64/signed,
		StringLen_nickname:16/unsigned,
		StringBin_nickname/binary
	>>;

api (_, _, _) -> 
	exit(nonexistent_api).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% Class转码
%% @end
%%--------------------------------------------------------------------------------------------------------------
class (Module, ClassName, Arg, typeof) ->
	class(Module,ClassName,Arg);
class (Module, ClassName, Arg, list) ->
	List = [class(Module,ClassName,Item) || Item <- Arg],
	Length = length(List),
	Bin = list_to_binary(List),
	{Bin,Length}.

class (_, _, _) -> 
	exit(nonexistent_class).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% Value转码
%% @end
%%--------------------------------------------------------------------------------------------------------------
value (bool, Arg) ->
	case Arg of true -> 1;false -> 0 end;

value (float, Arg) ->
	value(string,float_to_list(Arg));

value (double, Arg) ->
	value(string,float_to_list(Arg));

value (string, Arg) ->
	ArgBin = list_to_binary(Arg),
	ArgLen = size(ArgBin),
	{ArgBin,ArgLen}.
