%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : https处理
%%--------------------------------------------------------------------------------------------------------------
-module(https_handler).

-export([init/2]).				%%-----HandlerInit

-include("game.hrl").

%%-------------------------------------------------------------------------------------------------------------
%% @doc
%% 初始化请求入口
%% @end
%%-------------------------------------------------------------------------------------------------------------
init(Req0, Opts) ->
	Path	 = cowboy_req:path(Req0),
	Method	 = cowboy_req:method(Req0),
	Req = try do_hander(Path,Method,Req0) of
		Ret ->
			handle_result(Req0,Ret)
	catch
		Error : Reason : Stack -> 
			?ERROR(
				"login_request_error:~n"
				"    error   => ~p~n"
				"    reason  => ~p~n"
				"    req     => ~p~n"
				"    stack   => ~p~n",
				[Error,Reason,Req0,Stack]
			),
			CodeBin = iolist_to_binary(jsx:encode([{code,1}])),
			handle_result(Req0,CodeBin)
	end,
	{ok,Req,Opts}.
	
%%-------------------------------------------------------------------------------------------------------------
%% @doc
%% 结果返回
%% @end
%%-------------------------------------------------------------------------------------------------------------
handle_result (Req, Result) ->
	cowboy_req:reply(200,#{<<"content-type">> => <<"text/json;charset=utf-8">>},Result,Req).
	
%%-------------------------------------------------------------------------------------------------------------
%% @doc
%% 处理请求
%% @end
%%-------------------------------------------------------------------------------------------------------------
do_hander(Path, Method, Req) ->
	case Method of
		<<"GET">> -> do_get_hander(Path,Req);
		<<"POST">> -> do_post_hander(Path,Req)
	end.
	
%%-------------------------------------------------------------------------------------------------------------
%% @doc
%% get处理请求
%% @end
%%-------------------------------------------------------------------------------------------------------------
do_get_hander (Path, Req) ->
	DataList = cowboy_req:parse_qs(Req),
	do_api(Path,DataList).
	
%%-------------------------------------------------------------------------------------------------------------
%% @doc
%% post处理请求
%% @end
%%-------------------------------------------------------------------------------------------------------------
do_post_hander(Path, Req) ->
	case cowboy_req:has_body(Req) of
		true ->
			ok;
		false ->
			throw(body_error)
	end,
	{ok,DataList,_} = cowboy_req:read_urlencoded_body(Req),
	do_api(Path,DataList).
	
%%-------------------------------------------------------------------------------------------------------------
%% @doc
%% api
%% @end
%%-------------------------------------------------------------------------------------------------------------
do_api (<<"/login">>, DataList) ->
	UidBin	 = lib_misc:get_list_value(<<"uid">>,DataList),
	SignBin	 = lib_misc:get_list_value(<<"sign">>,DataList),
	Uid		 = binary_to_list(UidBin),
	Sign	 = binary_to_list(SignBin),
	String	 = "uid=" ++ Uid ++ "&" ++ "key=" ++ ?SIGN_KEY,
	case lib_misc:check_md5(String,Sign) of
		true ->
			ok;
		false ->
			throw(sign_error)
	end,
	Token = token:gen(Uid),
	iolist_to_binary(jsx:encode([{code,0},{token,list_to_binary(Token)}]));

do_api (_, _) ->
	throw(api_error).