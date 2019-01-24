-module(api_player).

-export([
	check_token/2,			%%-----检测token
	login/2					%%-----登陆
]).

-include("game.hrl").
-include("gen/player.hrl").

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 检测token
%% @end
%%--------------------------------------------------------------------------------------------------------------
check_token ([Uid,Token], State) ->
	case cluster:tid_trace_call(?SERVER_T_LOGIN,mod_token,check,[Uid,Token]) of
		{badrpc,nodedown} ->
			{{?LOGIN_NODE_ERROR},State};
		{badrpc,{_,invalid_uid}} ->
			{{?INVALID_UID},State};
		{badrpc,{_,invalid_token}} ->
			{{?INVALID_TOKEN}};
		{badrpc,_Reason} ->
			{{?UNKNOW_ERROR},State};
		Sid ->
			{{?SUCCESS},State #client_state{sid = Sid,check_token = true}}
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 登陆
%% @end
%%--------------------------------------------------------------------------------------------------------------
login ([Uid,Time,Sign], State) ->
	CheckToken = State #client_state.check_token,
	Return = if
		CheckToken ->
			Sid = State #client_state.sid,
			case cluster:sid_trace_call(Sid,mod_player,login,[?SID,self(),Uid,Time,Sign]) of
				{badrpc,_} ->
					{?UNKNOW_ERROR,0,""};
				Data ->
					erlang:insert_element(1,Data,?SUCCESS)
			end;
		true ->
			{?UNCHECK_TOKEN,0,""}
	end,
	PlayerId = element(2,Return),
	lib_misc:set_player_id(PlayerId),
	{Return,State #client_state{player_id = PlayerId}}.