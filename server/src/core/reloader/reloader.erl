%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : 热更新
%%--------------------------------------------------------------------------------------------------------------
-module(reloader).

-behaviour(gen_server2).

-export([
	doit/0,				%%-----更新
	is_changed/1		%%-----是否改变
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
-include_lib("kernel/include/file.hrl").

-record(state,{last_time,tref}).

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 更新
%% @end
%%--------------------------------------------------------------------------------------------------------------
doit () ->
	lists:foreach(
		fun({Module,FileName}) ->
			case is_list(FileName) of
				true ->
					case is_changed(Module) of
						true ->
							reload(Module);
						false ->
							ok
					end;
				false ->
					ok
			end
		end,
		code:all_loaded()
	).

doit (From, To) ->
	lists:foreach(
		fun({Module,Filename}) ->
			case is_list(Filename) of
				true ->
					case is_changed(Filename,From,To) of
						true ->
							reload(Module);
						false ->
							ok
					end;
				false ->
					ok
			end
		end,
		code:all_loaded()
	).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 是否改变
%% @end
%%--------------------------------------------------------------------------------------------------------------
is_changed (Module) ->
	Fun = fun () ->
		{Module,Beam,_} = Module:module_info(),
		{ok,{Module,Vsn1}} = beam_lib:version(Beam),
		List = code:get_object_code(Module),
		{_,Attributes} = lists:keyfind(attributes,1,List),
		{_,Vsn2} = lists:keyfind(vsn,1,Attributes),
		Vsn1 =/= Vsn2
	end,
	try Fun() of
		Result -> Result
	catch
		_ : _ -> false
	end.
	
is_changed (Filename, From, To) ->
	case file:read_file_info(Filename) of
		{ok,#file_info{mtime = Mtime}} when Mtime >= From, Mtime < To ->
			true;
		{ok,_} ->
            false;
		{error, enoent} ->
            false;
		{error, Reason} ->
			?INFO("reloader reading ~s's file info: ~p~n",[Filename,Reason]),
			false
	end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 重载代码
%% @end
%%--------------------------------------------------------------------------------------------------------------
reload (Module) ->
    code:purge(Module),
    case code:load_file(Module) of
        {module,Module} ->
			?INFO("reload ~p.beam ok.~n",[Module]),
            case erlang:function_exported(Module,test,0) of
                true ->
                    case catch Module:test() of
                        ok ->
							?INFO("call ~p:test() ok.~n",[Module]),
                            reloaded;
                        Reason ->
							?INFO("call ~p:test() fail: ~p.~n",[Reason]),
                            test_failed
                    end;
                false ->
                    reloaded
            end;
        {error,Reason} ->
			?INFO("reload ~p.beam failed: ~p.~n",[Reason]),
            error
    end.
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 发送更新消息
%% @end
%%--------------------------------------------------------------------------------------------------------------
send_doit () ->
	erlang:send_after(1000,self(),doit).
	
%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% API
%% @end
%%--------------------------------------------------------------------------------------------------------------
start_link() ->
    gen_server2:start_link({local,?MODULE},?MODULE,[],[]).

init([]) ->
	case ?IS_DEBUG of
		true ->
            {ok,#state{last_time = erlang:localtime(),tref = send_doit()},hibernate,?BACKOFF};
        false ->
            {ok,#state{last_time = erlang:localtime()},hibernate,?BACKOFF}
    end.
	
handle_call(Msg, _From, State) ->
    {reply,{ok,Msg,State},State,hibernate}.

handle_cast(_Msg, State) ->
    {noreply,State,hibernate}.

handle_info(doit, State) ->
	NowTime = erlang:localtime(),
    doit(State #state.last_time,NowTime),
	send_doit(),
    {noreply,State #state{last_time = NowTime},hibernate};
	
handle_info(_Msg, State) ->
    {noreply,State,hibernate}.

terminate(_Reason, State) ->
	catch erlang:cancel_timer(State #state.tref),
	{noreply,State}.

code_change(_Vsn, State, _Extra) ->
    {ok,State}.