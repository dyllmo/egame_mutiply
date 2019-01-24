-module(wss_client).

-behaviour(websocket_client_handler).

-export([
	start/0,
	start/1,
	init/2,
	websocket_handle/3,
	websocket_info/3,
	websocket_terminate/3
]).

start () ->
	start("wss://127.0.0.1:8000").
	
start (Link) ->
    crypto:start(),
    ssl:start(),
    websocket_client:start_link(Link,?MODULE,[]).

init([], _ConnState) ->
    {ok, []}.

websocket_handle({pong, _}, _ConnState, State) ->
    {ok,State};
	
websocket_handle({binary,_Data}, _ConnState, State) ->
    {ok,State};
	
websocket_handle(Msg, _ConnState, State) ->
    io:format("Received Handle Msg ~p~n",[Msg]),
    {ok,State}.
	
websocket_info(Msg, _ConnState, State) ->
	io:format("Received Info Msg ~p~n",[Msg]),
    {ok,State}.

websocket_terminate(Reason, _ConnState, State) ->
    io:format("Websocket closed in state ~p wih reason ~p~n",[State, Reason]),
    ok.