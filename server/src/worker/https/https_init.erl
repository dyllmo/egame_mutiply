%%--------------------------------------------------------------------------------------------------------------
%% Author : wu_huidong@126.com
%% Created by : 吴惠冬
%% Description : https初始化
%%--------------------------------------------------------------------------------------------------------------
-module(https_init).

-export([start/0]). 	%%-----启动

%%--------------------------------------------------------------------------------------------------------------
%% @doc
%% 启动
%% @end
%%--------------------------------------------------------------------------------------------------------------
start() ->
	ssl:start(),
	Dispatch	 = cowboy_router:compile([{'_',[{"/login",https_handler,[]}]}]),
	ServerPort	 = lib_misc:get_env_int(server_port,8888),
	MaxConns	 = lib_misc:get_env_int(max_conns,65535),
	Acceptors	 = lib_misc:get_env_int(acceptors,10),
	CrtFile		 = lib_misc:get_env_str(crt_file,"./cert/server.crt"),
	KeyFile		 = lib_misc:get_env_str(key_file,"./cert/server.key"),
	cowboy:start_tls(
		https,
		#{
			max_connections => MaxConns,
			num_acceptors => Acceptors,
			socket_opts =>[{port,ServerPort},{certfile,CrtFile},{keyfile,KeyFile}]
		},
		#{env => #{dispatch => Dispatch}}
	).
