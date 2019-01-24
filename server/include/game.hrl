%%-----应用名
-define(APP_NAME,lib_misc:app_name()).

%%-----是否debug模式
-define(IS_DEBUG,lib_misc:is_debug()).

%%-----日志(全等级)
-define(LOG(Level,Msg),glog:cast_write(Level,Msg,[])).
-define(LOG(Level,Msg,Args),glog:cast_write(Level,Msg,Args)).
-define(LOGS(Level,Msg),glog:call_write(Level,Msg,[])).
-define(LOGS(Level,Msg,Args),glog:call_write(Level,Msg,Args)).

%%-----debug日志
-define(DEBUG(Msg),glog:cast_write(debug,Msg,[])).
-define(DEBUG(Msg,Args),glog:cast_write(debug,Msg,Args)).
-define(DEBUGS(Msg),glog:call_write(debug,Msg,[])).
-define(DEBUGS(Msg,Args),glog:call_write(debug,Msg,Args)).

%%-----info日志
-define(INFO(Msg),glog:cast_write(info,Msg,[])).
-define(INFO(Msg,Args),glog:cast_write(info,Msg,Args)).
-define(INFOS(Msg),glog:call_write(info,Msg,[])).
-define(INFOS(Msg,Args),glog:call_write(info,Msg,Args)).

%%-----error日志
-define(ERROR(Msg),glog:cast_write(error,Msg,[])).
-define(ERROR(Msg,Args),glog:cast_write(error,Msg,Args)).
-define(ERRORS(Msg),glog:call_write(error,Msg,[])).
-define(ERRORS(Msg,Args),glog:call_write(error,Msg,Args)).

%%-----warning日志
-define(WARNING(Msg),glog:cast_write(warning,Msg,[])).
-define(WARNING(Msg,Args),glog:cast_write(warning,Msg,Args)).
-define(WARNINGS(Msg),glog:call_write(warning,Msg,[])).
-define(WARNINGS(Msg,Args),glog:call_write(warning,Msg,Args)).

%%-----codedb
-define(CODE_DB(FileName,Args),codedb:get(FileName,Args)).
-define(CODE_LOGIC(FileName),codedb:get_logic(FileName,[])).
-define(CODE_LOGIC(FileName,Args),codedb:get_logic(FileName,Args)).

%%-----玩家服务器id转换值
-define(PLAYER_SRV_ID_INDEX,1000000000).

%%-----签名key
-define(SIGN_KEY,"8ebe40da1fac1b1efefcb0bde484568a").

%%-----CALL超时时间
-define(CALL_TIMEOUT,60000).

%%-----gen_server2 backoff
-define(BACKOFF,{backoff,60 * 1000,60 * 1000,3600 * 1000}).

%%-----等待超时时间
-define(WAIT_TIMEOUT,1800).

%%-----服务器id
-define(SID,lib_misc:sid()).				%%-----服务器唯一id
-define(TID,lib_misc:tid()).				%%-----服务器类型id
-define(CID,lib_misc:cid()).				%%-----服务器子id
-define(BID,lib_misc:bid()).				%%-----运营商id

%%-----服务器类型
-define(SERVER_T_GATE,1).					%%-----网关服务器
-define(SERVER_T_LOGIN,2).					%%-----登陆服务器
-define(SERVER_T_LOGGER,3).					%%-----日志服务器
-define(SERVER_T_LOGIC,4).					%%-----游戏服务器

%%-----客户端状态
-record(client_state,{
    player_id		 = 0,      				%%-----玩家id
	ip				 = "0.0.0.0", 			%%-----玩家ip
	sid				 = 0, 					%%-----主逻辑服务器id
	check_token		 = false, 				%%-----是否确认过token
    last_error_time	 = 0    				%%-----最后错误时间
}).

%%-----在线玩家
-record(online,{
    player_id = 0,      					%%-----玩家id
	sid	 	  = 0, 							%%-----网关服务器id
	pid	 	  = null 						%%-----网关进程id
}).

%%-----sid对应节点信息
-record(node_info1,{
	sid		 = 0,							%%-----服务器id(唯一)
	tid		 = 0,							%%-----服务器类型id
	cid		 = 0,							%%-----子服务器id
	bid		 = 0,							%%-----运营商id
	node	 = ''							%%-----服务器节点
}).

%%-----tcb_id对应节点信息
-record(node_info2,{
	tcb_id	 = {0,0,0},						%%-----{服务器类型id,子服务器id,运营商id}
	sid		 = 0,							%%-----服务器id(唯一)
	node	 = ''							%%-----服务器节点
}).

%%-----uid转玩家id
-record(player_1,{
	uid			 = "",						%%-----uid
	player_id	 = 0						%%-----玩家id
}).

%%-----昵称转玩家id
-record(player_2,{
	nickname	 = "",						%%-----昵称
	player_id	 = 0						%%-----玩家id
}).

%%-----战斗属性
-record(fight_attribute, {
    base_health		= 0,					%%-----基础生命
    health			= 0,					%%-----生命
    max_health		= 0,					%%-----生命上限
    base_strength	= 0,               		%%-----基础力量
    strength		= 0,               		%%-----力量
    base_agile		= 0,          			%%-----基础敏捷
    agile			= 0,          			%%-----敏捷
	base_speed		= 0,          			%%-----基础速度
    speed			= 0,          			%%-----速度
    base_hit		= 0,					%%-----基础命中
    hit				= 0,					%%-----命中
    base_block		= 0,					%%-----基础格挡
	block			= 0,					%%-----格挡
	base_critical	= 0,					%%-----基础暴击
	critical		= 0						%%-----暴击
}).

%%-----玩家战斗数据
-record(player_fight_data, {
    player_id 		= 0,
    nickname 		= "",
    level			= 0,
    health			= 0,
    stunt_list		= [],
	attribute		= #fight_attribute{}
}).

%%-----战斗参数
-record(fight_param, {
    attack  = #player_fight_data{},
    defense = #player_fight_data{}
}).