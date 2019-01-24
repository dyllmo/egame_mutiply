{
	application,
	game, 
	[
		{description,"The Game Server"},
		{mod,{game,[]}},
		{env,[
			{version,2018010101},						%%-----版本
			{code_db_dir,"../ebin"},					%%-----code_db文件编译目录(默认./ebin)
			{code_db_prefix,"db_"},						%%-----db文件前缀(默认db_)
			{code_logic_prefix,"logic_"},				%%-----logic文件前缀(默认logic_)
			{code_module,mod_codedb},					%%-----codedb逻辑模块(默认mod_codedb)
			{uets_module,mod_uets},						%%-----uets逻辑模块(默认mod_uets)
			{log_dir,"../log"},							%%-----错误日志目录(默认./log)
			{data_dir,"../data"},						%%-----数据库日志目录(默认./data)
			{nodes_config,"../conf/nodes.config"}		%%-----节点列表文件(默认./nodes.config)
		]}
	]
}. 