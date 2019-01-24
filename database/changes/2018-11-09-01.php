<?php
	execute("
		DROP TABLE IF EXISTS `player_token`;
		CREATE TABLE `player_token` (		
			`uid` 		VARCHAR(128)  NOT NULL DEFAULT '' COMMENT 'uid',
			`salt` 		VARCHAR(16)   NOT NULL DEFAULT '' COMMENT 'salt',
			`srv_id` 	INTEGER 	  NOT NULL DEFAULT 0  COMMENT '服务器id',
			`token` 	VARCHAR(64)   NOT NULL DEFAULT '' COMMENT 'token',
			`time` 		INTEGER 	  NOT NULL DEFAULT 0  COMMENT '获取时间',
			PRIMARY KEY (`uid`)
		) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COMMENT='玩家Token表';
	");
?>