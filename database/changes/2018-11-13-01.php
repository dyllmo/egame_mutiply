<?php
	execute("
		DROP TABLE IF EXISTS `player`;
		CREATE TABLE `player` (		
			`id` 			BIGINT  	 NOT NULL DEFAULT 0  COMMENT 'id',
			`uid` 			VARCHAR(128) NOT NULL DEFAULT '' COMMENT 'uid',
			`nickname` 		VARCHAR(32)  NOT NULL DEFAULT '' COMMENT '昵称',
			`create_time` 	INTEGER 	 NOT NULL DEFAULT 0  COMMENT '创建时间',
			PRIMARY KEY (`id`)
		) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COMMENT='玩家表';
	");
?>