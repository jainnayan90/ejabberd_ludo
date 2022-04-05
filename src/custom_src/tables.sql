CREATE TABLE `players_info` (
	`id` int NOT NULL AUTO_INCREMENT,
	`user_id` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL,
	`name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT 'Guest',
        `domain`  varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL,
	`level` int(11)  NOT NULL default 1,
	`chips` bigint(20) NOT NULL default 2000,
	`image_url` varchar(500) default '',
	`created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	`last_active` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
	PRIMARY KEY (`id`),
	UNIQUE USERID(`user_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;



DELIMITER $$ 
CREATE TRIGGER ADD_PLAYER AFTER INSERT ON users
FOR EACH ROW 
BEGIN
INSERT INTO players_info (user_id, level, chips) VALUES (NEW.username, 1, 2000);
END $$    
DELIMITER;


CREATE TABLE `players_device_tokens` (
	`id` int NOT NULL AUTO_INCREMENT,
	`user_id` varchar(191) COLLATE utf8mb4_unicode_ci NOT NULL,
    `device_type` ENUM('android', 'ios') NOT NULL default 'android',
    `device_id` varchar(255) NOT NULL,
	`token` varchar(500) NOT NULL,
	`created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	PRIMARY KEY (`id`),
    CONSTRAINT UC_USER_DEVICE UNIQUE (user_id, device_id),  
	CONSTRAINT FK_DEVICE_USER_ID FOREIGN KEY (user_id) references users(username)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;


