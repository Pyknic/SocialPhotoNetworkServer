CREATE TABLE `socialnetwork`.`user` (
  `id` int(20) NOT NULL AUTO_INCREMENT,
  `mail` varchar(128) NOT NULL,
  `password` varchar(128) NOT NULL,
  `first_name` varchar(128),
  `last_name` varchar(128),
  `avatar` text,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 AUTO_INCREMENT=100;

CREATE TABLE `socialnetwork`.`image` (
  `id` int(20) NOT NULL AUTO_INCREMENT,
  `uploader` int(20) NOT NULL,
  `title` varchar(128) NOT NULL,
  `description` text NOT NULL,
  `img_data` text NOT NULL,
  `uploaded` timestamp NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 AUTO_INCREMENT=100;

CREATE TABLE `socialnetwork`.`follow` (
  `follower` int(20) NOT NULL,
  `follows` int(20) NOT NULL,
  PRIMARY KEY (`follower`,`follows`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 AUTO_INCREMENT=100;

ALTER TABLE `socialnetwork`.`image`
  ADD CONSTRAINT `image_uploader_to_user_id` FOREIGN KEY (`uploader`) REFERENCES `user` (`id`);

ALTER TABLE `socialnetwork`.`follow`
  ADD CONSTRAINT `follow_follower_to_user_id` FOREIGN KEY (`follower`) REFERENCES `user` (`id`);
ALTER TABLE `socialnetwork`.`follow`
  ADD CONSTRAINT `follow_follows_to_user_id` FOREIGN KEY (`follows`) REFERENCES `user` (`id`);
