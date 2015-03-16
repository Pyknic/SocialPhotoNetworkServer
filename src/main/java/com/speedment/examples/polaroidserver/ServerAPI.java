package com.speedment.examples.polaroidserver;

import java.time.LocalDateTime;
import java.util.Optional;

/**
 *
 * @author Emil Forslund
 */
public interface ServerAPI {
	
    String onRegister(String mail, String password);
	String onLogin(String mail, String password);
    String onUpload(String title, String description, String imgData, String sessionKey);
    String onFind(String freeText, String sessionKey);
    String onFollow(long userId, String sessionKey);
    String onBrowse(String sessionKey, Optional<LocalDateTime> before, Optional<LocalDateTime> after);

}