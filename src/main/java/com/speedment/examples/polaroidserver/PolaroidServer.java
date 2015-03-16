package com.speedment.examples.polaroidserver;

import fi.iki.elonen.ServerRunner;
import java.math.BigInteger;
import java.util.HashSet;
import java.util.Set;
import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.util.Optional;

/**
 *
 * @author Emil Forslund
 */
public class PolaroidServer extends Server {
	
	private final Set<String> sessionKeys = new HashSet<>();
	private final SecureRandom random = new SecureRandom();

	@Override
	public String onLogin(String mail, String password) {
		if ("mail@example.com".equals(mail)) {
			final String key = nextSessionId();
			sessionKeys.add(key);
			return key;
		}
        return "false";
	}

	@Override
	public String onRegister(String mail, String password) {
		// TODO: Write register function.
        return "false";
	}
    
    @Override
    public String onUpload(String title, String description, String imgData, String sessionKey) {
        // TODO: Write upload function.
        return "false";
    }

    @Override
    public String onFind(String freeText, String sessionKey) {
        // TODO: Write find function.
        return "false";
    }

    @Override
    public String onFollow(long userId, String sessionKey) {
        // TODO: Write follow function.
        return "false";
    }

    @Override
    public String onBrowse(String sessionKey, Optional<LocalDateTime> before, Optional<LocalDateTime> after) {
        // TODO: Write browse function.
        return "false";
    }
    
    private boolean verify(String sessionKey) {
		return sessionKeys.contains(sessionKey);
	}
	
	private String nextSessionId() {
		return new BigInteger(130, random).toString(32);
	}

	/**
	 * @param args the command line arguments
	 */
	public static void main(String... args) {
		ServerRunner.run(PolaroidServer.class);
	}
}
