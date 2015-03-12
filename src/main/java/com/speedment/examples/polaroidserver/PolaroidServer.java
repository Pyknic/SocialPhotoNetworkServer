package com.speedment.examples.polaroidserver;

import fi.iki.elonen.ServerRunner;
import java.math.BigInteger;
import java.util.HashSet;
import java.util.Set;
import java.security.SecureRandom;

/**
 *
 * @author Emil Forslund
 */
public class PolaroidServer extends Server {
	
	private final Set<String> sessionKeys = new HashSet<>();
	private final SecureRandom random = new SecureRandom();

	@Override
	public boolean verify(String sessionKey) {
		return sessionKeys.contains(sessionKey);
	}

	@Override
	public String onLogin(String mail, String password) {
		// TODO Read from database.
		if (mail.equals("emil.duncan@gmail.com")) {
			final String key = nextSessionId();
			sessionKeys.add(key);
			return key;
		} else {
			return "false";
		}
	}

	@Override
	public String onRegister(String mail, String password) {
		if (!mail.equals("emil.duncan@gmail.com")) {
			// TODO Insert into database.
			return onLogin(mail, password);
		} else {
			return "false";
		}
	}

	@Override
	public String getPictures() {
		return "{\"title\":\"Desert\",\"description\":\"A nice desert.\",\"src\":\"http://www.desertroseracing.com/wp-content/themes/desertrose/images/bg-dunes2.jpg\"}";
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
