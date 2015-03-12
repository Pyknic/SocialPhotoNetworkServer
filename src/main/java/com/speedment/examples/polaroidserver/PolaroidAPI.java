package com.speedment.examples.polaroidserver;

/**
 *
 * @author Emil Forslund
 */
public interface PolaroidAPI {
	boolean verify(String sessionKey);
	String onLogin(String mail, String password);
	String onRegister(String mail, String password);
	String getPictures();
}