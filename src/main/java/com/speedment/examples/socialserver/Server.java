package com.speedment.examples.socialserver;

import fi.iki.elonen.ServerRunner;
import java.math.BigInteger;
import java.security.SecureRandom;
import java.util.Optional;
import java.util.OptionalLong;
import java.util.Random;

/**
 *
 * @author Emil Forslund
 */
public class Server extends ServerBase {

    protected final Random random = new SecureRandom();

    @Override
    public String onRegister(String mail, String password) {
        // TODO: Write register function.
        return "false";
    }

    @Override
    public String onLogin(String mail, String password) {
        // TODO: Write login function.
        return "false";
    }

    @Override
    public String onSelf(String sessionKey) {
        // TODO: Write self function.
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
    public String onBrowse(String sessionKey, OptionalLong from, OptionalLong to) {
        // TODO: Write browse function.
        return "false";
    }

    @Override
    public String onUpdate(String mail, String firstname, String lastName, Optional<String> avatar, String sessionKey) {
        // TODO: Write update profile
        return "false";
    }

    protected String nextSessionId() {
        return new BigInteger(130, random).toString(32);
    }

    /**
     * @param args the command line arguments
     */
    public static void main(String... args) {
        ServerRunner.run(Server.class);
    }
}
