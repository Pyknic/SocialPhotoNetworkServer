package com.speedment.examples.socialserver;

import com.company.speedment.orm.test.project_1.Project1Application;
import com.company.speedment.orm.test.project_1.db0.socialnetwork.image.Image;
import com.company.speedment.orm.test.project_1.db0.socialnetwork.link.Link;
import com.company.speedment.orm.test.project_1.db0.socialnetwork.user.User;
import com.company.speedment.orm.test.project_1.db0.socialnetwork.user.UserBuilder;
import fi.iki.elonen.ServerRunner;
import java.math.BigInteger;
import java.security.SecureRandom;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Random;
import static java.util.stream.Collectors.joining;

/**
 *
 * @author Emil Forslund
 */
public class Server extends ServerBase {

    protected final Random random = new SecureRandom();
	private final Map<String, Long> sessions = new HashMap<>();
	
	public Server() {
		new Project1Application().start();
	}

    @Override
    public String onRegister(String mail, String password) {
        return User.builder()
			.setMail(mail)
			.setPassword(password)
			.persist()
			.map(this::newSession)
			.orElse("false");
    }

    @Override
    public String onLogin(String mail, String password) {
        return User.stream()
			.filter(u -> mail.equals(u.getMail()))
			.filter(u -> password.equals(u.getPassword()))
			.map(this::newSession)
			.findAny()
			.orElse("false");
    }

    @Override
    public String onSelf(String sessionKey) {
        return getSession(sessionKey)
			.map(u -> u.toJson())
			.orElse("false");
    }

    @Override
    public String onUpload(String title, String description, String imgData, String sessionKey) {
		// TODO Implement toUpload
		return "false";
    }

    @Override
    public String onFind(String freeText, String sessionKey) {
		// TODO: Implement onFind
		return "{\"users\":[]}";
    }

    @Override
    public String onFollow(long userId, String sessionKey) {
		// TODO: Implement onFollow
		return "false";
    }

    @Override
    public String onBrowse(String sessionKey, Optional<Timestamp> from, Optional<Timestamp> to) {
        return "{\"images\":[" + Image.stream()
			.filter(img -> !from.isPresent() || img.getUploaded().after(from.get()))
			.filter(img -> !to.isPresent()   || img.getUploaded().before(to.get()))
			.map(img -> img.toJson())
			.collect(joining(", ")) + "]}";
    }

    @Override
    public String onUpdate(String mail, String firstname, String lastName, Optional<String> avatar, String sessionKey) {
		return getSession(sessionKey)
			.flatMap(u -> {
				final UserBuilder ub = u.toBuilder()
					.setMail(mail)
					.setFirstName(firstname)
					.setLastName(lastName);
				
				avatar.ifPresent(a -> ub.setAvatar(a));
					
				return ub.update();
			}).map(u -> u.toJson()).orElse("false");
    }
	
	protected String newSession(User user) {
		final String key = nextSessionId();
		sessions.put(key, user.getId());
		return key;
	}
	
	protected Optional<User> getSession(String key) {
		final Long id = sessions.get(key);
		return User.stream()
			.filter(u -> u.getId().equals(id))
			.findAny();
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