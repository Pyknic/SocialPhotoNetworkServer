package com.speedment.examples.socialserver;

import com.company.speedment.orm.test.project_1.Project1Application;
import com.company.speedment.orm.test.project_1.db0.socialnetwork.image.Image;
import com.company.speedment.orm.test.project_1.db0.socialnetwork.link.Link;
import com.company.speedment.orm.test.project_1.db0.socialnetwork.user.User;
import com.company.speedment.orm.test.project_1.db0.socialnetwork.user.UserBuilder;
import com.company.speedment.orm.test.project_1.db0.socialnetwork.user.UserField;
import fi.iki.elonen.ServerRunner;
import java.math.BigInteger;
import java.security.SecureRandom;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Random;
import static java.util.stream.Collectors.joining;
import java.util.stream.Stream;

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
			.map(User::toJson)
			.orElse("false");
    }

    @Override
    public String onUpload(String title, String description, String imgData, String sessionKey) {
		return getSession(sessionKey).map(me -> 
			Image.builder()
				.setTitle(title)
				.setDescription(description)
				.setImgData(imgData)
				.setUploader(me.getId())
				.setUploaded(new Timestamp(System.currentTimeMillis()))
				.persist()
		).map(img -> "true")
			.orElse("false");
    }

    @Override
    public String onFind(String freeText, String sessionKey) {
		return getSession(sessionKey).map(me -> 
			"{\"users\":[" +
				User.stream()
                    // If the freetext matches any field.
                    .filter(
                        UserField.FIRSTNAME.startsWith(freeText).or(
                        UserField.LASTNAME.startsWith(freeText)).or(
                        UserField.MAIL.startsWith(freeText))
                    )
                    
                    // And no link exist.
                    .filter(them -> !me.linksByFollower()
                        .anyMatch(link -> them.getId().equals(link.getFollows()))
                    )

					.map(User::toJson)
					.collect(joining(", "))
			+ "]}"
        ).orElse("{\"users:\":[]}");
    }

    @Override
    public String onFollow(long userId, String sessionKey) {
        return getSession(sessionKey)
            .map(me -> Link.builder()
                .setFollower(me.getId())
                .setFollows(userId)
                .persist()
            ).map(l -> "true")
             .orElse("false");
    }

    @Override
    public String onBrowse(String sessionKey, Optional<Timestamp> from, Optional<Timestamp> to) {
        return getSession(sessionKey).map(me -> 
            "{\"images\":[" + 
            
            // Stream us and all the people we follow
            Stream.concat(
                Stream.of(me),
                me.linksByFollower().map(l -> l.findFollows()))
                
                // Get all the images uploaded by these users.
                .flatMap(User::images)
                
                // Filter the pictures uploaded since the last poll
                .filter(img -> !from.isPresent() || img.getUploaded().after(from.get()))
                .filter(img -> !to.isPresent()   || img.getUploaded().before(to.get()))
                
                // Convert them to json.
                .map(img -> img.toJson())
                .collect(joining(", ")) + "]}"
        ).orElse("false");
    }

    @Override
    public String onUpdate(String mail, String firstname, String lastName, 
        Optional<String> avatar, String sessionKey) {
        
		return getSession(sessionKey)
			.flatMap(me -> {
				final UserBuilder ub = me.toBuilder()
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