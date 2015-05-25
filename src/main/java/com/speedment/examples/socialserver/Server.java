package com.speedment.examples.socialserver;

import com.company.speedment.orm.test.project_1.Project1Application;
import com.company.speedment.orm.test.project_1.db0.socialnetwork.image.Image;
import com.company.speedment.orm.test.project_1.db0.socialnetwork.image.ImageField;
import com.company.speedment.orm.test.project_1.db0.socialnetwork.image.ImageManager;
import com.company.speedment.orm.test.project_1.db0.socialnetwork.link.Link;
import com.company.speedment.orm.test.project_1.db0.socialnetwork.link.LinkField;
import com.company.speedment.orm.test.project_1.db0.socialnetwork.user.User;
import com.company.speedment.orm.test.project_1.db0.socialnetwork.user.UserBuilder;
import com.company.speedment.orm.test.project_1.db0.socialnetwork.user.UserField;
import com.company.speedment.orm.test.project_1.db0.socialnetwork.user.UserManager;
import com.speedment.util.json.Json;
import fi.iki.elonen.ServerRunner;
import java.math.BigInteger;
import java.security.SecureRandom;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Random;
import java.util.Set;
import java.util.stream.Collectors;
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
    
    private String createSession(User user) {
		final String key = nextSessionId();
		sessions.put(key, user.getId());
		return key;
	}
	
	private Optional<User> getLoggedIn(String key) {
        return Optional.ofNullable(sessions.get(key))
            .flatMap(id -> User.stream()
                .filter(UserField.ID.equal(id))
                .findAny()
            );
	}

    @Override
    public String onRegister(String mail, String password) {
        return User.builder()
			.setMail(mail)
			.setPassword(password)
			.persist()
			.map(this::createSession)
			.orElse("false");
    }

    @Override
    public String onLogin(String mail, String password) {
        return User.stream()
            .filter(UserField.MAIL.equalIgnoreCase(mail))
            .filter(UserField.PASSWORD.equal(password))
            .findAny()
            .map(this::createSession)
            .orElse("false");
    }

    @Override
    public String onSelf(String sessionKey) {
        return getLoggedIn(sessionKey)
			.map(User::toJson)
			.orElse("false");
    }

    @Override
    public String onUpload(String title, String description, String imgData, String sessionKey) {
		return getLoggedIn(sessionKey)
            .flatMap(me -> 
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
		return getLoggedIn(sessionKey).map(me -> 
			"{\"users\":[" +
				User.stream()
                    // If the freetext matches any field.
                    .filter(
                        UserField.FIRSTNAME.startsWith(freeText).or(
                        UserField.LASTNAME.startsWith(freeText)).or(
                        UserField.MAIL.startsWith(freeText))
                    )
                    
                    // And this is not us.
                    .filter(UserField.ID.notEqual(me.getId()))
                    
                    // Remove people we already follow
                    .filter(them -> !me.linksByFollower()
                        .anyMatch(LinkField.FOLLOWS.equal(them.getId()))
                    )
                    
                    // Limit result to 10 persons.
                    .limit(10)
                    
                    // Sort the list after number of mutual contacts
                    .sorted((a, b) -> {
                        final Set<Long> 
                            total = a.linksByFollower()
                                .map(Link::findFollows)
                                .map(User::getId)
                                .collect(Collectors.toSet()),
                        
                            onlyB = b.linksByFollower()
                                .map(Link::findFollows)
                                .map(User::getId)
                                .collect(Collectors.toSet()),
                            
                            intersection = new HashSet<>(total);
                        
                        intersection.retainAll(onlyB);
                        total.addAll(onlyB);
                        
                        return total.size() - intersection.size();
                    })

					.map(u -> UserManager.get()
                        .toJson()
                        .remove(UserField.PASSWORD)
                        .build(u)
                    )
                    
					.collect(joining(", "))
			+ "]}"
        ).orElse("{\"users:\":[]}");
    }

    @Override
    public String onFollow(long userId, String sessionKey) {
        return getLoggedIn(sessionKey)
            .flatMap(me -> Link.builder()
                .setFollower(me.getId())
                .setFollows(userId)
                .persist()
                .map(l -> "true")
            ).orElse("false");
    }

    @Override
    public String onBrowse(String sessionKey, Optional<Timestamp> from, Optional<Timestamp> to) {
        return getLoggedIn(sessionKey).map(me -> 
            "{\"images\":[" + 
            
            // Stream us and allFrom the people we follow
            Stream.concat(
                Stream.of(me),
                me.linksByFollower().map(Link::findFollows))
                
                // Get allFrom the images uploaded by these users.
                .flatMap(User::images)
                
                // Filter the pictures uploaded since the last poll
                .filter(img -> !from.isPresent() || img.getUploaded().after(from.get()))
                .filter(img -> !to.isPresent()   || img.getUploaded().before(to.get()))
                
                // Convert them to json.
                .map(img -> Json.allFrom(ImageManager.get())
                    .put(ImageField.UPLOADER, 
                        Json.allFrom(UserManager.get())
                            .remove(UserField.PASSWORD)
                    ).build(img)
                )
                
                .collect(joining(", ")) + "]}"
        ).orElse("false");
    }

    @Override
    public String onUpdate(String mail, String firstname, String lastName, 
        Optional<String> avatar, String sessionKey) {
        
		return getLoggedIn(sessionKey)
			.flatMap(me -> {
				final UserBuilder ub = me.toBuilder()
					.setMail(mail)
					.setFirstName(firstname)
					.setLastName(lastName);
				
				avatar.ifPresent(a -> ub.setAvatar(a));
					
				return ub.update();
			}).map(usr -> Json
                .allFrom(UserManager.get())
                .remove(UserField.PASSWORD)
                .build(usr)
            )
            .orElse("false");
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