package com.speedment.examples.socialserver;

import com.company.speedment.test.socialnetwork.SocialnetworkApplication;
import com.company.speedment.test.socialnetwork.db0.socialnetwork.image.Image;
import com.company.speedment.test.socialnetwork.db0.socialnetwork.image.ImageField;
import com.company.speedment.test.socialnetwork.db0.socialnetwork.image.ImageManager;
import com.company.speedment.test.socialnetwork.db0.socialnetwork.link.Link;
import com.company.speedment.test.socialnetwork.db0.socialnetwork.link.LinkField;
import com.company.speedment.test.socialnetwork.db0.socialnetwork.user.User;
import com.company.speedment.test.socialnetwork.db0.socialnetwork.user.UserBuilder;
import com.company.speedment.test.socialnetwork.db0.socialnetwork.user.UserField;
import com.company.speedment.test.socialnetwork.db0.socialnetwork.user.UserManager;
import com.speedment.util.json.JsonFormatter;
import fi.iki.elonen.ServerRunner;
import java.math.BigInteger;
import java.security.SecureRandom;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.Comparator;
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
    
    private final static JsonFormatter<User> JSON_USER_FORMATTER = 
        JsonFormatter
            .allFrom(UserManager.get())
            .remove(UserField.PASSWORD);
    
    private final static JsonFormatter<Image> JSON_IMAGE_FORMATTER =
        JsonFormatter
            .allFrom(ImageManager.get())
            .put(ImageField.UPLOADER, 
                JsonFormatter.allFrom(UserManager.get())
                    .remove(UserField.AVATAR)
                    .remove(UserField.PASSWORD)
            );
            
	
	public Server() {
		new SocialnetworkApplication().start();
	}
    
    private String createSession(User user) {
		final String key = nextSessionId();
		sessions.put(key, user.getId());
		return key;
	}
	
	private Optional<User> getLoggedIn(String key) {
        final Optional<Long> userId = Optional.ofNullable(sessions.get(key));
        
        return userId.flatMap(id ->
            User.stream()
                .filter(UserField.ID.equal(id))
                .findAny()
        );
	}

    @Override
    public String onRegister(String mail, String password) {
        final Optional<User> user = User.builder()
			.setMail(mail)
			.setPassword(password)
			.persist();
        
        return user.map(this::createSession)
                   .orElse("false");
    }

    @Override
    public String onLogin(String mail, String password) {
        final Optional<User> user = User.stream()
            .filter(UserField.MAIL.equalIgnoreCase(mail))
            .filter(UserField.PASSWORD.equal(password))
            .findAny();
        
        return user.map(this::createSession)
                   .orElse("false");
    }

    @Override
    public String onSelf(String sessionKey) {
        final Optional<User> user = getLoggedIn(sessionKey);
        return user.map(JSON_USER_FORMATTER::apply)
                   .orElse("false");
    }

    @Override
    public String onUpload(String title, String description, String imgData, String sessionKey) {
        final Optional<User> user = getLoggedIn(sessionKey);
        
        if (user.isPresent()) {
            final Optional<Image> img = Image.builder()
                .setTitle(title)
                .setDescription(description)
                .setImgData(imgData)
                .setUploader(user.get().getId())
                .setUploaded(Timestamp.from(Instant.now()))
                .persist();
            
            if (img.isPresent()) {
                return "true";
            }
        }
        
        return "false";
    }

    @Override
    public String onFind(String freeText, String sessionKey) {
        final Optional<User> user = getLoggedIn(sessionKey);
        
        if (user.isPresent()) {
            final User me = user.get();
            
            final Stream<User> found = User.stream()
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
                .limit(10);
            
            final String result = found
                .map(JSON_USER_FORMATTER::apply)
                .collect(joining(", "));
            
            return "{\"users\":[" + result + "]}";
        }
        
        return "false";
    }

    @Override
    public String onFollow(long userId, String sessionKey) {
        final Optional<User> user = getLoggedIn(sessionKey);
        
        if (user.isPresent()) {
            final User me = user.get();
            
            final Optional<Link> link = Link.builder()
                .setFollower(me.getId())
                .setFollows(userId)
                .persist();
            
            if (link.isPresent()) {
                return "true";
            }
        }
        
        return "false";
    }

    @Override
    public String onBrowse(String sessionKey, Optional<Timestamp> from, Optional<Timestamp> to) {
        final Optional<User> user = getLoggedIn(sessionKey);
        
        if (user.isPresent()) {
            final User me = user.get();
            
            final Stream<User> visibleUsers = Stream.concat(
                Stream.of(me),
                me.linksByFollower().map(Link::findFollows)
            );
            
            final Stream<Image> images = visibleUsers
                .flatMap(User::images)
                .filter(img -> !from.isPresent() || img.getUploaded().after(from.get()))
                .filter(img -> !to.isPresent()   || img.getUploaded().before(to.get()))
            ;
            
            final String result = images
                .map(JSON_IMAGE_FORMATTER::apply)
                .collect(joining(","));
            
            return "{\"images\":[" + result + "]}";
        }
        
        return "false";
    }

    @Override
    public String onUpdate(String mail, String firstname, String lastName, 
        Optional<String> avatar, String sessionKey) {
        
        final Optional<User> user = getLoggedIn(sessionKey);
        
        if (user.isPresent()) {
            final User me = user.get();
            
            final UserBuilder ub = me.toBuilder()
                .setMail(mail)
                .setFirstName(firstname)
                .setLastName(lastName);
            
            if (avatar.isPresent()) {
                ub.setAvatar(avatar.get());
            }
            
            final Optional<User> updated = ub.update();
            return updated.map(JSON_USER_FORMATTER::apply)
                          .orElse("false");
        }
        
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