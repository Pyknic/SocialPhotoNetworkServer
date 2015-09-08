package com.speedment.examples.socialserver;

import com.company.speedment.test.socialnetwork.SocialnetworkApplication;
import com.company.speedment.test.socialnetwork.db0.socialnetwork.image.Image;
import com.company.speedment.test.socialnetwork.db0.socialnetwork.link.Link;
import com.company.speedment.test.socialnetwork.db0.socialnetwork.user.User;
import com.speedment.Manager;
import com.speedment.Speedment;
import com.speedment.exception.SpeedmentException;
import com.speedment.internal.core.field.encoder.JsonEncoder;
import fi.iki.elonen.ServerRunner;
import java.math.BigInteger;
import java.security.SecureRandom;
import java.sql.Timestamp;
import java.time.Instant;
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

    private final Speedment speed;

    private final Manager<User> users;
    private final Manager<Image> images;
    private final Manager<Link> links;

    private final JsonEncoder<User> jsonUserEncoder;
    private final JsonEncoder<Image> jsonImageEncoder;

    public Server() {
        speed = new SocialnetworkApplication().build();
        users = speed.managerOf(User.class);
        images = speed.managerOf(Image.class);
        links = speed.managerOf(Link.class);

        jsonUserEncoder = JsonEncoder.allOf(users)
                .remove(User.PASSWORD);

        jsonImageEncoder = JsonEncoder
                .allOf(images)
                .put(Image.UPLOADER,
                        JsonEncoder.allOf(users)
                        .remove(User.AVATAR)
                        .remove(User.PASSWORD)
                );
    }

    private String createSession(User user) {
        final String key = nextSessionId();
        sessions.put(key, user.getId());
        return key;
    }

    private Optional<User> getLoggedIn(String key) {
        final Optional<Long> userId = Optional.ofNullable(sessions.get(key));

        return userId.flatMap(id
                -> users.stream()
                .filter(User.ID.equal(id))
                .findAny()
        );
    }

    @Override
    public String onRegister(String mail, String password) {
        // Todo
        return "false";
    }

    @Override
    public String onLogin(String mail, String password) {
        // Todo
        return "false";
    }

    @Override
    public String onSelf(String sessionKey) {
        // Todo
        return "false";
    }

    @Override
    public String onUpload(String title, String description, String imgData, String sessionKey) {
        // Todo
        return "false";
    }

    @Override
    public String onFind(String freeText, String sessionKey) {
        final Optional<User> user = getLoggedIn(sessionKey);

        if (user.isPresent()) {
            final User me = user.get();

            final Stream<User> found = users.stream()
                    // If the freetext matches any field.
                    .filter(
                            User.FIRSTNAME.startsWith(freeText).or(
                            User.LASTNAME.startsWith(freeText)).or(
                            User.MAIL.startsWith(freeText))
                    )
                    // And this is not us.
                    .filter(User.ID.notEqual(me.getId()))
                    // Remove people we already follow
                    .filter(them -> !me.findLinksByFollower()
                            .anyMatch(Link.FOLLOWS.equal(them.getId()))
                    )
                    // Limit result to 10 persons.
                    .limit(10);

            final String result = found
                    .map(jsonUserEncoder::apply)
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

            try {
                links.newInstance()
                        .setFollower(me.getId())
                        .setFollows(userId)
                        .persist();

                return "true";
            } catch (SpeedmentException ex) {
                return "false";
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
                    me.findLinksByFollower().map(Link::findFollows)
            );

            final Stream<Image> imagesToBrowse = visibleUsers
                    .flatMap(User::findImages)
                    .filter(img -> !from.isPresent() || img.getUploaded().after(from.get()))
                    .filter(img -> !to.isPresent() || img.getUploaded().before(to.get()));

            final String result = imagesToBrowse
                    .map(jsonImageEncoder::apply)
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

            final User copy = me.copy()
                    .setMail(mail)
                    .setFirstName(firstname)
                    .setLastName(lastName);

            if (avatar.isPresent()) {
                copy.setAvatar(avatar.get());
            }

            try {
                final User updated = copy.update();
                return jsonUserEncoder.apply(updated);
            } catch (SpeedmentException ex) {
                return "false";
            }
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
