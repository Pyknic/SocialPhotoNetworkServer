package com.speedment.examples.polaroidserver.solution;

import com.company.speedment.orm.test.polaroid.PolaroidApplication;
import com.company.speedment.orm.test.polaroid.db0.polaroid.image.Image;
import com.company.speedment.orm.test.polaroid.db0.polaroid.image.ImageManager;
import com.company.speedment.orm.test.polaroid.db0.polaroid.link.Link;
import com.company.speedment.orm.test.polaroid.db0.polaroid.link.LinkManager;
import com.company.speedment.orm.test.polaroid.db0.polaroid.user.User;
import com.company.speedment.orm.test.polaroid.db0.polaroid.user.UserBuilder;
import com.company.speedment.orm.test.polaroid.db0.polaroid.user.UserManager;
import com.speedment.examples.polaroidserver.PolaroidServer;
import fi.iki.elonen.ServerRunner;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 *
 * @author pemi
 */
public class MyPolaroidServer extends PolaroidServer {

    private static final String NULL = "\"\"";

    private final Map<String, Long> sessionMap;

    public MyPolaroidServer() {
        this.sessionMap = new ConcurrentHashMap<>();
        new PolaroidApplication().start();
    }

    @Override
    public String onRegister(String mail, String password) {
        // Persist should return Optional<User> ?
        final User user = UserManager.get().persist(UserManager.get().builder().setMail(mail).setPassword(password));
        return Optional.ofNullable(user).map(this::newSession).orElse(fail());
    }

    @Override
    public String onLogin(String mail, String password) {
        final Optional<User> user = UserManager.get().stream().filter(u -> mail.equals(u.getMail()) & password.equals(u.getPassword())).findAny();
        return user.map(this::newSession).orElse(fail());
    }

    @Override
    public String onUpload(String title, String description, String imgData, String sessionKey) {
        return sessionProtected(sessionKey, uid -> {
            final Image image = ImageManager.get().builder()
                    .setTitle(title)
                    .setDescription(description)
                    .setUploaded(now())
                    .setUploader(uid)
                    .setImgData(imgData);
            return Optional.ofNullable(ImageManager.get().persist(image)).map(i -> success()).orElse(fail());
        });
    }

    @Override
    public String onSelf(String sessionKey) {
        return sessionProtected(sessionKey, uid -> {
            final Optional<User> user = UserManager.get().stream().filter(u -> uid.equals(u.getId())).findAny();
            return user.map(this::toJson).orElse(fail());
        });
    }

    @Override
    public String onFind(String freeText, String sessionKey) {
        return sessionProtected(sessionKey, uid -> {
            return "{" + formatKey("users") + ":["
                    + UserManager.get().stream()
                    .filter(u -> u.getMail().contains(freeText))
                    .map(this::toJson).collect(Collectors.joining(", "))
                    + "]}";
        });
    }

    @Override
    public String onFollow(long userId, String sessionKey) {
        return sessionProtected(sessionKey, uid -> {
            final Optional<Link> link = Optional.ofNullable(LinkManager.get().persist(LinkManager.get().builder().setFollower(uid).setFollows(userId)));
            return link.map(l -> success()).orElse(fail());
        });
    }

    @Override
    public String onBrowse(String sessionKey, Optional<LocalDateTime> from, Optional<LocalDateTime> to) {
        return sessionProtected(sessionKey, uid -> {
            Predicate<Image> fromPredicate = i -> from.map(e -> i.getUploaded().toLocalDateTime().isAfter(e)).orElse(true);
            Predicate<Image> toPredicate = i -> to.map(s -> i.getUploaded().toLocalDateTime().isBefore(s)).orElse(true);
            return "{" + formatKey("images") + ":["
                    + ImageManager.get().stream()
                    .filter(fromPredicate)
                    .filter(toPredicate)
                    .map(this::toJson).collect(Collectors.joining(", "))
                    + "]}";
        });
    }

    @Override
    public String onUpdate(String mail, String firstname, String lastName, Optional<String> oAvatar, String sessionKey) {
        return sessionProtected(sessionKey, uid -> {
            Optional<User> oUser = UserManager.get().stream().filter(u -> u.getId().equals(uid)).findAny();
            if (oUser.isPresent()) {
                final UserBuilder ub = UserManager.get().toBuilder(oUser.get()).setMail(mail).setFirstName(firstname).setLastName(lastName);
                if (oAvatar.isPresent()) {
                    ub.setAvatar(oAvatar.get());
                }
                Optional<User> oNewUser = Optional.ofNullable(UserManager.get().update(ub));
                return oNewUser.map(this::toJson).orElse(fail());
            }
            return fail();
        });
    }

    public String sessionProtected(String sessionKey, Function<Long, String> mapper) {
        final Long userId = sessionMap.get(sessionKey);
        if (userId != null) {
            return mapper.apply(userId);
        }
        return fail();
    }

    private String newSession(User user) {
        final String newSession = nextSessionId();
        sessionMap.put(newSession, user.getId());
        return newSession;
    }

    private String toJson(User user) {
        return toJson(
                jsonPair("id", user.getId()),
                jsonPair("mail", user.getMail()),
                jsonPair("firstname", user.getFirstName()),
                jsonPair("lastname", user.getLastName()),
                jsonPair("avatar", user.getAvatar())
        );
    }

    private String toJson(Image image) {
        return toJson(
                jsonPair("id", image.getId()),
                jsonPair("title", image.getTitle()),
                jsonPair("description", image.getDescription()),
                jsonPair("imgdata", image.getImgData()),
                jsonPair("uploaded", image.getUploaded()),
                jsonPair("uploadedby", image.findUploader())
        );
    }

    private String toJson(String... items) {
        return Stream.of(items).collect(Collectors.joining(", ", "{ ", " }"));
    }

    private String jsonPair(String key, String value) {
        return formatKey(key) + ":" + quote(value);
    }

    private String jsonPair(String key, Long value) {
        return formatKey(key) + ":" + Objects.toString(value, NULL);
    }

    private String jsonPair(String key, Timestamp value) {
        return formatKey(key) + ":" + quote(value.toString().substring(0, 19));
    }

    private String jsonPair(String key, User value) {
        return formatKey(key) + ":" + toJson(value);
    }

    private String formatKey(String key) {
        return quote(Objects.requireNonNull(key));
    }

    private String quote(Object item) {
        if (item == null) {
            return NULL;
        }
        return "\"" + item.toString() + "\"";
    }

    private String fail() {
        return Boolean.FALSE.toString();
    }

    private String success() {
        return Boolean.TRUE.toString();
    }

    private Timestamp now() {
        return new Timestamp(System.currentTimeMillis());
    }

    public static void main(String... args) {
        ServerRunner.run(MyPolaroidServer.class);
    }

}
