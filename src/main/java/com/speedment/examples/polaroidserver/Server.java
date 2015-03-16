package com.speedment.examples.polaroidserver;

import fi.iki.elonen.NanoHTTPD;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import static java.util.Optional.ofNullable;
import java.util.stream.Collectors;

/**
 *
 * @author Emil Forslund
 */
public abstract class Server extends NanoHTTPD implements ServerAPI {
	public Server() {
		super (8080);
		System.out.println("System running on http://127.0.0.1:8080.");
	}
	
	@Override 
	public NanoHTTPD.Response serve(NanoHTTPD.IHTTPSession session) {
		final Map<String, String> files = new HashMap<>();
        final NanoHTTPD.Method method = session.getMethod();
        final String uri = session.getUri();
        final String command = uri.substring(uri.indexOf("/") + 1);

		if (Method.PUT.equals(method) 
		||  Method.POST.equals(method)) {
			try {
				session.parseBody(files);
			} catch (IOException ex) {
				return new Response(Response.Status.INTERNAL_ERROR, MIME_PLAINTEXT, 
                    "SERVER INTERNAL ERROR: IOException: " + ex.getMessage()
                );
			} catch (ResponseException ex) {
				return new Response(ex.getStatus(), MIME_PLAINTEXT, ex.getMessage());
			}
		}

		final Map<String, String> params = session.getParms();
		
        System.out.print(
			method + " '" + uri + "' " +
			params.entrySet().stream()
                .map(e -> "\"" + e.getKey() + "\" = \"" + e.getValue() + "\"")
                .collect(Collectors.joining(", ", "(", ")")) +
			" -> "
		);

        final long userId	     = parseLong(params, "userid");
		final String mail	     = parseString(params, "mail");
		final String password    = parseString(params, "password");
        final String sessionKey  = parseString(params, "sessionKey");
        final String title       = parseString(params, "title");
        final String description = parseString(params, "description");
        final String imgData     = parseString(params, "imgdata");
        final String freeText    = parseString(params, "freetext");
        final Optional<LocalDateTime> beforeTimestamp = parseTime(params, "beforeTimestamp");
        final Optional<LocalDateTime> afterTimestamp  = parseTime(params, "afterTimestamp");
		
		final String msg;
		switch (command) {
			case "register" : msg = onRegister(mail, password); break;
            case "login" :	  msg = onLogin(mail, password); break;
            case "uploaded" : msg = onUpload(title, description, imgData, sessionKey); break;
            case "find" :	  msg = onFind(freeText, sessionKey); break;
			case "follow" :   msg = onFollow(userId, sessionKey); break;
            case "browse" :	  msg = onBrowse(sessionKey, beforeTimestamp, afterTimestamp); break; 
			default : msg = "Unknown command."; break;
		}
		
		System.out.println("\"" + msg + "\"");
		
		return new NanoHTTPD.Response(msg);
    }
    
    private long parseLong(Map<String, String> params, String command) {
        return parseOptional(params, command).map(s -> Long.parseLong(s)).orElse(-1L);
    }
    
    private Optional<LocalDateTime> parseTime(Map<String, String> params, String command) {
        return parseOptional(params, command).map(s -> LocalDateTime.parse(s));
    }
    
    private String parseString(Map<String, String> params, String command) {
        return parseOptional(params, command).orElse("");
    }
    
    private Optional<String> parseOptional(Map<String, String> params, String command) {
        return ofNullable(params.get(command)).map(s -> s.trim());
    }
}
