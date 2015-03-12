package com.speedment.examples.polaroidserver;

import fi.iki.elonen.NanoHTTPD;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import static java.util.Optional.ofNullable;
import java.util.stream.Collectors;

/**
 *
 * @author Emil Forslund
 */
public abstract class Server extends NanoHTTPD implements PolaroidAPI {
	public Server() {
		super (8080);
		System.out.println("System running on 127.0.0.1:8080.");
	}
	
	@Override 
	public NanoHTTPD.Response serve(NanoHTTPD.IHTTPSession session) {
		Map<String, String> files = new HashMap<String, String>();
        final NanoHTTPD.Method method = session.getMethod();
        final String uri = session.getUri();

		if (Method.PUT.equals(method) 
		||  Method.POST.equals(method)) {
			try {
				session.parseBody(files);
			} catch (IOException ex) {
				return new Response(Response.Status.INTERNAL_ERROR, MIME_PLAINTEXT, "SERVER INTERNAL ERROR: IOException: " + ex.getMessage());
			} catch (ResponseException ex) {
				return new Response(ex.getStatus(), MIME_PLAINTEXT, ex.getMessage());
			}
		}

		final String postBody = session.getQueryParameterString();
		final Map<String, String> params = session.getParms();
		
        System.out.print(
			method + " '" + uri + "' " +
			params.entrySet().stream().map(e -> "\"" + e.getKey() + "\" = \"" + e.getValue() + "\"").collect(Collectors.joining(", ", "(", ")")) +
					" -> "
		);

		final String command  = ofNullable(params.get("c")).orElse("");
		final String mail	  = ofNullable(params.get("mail")).orElse("");
		final String password = ofNullable(params.get("password")).orElse("");
		
		final String msg;
		switch (command) {
			case "ping" :	  msg = "true"; break;
			case "login" :	  msg = onLogin(mail, password); break;
			case "register" : msg = onRegister(mail, password); break;
			default : msg = "Unknown command."; break;
		}
		
		System.out.println("\"" + msg + "\"");
		
		return new NanoHTTPD.Response(msg);
    }
}
