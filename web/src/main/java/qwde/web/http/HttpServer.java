package qwde.web.http;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;
import java.nio.charset.Charset;
import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import qwde.web.servlets.BollingerBrand;
import qwde.web.servlets.IndexServlet;
import qwde.web.servlets.SimpleMovingAverage;
import qwde.web.util.FileUtil;

public class HttpServer implements Runnable {
  private static Logger logger = LoggerFactory.getLogger(HttpServer.class);
  private static final String HTTP_200 = "HTTP/1.1 200 OK\r\n";
  private static final String HTTP_404 = "HTTP/1.1 404 Not Found\r\n";
  private static final String HTTP_500 = "HTTP/1.1 500 \r\n";

  private final Socket client;
  private DataOutputStream outClient;

  public HttpServer(Socket cl) {
    client = cl;
  }

  public Map<String, List<String>> getQueries(String httpQuery) {
    if (httpQuery.indexOf('?') < 0) {
      return Collections.emptyMap();
    }

    return Arrays.stream(httpQuery.substring(httpQuery.indexOf('?') + 1).split("&"))
      .map(this::splitQueryParameter)
      .collect(Collectors.groupingBy(SimpleImmutableEntry::getKey, LinkedHashMap::new, Collectors.mapping(Map.Entry::getValue, Collectors.toList())));
  }

  public SimpleImmutableEntry<String, String> splitQueryParameter(String it) {
    final int idx = it.indexOf('=');
    final String key = idx > 0 ? it.substring(0, idx) : it;
    final String value = idx > 0 && it.length() > idx + 1 ? it.substring(idx + 1) : null;
    return new SimpleImmutableEntry<>(key, value);
  }

  @Override
  public void run() {
    try {
      logger.debug("The Client {}:{} is connected", client.getInetAddress(), client.getPort());

      BufferedReader inClient = new BufferedReader(new InputStreamReader(client.getInputStream(), Charset.forName("UTF-8")));
      this.outClient = new DataOutputStream(client.getOutputStream());

      String requestString = inClient.readLine();
      String headerLine = requestString;

      if (headerLine == null) {
        return;
      }

      StringTokenizer tokenizer = new StringTokenizer(headerLine);
      String httpMethod = tokenizer.nextToken();
      String httpQueryString = tokenizer.nextToken();

      while (inClient.ready()) {
        logger.trace(requestString);
        requestString = inClient.readLine();
      }

      Map<String, List<String>> urlMapping = getQueries(httpQueryString);

      if ("GET".equals(httpMethod)) {
        if ("/".equals(httpQueryString)) {
          sendResponse(HTTP_200, IndexServlet.doGet(urlMapping));
        } else if (httpQueryString.startsWith("/sma")) {
          sendResponse(HTTP_200, SimpleMovingAverage.doGet(urlMapping));
        } else if (httpQueryString.startsWith("/bb")) {
          sendResponse(200, BollingerBrand.doGet(urlMapping));
        } else if (httpQueryString.startsWith("/plotly-latest.min.js")) {
          try {
            sendResponse(HTTP_200, FileUtil.getResourceFile("plotly-latest.min.js"));
          } catch (IOException exception) {
            logger.debug("wtf?", exception);
            sendResponse(HTTP_404, ":(");
          }
        } else {
          sendResponse(HTTP_404, "<b>The Requested resource not found.</b>");
        }
      } else {
        sendResponse(HTTP_404, "<b>The Requested resource not found.</b>");
      }
    } catch (Exception exception) {
      logger.debug("", exception);
      try {
        sendResponse(HTTP_500, "<b>Error happened</b>");
      } catch (Exception innerException) {
        logger.debug("", innerException);
      }
    }
  }

  public void sendResponse(String status, String responseString) throws Exception {
    this.outClient.writeBytes(String.format("%s\r%n%s\r%n%s\r%n%s\r%n%s\r%n%s\r%n",
        status,
        "java.net.ServerSocket",
        "Content-Type: text/html\r\n",
        "Content-Length" + responseString.length() + "\r\n",
        "<html><title>HTTP Server in Java</title><body>" + responseString + "</body></html>",
        "Connection: close\r\n"));
    this.outClient.close();
  }
}
