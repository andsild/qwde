package qwde.web;

import java.io.IOException;
import java.net.ServerSocket;
import java.sql.SQLException;
import java.util.concurrent.Callable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.prometheus.client.exporter.HTTPServer;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;
import qwde.dataprovider.db.DatabaseManager;
import qwde.web.http.HttpServer;

@Command(name = "qwde stuff", mixinStandardHelpOptions = true, version = "0.1")
public class App implements Callable<Integer> {
  private static Logger logger = LoggerFactory.getLogger(App.class);

  @Option(names = { "-p" },
      description = "Port to run prometheus on",
      defaultValue = "9458"
  )
  private String port;

  @Option(names = { "-s" },
      description = "Port to run HTTP server on",
      defaultValue = "8080"
  )
  private String serverPort;

  @Override
  public Integer call() {
    logger.trace("Got argument \"{}\"", this.port);

    try {
      new HTTPServer(Integer.valueOf(this.port));
    } catch (IOException exception) {
      logger.error("Could not start prometheus server at 9456", exception);
      return 1;
    }

    ServerSocket server = null;
    try {
      server = new ServerSocket(Integer.valueOf(serverPort), 10);
    } catch (IOException exception) {
      logger.error("", exception);
      return 1;
    }

    try {
      DatabaseManager.initialize();
    } catch (ClassNotFoundException | IOException | SQLException exception) {
      logger.error("", exception);
      return 1;
    }

    logger.info("Started server {}", server);

    while (Thread.currentThread().isAlive()) {
      try {
        Thread t = new Thread(new HttpServer(server.accept()));
        t.start();
      } catch (Exception exception) {
        logger.error("", exception);
        return 1;
      }
    }

    return 0;
  }

  public static void main(String[] args) {
    logger.info("Starting");

    try {
      System.exit(CommandLine.call(new App(), args));
    } catch (Exception exception) {
      logger.error("Unexpected error happened", exception);
      System.exit(1);
    }
  }
}
