/* Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
 * See LICENSE for details.
 */
package cltestgrid;

import com.google.appengine.api.files.AppEngineFile;
import com.google.appengine.api.files.FileReadChannel;
import com.google.appengine.api.files.FileServiceFactory;
import org.apache.commons.io.IOUtils;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.channels.Channels;
import java.util.zip.GZIPInputStream;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import static javax.servlet.http.HttpServletResponse.SC_MOVED_PERMANENTLY;
import javax.servlet.ServletException;

public class GetBlob extends HttpServlet {

  public void doGet(HttpServletRequest req, HttpServletResponse resp)
      throws IOException, ServletException
 {
    String userAgent = req.getHeader("User-Agent");
    if (userAgent != null && userAgent.contains("Baiduspider")) {
      resp.setStatus(SC_MOVED_PERMANENTLY);
      resp.setHeader("Location", "http://www.baidu.com/search/spider.html");
      resp.setHeader("X-READ-ME", "Please honor robots.txt and don't waste our resources. http://cl-test-grid.appspot.com/robots.txt");
      return;
    }

    final String key = req.getParameter("key");
    String filename = "/gs/cl-test-grid-logs/" + key;
    AppEngineFile file = new AppEngineFile(filename);
    FileReadChannel readChannel = null;
    InputStream inputStream = null;
    try {
      final boolean lock = false;
      readChannel = FileServiceFactory.getFileService().openReadChannel(file, lock);
      inputStream = Channels.newInputStream(readChannel);
      resp.setContentType("text/plain");
      // The log files are gzipped, but we can't serve gzipped content ourselves,
      // because GAE gzips servlet output itlself and so logs would end-up gzipped txice.
      // Therefore we need to ungzip the log.
      InputStream ungzipper = new GZIPInputStream(new BufferedInputStream(inputStream, 100*1024));
      IOUtils.copy(ungzipper, resp.getOutputStream());
      resp.getOutputStream().flush();
    } finally {
      IOUtils.closeQuietly(inputStream);
      IOUtils.closeQuietly(readChannel);
    }
  }
}
