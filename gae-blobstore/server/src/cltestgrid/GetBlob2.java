/* Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
 * See LICENSE for details.
 */
package cltestgrid;

import com.google.appengine.tools.cloudstorage.GcsFilename;
import com.google.appengine.tools.cloudstorage.GcsInputChannel;
import com.google.appengine.tools.cloudstorage.GcsServiceFactory;

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

public class GetBlob2 extends HttpServlet {

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
    GcsFilename filename = new GcsFilename("cl-test-grid-logs", key);
    GcsInputChannel readChannel = null;
    InputStream inputStream = null;
    try {
      final boolean lock = false;
      //readChannel = FileServiceFactory.getFileService().openReadChannel(file, lock);
      readChannel = GcsServiceFactory.createGcsService().openReadChannel(filename, 0);
      inputStream = Channels.newInputStream(readChannel);
      resp.setContentType("text/plain");
      // The log files are gzipped, but the Cloud Storage Client Library ungzips them automatically.
      // After we return this data, GAE gzips our output in case client accepts 'gzip' contect encoding.
      IOUtils.copy(new BufferedInputStream(inputStream, 100*1024), resp.getOutputStream());
      resp.getOutputStream().flush();
    } finally {
      IOUtils.closeQuietly(inputStream);
      IOUtils.closeQuietly(readChannel);
    }
  }
}
