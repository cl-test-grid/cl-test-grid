/* Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
 * See LICENSE for details.
 */
package cltestgrid;

import java.io.IOException;

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
    if (userAgent != null && userAgent.indexOf("Baiduspider") >= 0) {
      resp.setStatus(SC_MOVED_PERMANENTLY);
      resp.setHeader("Location", "http://www.baidu.com/search/spider.html");
      resp.setHeader("X-READ-ME", "Please honor robots.txt and don't waste our resources. http://cl-test-grid.appspot.com/robots.txt");
      return;
    }

    final String key = req.getParameter("key");
    resp.sendRedirect("http://cl-test-grid-logs.storage.googleapis.com/" + key);
  }
}
