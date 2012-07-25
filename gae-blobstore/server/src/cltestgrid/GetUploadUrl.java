/* Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
 * See LICENSE for details.
 */
package cltestgrid;

import java.io.IOException;
import java.util.logging.Logger;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.google.appengine.api.blobstore.BlobstoreService;
import com.google.appengine.api.blobstore.BlobstoreServiceFactory;

@SuppressWarnings("serial")
public class GetUploadUrl extends HttpServlet {

  private static final Logger logger = Logger.getLogger(GetUploadUrl.class.getName());

  private BlobstoreService blobstoreService =
    BlobstoreServiceFactory.getBlobstoreService();

  @Override
  public void doGet(HttpServletRequest req, HttpServletResponse resp) 
    throws IOException, ServletException 
  {
    resp.setContentType("text/plain; charset=utf-8");

    resp.setHeader("Pragma", "no-cache");
    resp.setHeader("Expires", "Fri, 01 Jan 1990 00:00:00 GMT");
    resp.setHeader("Cache-Control", "no-cache, no-store, must-revalidate");

    String uploadURL = blobstoreService.createUploadUrl("/upload");
    logger.info("returning new uploadURL: " + uploadURL);

    resp.getWriter().print(uploadURL);
  }
}
