/* Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
 * See LICENSE for details.
 */
package cltestgrid;

import java.io.IOException;
import java.net.URLEncoder;
import java.util.List;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.ServletException;

import com.google.appengine.api.blobstore.BlobKey;
import com.google.appengine.api.blobstore.BlobstoreService;
import com.google.appengine.api.blobstore.BlobstoreServiceFactory;
import com.google.appengine.api.blobstore.BlobInfoFactory;

import com.google.appengine.api.datastore.DatastoreService;
import com.google.appengine.api.datastore.DatastoreServiceFactory;
import com.google.appengine.api.datastore.Entity;
import com.google.appengine.api.datastore.Key;
import com.google.appengine.api.datastore.KeyFactory;
import com.google.appengine.api.datastore.EntityNotFoundException;

@SuppressWarnings("serial")
public class GetBlob extends HttpServlet {

  private static final Logger log = Logger.getLogger(Upload.class.getName());

  private BlobstoreService blobstoreService = BlobstoreServiceFactory.getBlobstoreService();
  private DatastoreService datastore = DatastoreServiceFactory.getDatastoreService();

  public void doGet(HttpServletRequest req, HttpServletResponse resp)
      throws IOException, ServletException
 {
    String userAgent = request.getHeader("User-Agent");
    if (userAgent != null && userAgent.indexOf("Baiduspider") >= 0) {
      resp.sendRedirect("http://www.baidu.com/search/spider.html?please_honor_robots.txt_and_down_waste_our_resources.http://cl-test-grid.appspot.com/robots.txt");
      return;
    }

    final String key = req.getParameter("key");
    try {
      BlobKey blobKey;
      // the key may be specified in one of two forms:
      if (key.length() > 19) {
        // Very long string (162 characters) are real blobstore BlobKeys;
        // the URLs with such long keys are very nasty.
        blobKey = new BlobKey(key);
        if (null == new BlobInfoFactory().loadBlobInfo(blobKey)) {
          throw new NotFoundException("Unknown key is specified: " + key);
        }
      } else if (BlobKeyUtil.isCloudStorageKey(key)) {
          throw new NotFoundException("Unknown key is specified: " + key);
      } else {
        // Indirectly, via intermediate short key,
        // which is ID of a datastore Entity, storing 
        // the original BlobKey; 
        // that way we allow prettier URLs.
        Key datastoreKey = KeyFactory.createKey("ShortKey", Long.valueOf(key));
        try {
          Entity shortKeyEntity = datastore.get(datastoreKey);
          blobKey = (BlobKey)shortKeyEntity.getProperty("blobKey");
        } catch (EntityNotFoundException e) {
          throw new NotFoundException("Unknown key is specified: " + key);
        }
      }
      blobstoreService.serve(blobKey, resp);
    } catch (NotFoundException e) {
      resp.sendRedirect("http://cl-test-grid-logs.storage.googleapis.com/" + key);
    }
  }
}

class NotFoundException extends RuntimeException {
  public NotFoundException(String message) {
    super(message);
  }
}
