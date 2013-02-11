/* Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
 * See LICENSE for details.
 */
package cltestgrid;

import java.io.IOException;
import java.net.URLEncoder;
import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.ServletException;

import com.google.appengine.api.blobstore.BlobKey;
import com.google.appengine.api.blobstore.BlobstoreService;
import com.google.appengine.api.blobstore.BlobstoreServiceFactory;

import com.google.appengine.api.datastore.DatastoreService;
import com.google.appengine.api.datastore.DatastoreServiceFactory;
import com.google.appengine.api.datastore.Entity;
import com.google.appengine.api.datastore.Key;
import com.google.appengine.api.datastore.KeyFactory;
import com.google.appengine.api.datastore.EntityNotFoundException;

import com.google.appengine.api.files.AppEngineFile;
import com.google.appengine.api.files.FileService;
import com.google.appengine.api.files.FileServiceFactory;

@SuppressWarnings("serial")
public class DeleteBlobs extends HttpServlet {

  private static final Logger logger = Logger.getLogger(Upload.class.getName());

  private DatastoreService datastore = DatastoreServiceFactory.getDatastoreService();
  private FileService fileService = FileServiceFactory.getFileService();

  @Override
  public void doGet(HttpServletRequest req, HttpServletResponse resp) throws
      IOException, ServletException 
  {
    doPost(req, resp);
  }

  @Override
  public void doPost(HttpServletRequest req, HttpServletResponse resp) throws
      IOException, ServletException 
  {
    final List<String> strKeys = parseKeys(req.getParameter("keys"));
    logger.info("deleting " + strKeys.size() + " blobs...");

    List<AppEngineFile> files = new ArrayList<AppEngineFile>();
    List<Key> shortKeys = new ArrayList<Key>();

    for (String key : strKeys) {
      // the key may be specified in one of two forms:
      if (key.length() > 19) {
        // Very long string (162 characters) are real blobstore BlobKeys;
        // the URLs with such long keys are very nasty.
        files.add(fileService.getBlobFile(new BlobKey(key)));
      } else if (!BlobKeyUtil.isCloudStorageKey(key)) {
        // Indirectly, via intermediate short key,
        // which is ID of a datastore Entity, storing 
        // the original BlobKey; 
        // that way we allow prettier URLs.
        Key datastoreKey = KeyFactory.createKey("ShortKey", Long.valueOf(key));
        try {
            Entity shortKeyEntity = datastore.get(datastoreKey);
            BlobKey blobKey = (BlobKey)shortKeyEntity.getProperty("blobKey");

            files.add(fileService.getBlobFile(blobKey));
            shortKeys.add(datastoreKey);
        } catch (EntityNotFoundException e) {
            logger.warning("Unknown short key is specified: " + key);
        }
      }
      // also delete the Google Cloud Storage file (if exists)
      files.add(new AppEngineFile("/gs/cl-test-grid-logs/" + key));
    }
    // TODO: spawn multiple threads to speed-up blob deletion
    fileService.delete(files.toArray(new AppEngineFile[files.size()]));
    datastore.delete(shortKeys);
  }

  private static final Pattern KEY_SEPARATORS = Pattern.compile("[\\n\\r,]+");

  private List<String> parseKeys(String keys) {
    if (null == keys || keys.length() < 1) {
      throw new RuntimeException("keys parameter must not be empty"); // todo: use BadRequestException
    }
    return Arrays.asList(KEY_SEPARATORS.split(keys));
  }
}
