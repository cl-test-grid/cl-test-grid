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
import java.util.logging.Level;
import java.util.regex.Pattern;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ConcurrentLinkedQueue;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.ServletException;

import com.google.appengine.tools.cloudstorage.GcsService;
import com.google.appengine.tools.cloudstorage.GcsServiceFactory;
import com.google.appengine.tools.cloudstorage.RetryParams;
import com.google.appengine.tools.cloudstorage.GcsFilename;

import com.google.appengine.api.ThreadManager;

@SuppressWarnings("serial")
public class DeleteBlobs extends HttpServlet {

  private static final Logger log = Logger.getLogger(DeleteBlobs.class.getName());

  private GcsService gcsService = GcsServiceFactory.createGcsService(RetryParams.getDefaultInstance());

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
    log.info("deleting " + strKeys.size() + " blobs...");

    final int THREAD_COUNT = 20;
    final ConcurrentLinkedQueue<String> tasks = new ConcurrentLinkedQueue<String>(strKeys);
    final CountDownLatch doneLatch = new CountDownLatch(THREAD_COUNT);
    // Error reference. Could be just a volatile, but inner class won't be able to access it:
    final AtomicReference<RuntimeException> error = new AtomicReference<RuntimeException>();

    for (int i = 0; i < THREAD_COUNT; i++) {
      ThreadManager.createThreadForCurrentRequest(new Runnable() { public void run() {
        String key = null;
        try {
          while ((key = tasks.poll()) != null && error.get() == null) {
            boolean ok = gcsService.delete(new GcsFilename("cl-test-grid-logs", key));
            log.fine("delete " + key + ": " + ok);
          }
        } catch (Throwable t) {
          log.log(Level.SEVERE, "Error deleting blob " + key, t);
          error.set(new RuntimeException("Error in delete worker thread while deleting blob " + key, t));
        } finally {
          doneLatch.countDown();
        }
      }}).start();
    }

    try {
      doneLatch.await();
    } catch (InterruptedException e) {
      throw new RuntimeException("Interrupted while saving blobs", e);
    }

    if (error.get() != null) {
      throw error.get();
    }
  }

  private static final Pattern KEY_SEPARATORS = Pattern.compile("[\\n\\r,]+");

  private List<String> parseKeys(String keys) {
    if (null == keys || keys.length() < 1) {
      throw new RuntimeException("keys parameter must not be empty"); // todo: use BadRequestException
    }
    return Arrays.asList(KEY_SEPARATORS.split(keys));
  }
}
