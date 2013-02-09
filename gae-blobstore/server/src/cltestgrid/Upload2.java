/* Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
 * See LICENSE for details.
 */
package cltestgrid;

import java.io.PrintWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.ByteArrayOutputStream;
import java.io.FilterOutputStream;
import java.io.File;
import java.io.UnsupportedEncodingException;
import java.io.StringWriter;
import java.io.PrintWriter;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Collections;
import java.nio.channels.Channels;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.Random;
import java.security.SecureRandom;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.google.appengine.api.files.AppEngineFile;
import com.google.appengine.api.files.FileReadChannel;
import com.google.appengine.api.files.FileService;
import com.google.appengine.api.files.FileServiceFactory;
import com.google.appengine.api.files.FileWriteChannel;
import com.google.appengine.api.files.GSFileOptions.GSFileOptionsBuilder;
import com.google.appengine.api.ThreadManager;

import org.apache.commons.fileupload.*;

public class Upload2 extends HttpServlet {

  private static final Logger log = Logger.getLogger(Upload2.class.getName());

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
    try {

      /* During migration we wanted to upload logs under their old names.
      // Use req.getQueryString instead of req.getParameters, as it mutst be a POST
      // request, and we want to parse the request body ourselves.
      final boolean keepNames = (req.getQueryString() + "").contains("keepNames=true");
      */

      final boolean keepNames = false;
      Map<String, String> map = saveBlobs(req, keepNames);

      String result = asAssocList(map);
      log.info("returning blobName-blobKey map: " + result);
      resp.getWriter().write(result);
    } catch (BadRequestException e) {
      log.log(Level.SEVERE, "Bad request, returning status 400", e);
      resp.sendError(HttpServletResponse.SC_BAD_REQUEST, e.getMessage());
    }
  }

  private static class BadRequestException extends RuntimeException {
    public BadRequestException(String message) {
      super(message);
    }
  }

  /** Formats a Map so that it can be read by Lisp readers as an association list.
    */
  private static String asAssocList(Map<String, String> map) {
    StringWriter buf = new StringWriter();
    PrintWriter out = new PrintWriter(buf);
    out.println("(");
    for (String key : map.keySet()) {
      String val = map.get(key);
      out.println(" (\"" + key + "\" . \"" + val + "\")");
    }
    out.println(")");
    return buf.toString();
  }

  /*
    Storing files submitted via HTTP multipart/form-data to Google Cloud Storage.

    Handling of multipart/form-data is based on Apache Commons FileUpload,
    because Google App Engine does not support servlet API 3.0 yet.

    But Commons FileUpload has code which can not be used in Google App Engine - 
    it tries to save files on temporary file system if the files are too
    big to store them in memory. Google App Engine does not allow to store
    data on file system, therefore the corresponding FileUpload code fails.

    We hook into the Commons FileUpload implementation and provide
    an OutputStream for each file uploaded, and the FileUpload saves
    the file content to our OutputStream.

    The OutputStream we provide for every file could be one of:
    1. A stream writting directly to a CloudStorage blob.
       Unfortunatelly, saving to CloudStorage is quite slow, 
       and if we save the files one by one, the request handling
       takes too long and exceedes the 30 seconds timeout. In result
       Google App Engine kills our servlet. This happens even
       if the submit consists of just 10-15 files.
    2. Therefore we provide a temporary ByteArrayOutputStream
       for every file. This allows us to quickly parse the HTTP request
       into separate files, and then start multiple therads writting
       the files into CloudStorage. This scheme allows to handle 300-500
       files submitted in one request.
       As we accumulate all the files in memory, we impose a limit on
       the file size and total number of files submitted in one request.

    ---------------------------------

    Hooking into FileUpload is done by suppying our custom FileItem implementation -
    MyFileItem. This is done via our custom factory - MyFileItemFactory. 

  */

  public static final int MAX_FILE_LEN = 50000;
  public static final int MAX_FILES = 500;

  /** 
      Returns a map from fieldNames of the submitted files to
      blobNames of these files stored on CloudStorage.
   */
  private Map<String, String> saveBlobs(HttpServletRequest req, boolean keepNames) throws IOException, ServletException {
    List<MyFileItem> items = parseRequest(req);

    for (MyFileItem item : items) {
      if (keepNames) {
        item.blobName = item.fileName;
      } else {
        item.blobName = generateId();
      }
    }

    saveBlobsInParallel(items);

    Map<String, String> map = new HashMap<String, String>();
    for (MyFileItem item : items) {
      // assert !item.isFormField()
      if (item.saveError != null) {
        throw new RuntimeException("Error signalled by saveBlob worker thread", item.saveError);
      }
      map.put(item.fieldName, item.blobName);
    }

    return map;
  }

  private List<MyFileItem> parseRequest(HttpServletRequest req) throws RuntimeException {
    try {
      if (isMultipartContent(req)) {    
        FileItemFactory factory = new MyFileItemFactory();
        FileUpload upload = new FileUpload(factory);
        return (List<MyFileItem>)upload.parseRequest(req);
      } else {
        log.warning("Non multipart request");
        return Collections.<MyFileItem>emptyList();
      }
    } catch (FileUploadException e) {
      throw new RuntimeException("Error parsing multipart request body", e);
    }
  }

  private void saveBlobsInParallel(List<MyFileItem> items) {
    final int THREAD_COUNT = 20;

    final ConcurrentLinkedQueue<MyFileItem> tasks = new ConcurrentLinkedQueue<MyFileItem>(items);
    final CountDownLatch doneLatch = new CountDownLatch(THREAD_COUNT);

    for (int i = 0; i < THREAD_COUNT; i++) {
      ThreadManager.createThreadForCurrentRequest(new Runnable() { public void run() {
        MyFileItem item = null;
        try {
          while ((item = tasks.poll()) != null) {
            try {
              saveBlob(item.blobName, item.contentType, item.dataCollector.toByteArray());
              // Saving blob may throw a LockException due to CloudStorage issue
              // http://code.google.com/p/googleappengine/issues/detail?id=8592
              // Therefore retry two times in case of LockException:
            } catch (com.google.appengine.api.files.LockException e) {                
              try {
                log.log(Level.WARNING, "retry saving blob " + item.blobName + " because of LockException when saving it first time", e);
                saveBlob(item.blobName, item.contentType, item.dataCollector.toByteArray());
              } catch (com.google.appengine.api.files.LockException e2) {
                log.log(Level.WARNING, "second retry saving blob " + item.blobName + " because of LockException when saving it at first retry", e2);
                saveBlob(item.blobName, item.contentType, item.dataCollector.toByteArray());
              }
            }
          }
        } catch (Throwable t) {
          if (item != null) {item.saveError = t;}
          log.log(Level.SEVERE, "Error while saving blob", t);
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
  }

  private static final boolean isMultipartContent(HttpServletRequest request) {
    if (!"post".equals(request.getMethod().toLowerCase())) {
        return false;
    }
    String contentType = request.getContentType();
    return contentType != null
            && contentType.toLowerCase().startsWith("multipart");
  }

  private static class MyFileItem implements FileItem {

    // unnecessary methods - the FileUpload API part I don't need
    public InputStream getInputStream() throws IOException {return null;}
    public String getContentType() {return null;}
    public String getName() {return null;}
    public boolean isInMemory() {return false;}
    public long getSize() {return 0;}
    public byte[] get() {return null;}
    public String getString(String encoding) throws UnsupportedEncodingException {return null;}
    public String getString() {return null;}
    public void write(File file) throws Exception {}
    public void delete() {}
    public String getFieldName() {return null;}
    public void setFieldName(String name) {}
    public void setFormField(boolean state) {}
    public boolean isFormField() {return false;}
    // ----------------------

    // the file data received from request
    final String fieldName, fileName, contentType;
    final LimitingDataCollector dataCollector = new LimitingDataCollector();

    // results of storing the blob
    volatile String blobName = null;
    volatile Throwable saveError = null;

    public MyFileItem(String fieldName, String fileName, String contentType) {
      this.fieldName = fieldName;
      this.fileName = fileName;
      this.contentType = contentType;
    }

    public OutputStream getOutputStream() {return this.dataCollector;}
  }

  private static class LimitingDataCollector extends ThresholdingOutputStream {

    final ByteArrayOutputStream stream = new ByteArrayOutputStream(1500);

    public LimitingDataCollector() {
      super(MAX_FILE_LEN);
    }

    @Override protected OutputStream getStream() {
      return stream;
    }

    @Override protected void thresholdReached() {
      throw new BadRequestException("Maximum file size to upload is " + MAX_FILE_LEN + " bytes");
    }

    public byte[] toByteArray() {
      return stream.toByteArray();
    }
  }

  private static class MyFileItemFactory implements FileItemFactory {

    private int filesSeen = 0;
    public FileItem createItem(String fieldName,
                               String contentType,
                               boolean isFormField,
                               String fileName)
    { 
      log.info("blob to save: fileName: " + fileName + "; fieldName: " + fieldName + "; contentType: " + contentType);
      if (isFormField) throw new BadRequestException("This servlet only accepts files, support for usual form fields is not implemented.");

      this.filesSeen++;
      if (this.filesSeen > MAX_FILES) {
        throw new BadRequestException("Maximum nuber of files in one request is " + MAX_FILES);
      }
      
      return new MyFileItem(fieldName, fileName, contentType);
    }
  }
  
  private static void saveBlob(String blobName, String contentType, byte[] data) throws IOException {
    log.info("saving blob " + blobName);
    FileWriteChannel blobChannel = newBlobChannel(blobName, contentType);
    
    OutputStream ostream = Channels.newOutputStream(blobChannel);
    ostream.write(data);
    ostream.flush();
    blobChannel.closeFinally();
  }

  private static FileWriteChannel newBlobChannel(String blobName, String contentType) throws IOException {
    FileService fileService = FileServiceFactory.getFileService();

    GSFileOptionsBuilder optionsBuilder = new GSFileOptionsBuilder()
      .setBucket("cl-test-grid-logs")
      .setKey(blobName)
      .setAcl("public-read")
      .setContentEncoding("gzip");
    if (contentType != null) {
      optionsBuilder.setMimeType(contentType);
    }

    AppEngineFile writableFile = fileService.createNewGSFile(optionsBuilder.build());

    boolean lockForWrite = true; // We want to lock it, because we are going to call closeFinally in the end
    return fileService.openWriteChannel(writableFile, lockForWrite);
  }

  /* ======== ID Generation ========= */

  private static final Random random = new Random(asLong(SecureRandom.getSeed(8)));

  private static long asLong(byte[] bytes) {
    long l = bytes[0];
    for (int i = 1; i < 8; i++) {
      l = ((l << 8) | (bytes[i] & 0xff));
    }
    return l;
  }

  private static String generateId() {
    final int ID_LEN = 10;

    long l = Math.abs(random.nextLong());
    String str = Long.toString(l, Character.MAX_RADIX);
    if (str.length() > ID_LEN) {
      return str.substring(0, ID_LEN);
    }
    if (str.length() < ID_LEN) {
      StringBuilder padding = new StringBuilder(ID_LEN - str.length());
      for (int i = str.length(); i < ID_LEN; i++) {
        padding.append('0');
      }
      return padding.toString() + str;
    }
    return str;
  }
}