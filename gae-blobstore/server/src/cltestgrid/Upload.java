/* Copyright (c) 2009 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package cltestgrid;

import java.io.IOException;
import java.io.StringWriter;
import java.io.PrintWriter;
import java.util.Map;
import java.util.Iterator;
import java.util.logging.Logger;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.google.appengine.api.blobstore.BlobstoreServiceFactory;
import com.google.appengine.api.blobstore.BlobstoreService;
import com.google.appengine.api.blobstore.BlobKey;

import com.google.appengine.api.datastore.DatastoreService;
import com.google.appengine.api.datastore.DatastoreServiceFactory;
import com.google.appengine.api.datastore.Entity;

@SuppressWarnings("serial")
public class Upload extends HttpServlet {

  private static final Logger log = Logger.getLogger(Upload.class.getName());

  private BlobstoreService blobstoreService =
    BlobstoreServiceFactory.getBlobstoreService();

  private DatastoreService datastore = DatastoreServiceFactory.getDatastoreService();

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
    resp.setContentType("text/plain; charset=utf-8");

    Map<String, BlobKey> blobs = blobstoreService.getUploadedBlobs(req);

    StringWriter buf = new StringWriter();
    PrintWriter out = new PrintWriter(buf);

    out.println("(");
    Iterator<String> names = blobs.keySet().iterator();
    while (names.hasNext()) {
        String blobName = names.next();
        BlobKey blobKey = blobs.get(blobName);

        // Instead of returning the BlobKey string, which is of 162 character long,
        // return a shorter key. 
        // We acheave this by creating a datastore Entity storing the original BlobKey,
        // and returning the Entity numeric ID to the client as the blob key.
        Entity shortKeyEntity = new Entity("ShortKey");
        shortKeyEntity.setProperty("blobKey", blobKey);
        datastore.put(shortKeyEntity); 

        out.println(" (\"" + blobName + "\" . \"" + shortKeyEntity.getKey().getId() + "\")");
    }
    out.println(")");

    String result = buf.toString();
    log.info("returning blobName-blobKey map: " + result);
    resp.getWriter().write(result);
  }
}
