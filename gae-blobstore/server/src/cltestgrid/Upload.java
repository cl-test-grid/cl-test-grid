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

@SuppressWarnings("serial")
public class Upload extends HttpServlet {

  private static final Logger log = Logger.getLogger(Upload.class.getName());

  private BlobstoreService blobstoreService =
    BlobstoreServiceFactory.getBlobstoreService();

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
    resp.setContentType("text/html; charset=utf-8");

    Map<String, BlobKey> blobs = blobstoreService.getUploadedBlobs(req);

    resp.getWriter().println("(");

    Iterator<String> names = blobs.keySet().iterator();
    while (names.hasNext()) {
        String blobName = names.next();
        BlobKey blobKey = blobs.get(blobName);
        resp.getWriter().println(" (\"" + blobName + "\" . \"" + blobKey.getKeyString() + "\")");
    }

    resp.getWriter().println(")");
  }
}
