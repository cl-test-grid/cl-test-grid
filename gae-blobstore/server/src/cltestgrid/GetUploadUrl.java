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

    String uploadURL = blobstoreService.createUploadUrl("/upload");
    logger.info("returning new uploadURL: " + uploadURL);

    resp.getWriter().print(uploadURL);
  }
}
