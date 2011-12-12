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
import java.util.logging.Level;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import java.util.Properties;

import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

public class SubmitRunInfo extends HttpServlet {

  private static final Logger logger = Logger.getLogger(Upload.class.getName());

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

    logger.log(Level.INFO, "Handling the run-info submit from " + req.getRemoteAddr());
    
    Properties props = new Properties();
    Session session = Session.getDefaultInstance(props, null);

    String runInfo = req.getParameter("run-info");

    try {
      if (null == runInfo || runInfo.length() == 0) {
        throw new Exception("The \"run-info\" parameter is missed");
      }

      Message msg = new MimeMessage(session);
      msg.setFrom(new InternetAddress("cl.test.grid@gmail.com", "cl-test-grid GAE server"));
      msg.addRecipient(Message.RecipientType.TO,
                       new InternetAddress("avodonosov@yandex.ru", "Anton Vodonosov"));
      msg.setSubject("[cl-test-grid] [test run submitted]");
      msg.setText(runInfo);
      Transport.send(msg);
    
      logger.log(Level.INFO, "The run-info successfully received and resend further by email.");
      resp.getWriter().println(":ok");

    } catch (Exception e) {
      logger.log(Level.SEVERE, "Error sending the email", e);
      resp.getWriter().println(":error " + e.getMessage());
    }
  }
}
