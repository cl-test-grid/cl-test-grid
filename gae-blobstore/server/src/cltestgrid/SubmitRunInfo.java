/* Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
 * See LICENSE for details.
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
import javax.mail.Multipart;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMultipart;

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
    resp.setContentType("text/plain; charset=utf-8");

    logger.log(Level.INFO, 
        "Handling the run-info submit from " + req.getRemoteAddr() 
        + "; X-AppEngine-Country: " + req.getHeader("X-AppEngine-Country"));
    
    Properties props = new Properties();
    Session session = Session.getDefaultInstance(props, null);

    String runInfo = req.getParameter("run-info");

    try {
      if (null == runInfo || runInfo.length() == 0) {
        throw new Exception("The \"run-info\" parameter is missed");
      }

      if (logger.isLoggable(Level.FINE)) {
        logger.log(Level.FINE, "run-info: " + runInfo);
      }

      Message msg = new MimeMessage(session);
      msg.setFrom(new InternetAddress("cl.test.grid@gmail.com", "cl-test-grid GAE server"));
      msg.addRecipient(Message.RecipientType.TO,
                       new InternetAddress("avodonosov@yandex.ru", "Anton Vodonosov"));
      msg.addRecipient(Message.RecipientType.TO,
                       new InternetAddress("cl-test-grid@yandex.ru", "cl-test-grid results inbox"));
      msg.setSubject("[cl-test-grid] [test run submitted]");

      Multipart multipart = new MimeMultipart();

      // Set the email message text.
      MimeBodyPart messagePart = new MimeBodyPart();
      String runInfoBeginning = runInfo.substring(0, Math.min(runInfo.length(), 300));
      messagePart.setText("See attach. The first 300 characters:\r\n\r\n" + runInfoBeginning);
      multipart.addBodyPart(messagePart);

      // Attach the test run info.
      MimeBodyPart attachmentPart = new MimeBodyPart();
      attachmentPart.setFileName("test-run-info.lisp");
      attachmentPart.setText(runInfo, "UTF-8");
      multipart.addBodyPart(attachmentPart);

      msg.setContent(multipart);
    
      Transport.send(msg);
    
      logger.log(Level.INFO, "The run-info successfully received and resend further by email.");
      resp.getWriter().println(":ok");

    } catch (Exception e) {
      logger.log(Level.SEVERE, "Error sending the email", e);
      resp.getWriter().println(":error " + e.getMessage());
    }
  }
}
