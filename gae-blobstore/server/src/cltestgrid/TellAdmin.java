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

public class TellAdmin extends HttpServlet {

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
        "Handling the tell-admin submit from " + req.getRemoteAddr() 
        + "; X-AppEngine-Country: " + req.getHeader("X-AppEngine-Country"));
    
    Properties props = new Properties();
    Session session = Session.getDefaultInstance(props, null);

    String subject = req.getParameter("subject");
    String body = req.getParameter("body");

    try {
      if (null == subject || subject.length() == 0) {
        throw new Exception("The \"subject\" parameter is missed");
      }
      if (null == body || body.length() == 0) {
        throw new Exception("The \"body\" parameter is missed");
      }

      if (logger.isLoggable(Level.FINE)) {
        logger.log(Level.FINE, "subject: " + subject);
        logger.log(Level.FINE, "body: " + body);
      }

      Message msg = new MimeMessage(session);
      msg.setFrom(new InternetAddress("cl.test.grid@gmail.com", "cl-test-grid GAE server"));
      msg.addRecipient(Message.RecipientType.TO,
                       new InternetAddress("avodonosov@yandex.ru", "Anton Vodonosov"));
      msg.setSubject("[cl-test-grid] " + subject);

      Multipart multipart = new MimeMultipart();

      // Set the email message text.
      MimeBodyPart messagePart = new MimeBodyPart();
      messagePart.setText(body);
      multipart.addBodyPart(messagePart);

      msg.setContent(multipart);
    
      Transport.send(msg);
    
      logger.log(Level.INFO, "Send message to admin successfuly.");
      resp.getWriter().println(":ok");

    } catch (Exception e) {
      logger.log(Level.SEVERE, "Error sending the email", e);
      resp.getWriter().println(":error " + e.getMessage());
    }
  }
}
