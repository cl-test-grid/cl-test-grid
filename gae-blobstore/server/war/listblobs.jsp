<%@ page import="com.google.appengine.api.blobstore.BlobInfo" %>
<%@ page import="com.google.appengine.api.blobstore.BlobInfoFactory" %>
<%@ page import="java.util.Iterator" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN"
  "http://www.w3.org/TR/html4/strict.dtd">

<html lang="en">
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    <title>Blobs - CL Test Grid</title>
  </head>
  <body>

<h3>CL Test Grid Blobs</h3>
  <ol>
<%
  BlobInfoFactory blobInfoFactory = new BlobInfoFactory();
  Iterator<BlobInfo> blobInfos = blobInfoFactory.queryBlobInfos(); 
  while (blobInfos.hasNext()) {

%>
    <li><%=blobInfos.next()%></li>
<%
  }
%>
  </ol>
  </body>
</html>
