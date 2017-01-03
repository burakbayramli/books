package com.manning.hip.common;

import org.apache.commons.lang.StringUtils;
import org.apache.hadoop.io.Text;

import java.io.IOException;
import java.text.SimpleDateFormat;

public class ApacheCommonLogReader {
  private final ApacheCommonLogParser parser = new ApacheCommonLogParser();
  private final SimpleDateFormat sdf =
  new SimpleDateFormat("dd/MMM/yyyy:hh:mm:ss Z");

  public CommonLogEntry decodeLine(Text line) throws IOException {
    if(line == null) {
      return null;
    }
    return decodeLine(line.toString());
  }

  public CommonLogEntry decodeLine(String line) throws IOException {
    CommonLogEntry e = new CommonLogEntry();

    String parts[] = parser.parseLine(line);

    if (parts == null || parts.length != 8) {
      return null;
    }

    e.setRemoteAddress(getAsString(parts[0]));
    e.setRemoteLogname(getAsString(parts[1]));
    e.setUserId(getAsString(parts[2]));
    e.setTime(getAsString(parts[3] + " " + parts[4]));
    e.setRequestLine(getAsString(parts[5]));
    e.setStatusCode(getAsLong(parts[6]));
    e.setObjSize(getAsLong(parts[7]));

    if (e.getRequestLine() != null) {
      String[] requestParts = e.getRequestLine().split(" ");
      e.setMethod(requestParts[0]);
      e.setResource(requestParts[1]);
      e.setProtocol(requestParts[2]);
    }

    // epoch
    String trimmedDate = e.getTime()
      .substring(1, e.getTime().length() - 1);

    try {
      e.setEpoch(sdf.parse(trimmedDate).getTime());
    } catch (java.text.ParseException e1) {
    }
    return e;
  }

  public static boolean isNull(String part) {
    return StringUtils.isEmpty(part) || "-".equals(part);
  }

  public static String getAsString(String part) {
    if (isNull(part)) {
      return null;
    }
    return part;
  }

  public static Long getAsLong(String part) {
    if (isNull(part)) {
      return null;
    }
    return Long.valueOf(part);
  }

}
