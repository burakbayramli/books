package com.manning.hip.common;

import org.apache.hadoop.io.Writable;
import org.apache.hadoop.io.WritableUtils;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;

/**
 */
public class CommonLogEntry implements Writable {
  private String remoteAddress;
  private String remoteLogname;
  private String userId;
  private String time;
  private String requestLine;
  private Long statusCode;
  private Long objSize;
  private String method;
  private String resource;
  private String protocol;
  private Long epoch;

  @Override
  public void write(DataOutput out) throws IOException {
    WritableUtils.writeString(out, remoteAddress);
    WritableUtils.writeString(out, remoteLogname);
    WritableUtils.writeString(out, userId);
    WritableUtils.writeString(out, time);
    WritableUtils.writeString(out, requestLine);
    writeLong(out, statusCode);
    writeLong(out, objSize);
    WritableUtils.writeString(out, method);
    WritableUtils.writeString(out, resource);
    WritableUtils.writeString(out, protocol);
    writeLong(out, epoch);
  }

  @Override
  public void readFields(DataInput in) throws IOException {
    remoteAddress = WritableUtils.readString(in);
    remoteLogname = WritableUtils.readString(in);
    userId = WritableUtils.readString(in);
    time = WritableUtils.readString(in);
    requestLine = WritableUtils.readString(in);
    statusCode = readLong(in);
    objSize = readLong(in);
    method = WritableUtils.readString(in);
    resource = WritableUtils.readString(in);
    protocol = WritableUtils.readString(in);
    epoch = readLong(in);
  }

  public static void writeLong(DataOutput out, Long l) throws IOException {
    if (l != null) {
      out.writeInt(1);
      out.writeLong(l);
    } else {
      out.writeInt(-1);
    }
  }

  public static Long readLong(DataInput in) throws IOException {
    int length = in.readInt();
    if (length == -1) return null;
    return in.readLong();
  }


  public String getRemoteAddress() {
    return remoteAddress;
  }

  public void setRemoteAddress(String remoteAddress) {
    this.remoteAddress = remoteAddress;
  }

  public String getRemoteLogname() {
    return remoteLogname;
  }

  public void setRemoteLogname(String remoteLogname) {
    this.remoteLogname = remoteLogname;
  }

  public String getUserId() {
    return userId;
  }

  public void setUserId(String userId) {
    this.userId = userId;
  }

  public String getTime() {
    return time;
  }

  public void setTime(String time) {
    this.time = time;
  }

  public String getRequestLine() {
    return requestLine;
  }

  public void setRequestLine(String requestLine) {
    this.requestLine = requestLine;
  }

  public Long getStatusCode() {
    return statusCode;
  }

  public void setStatusCode(Long statusCode) {
    this.statusCode = statusCode;
  }

  public Long getObjSize() {
    return objSize;
  }

  public void setObjSize(Long objSize) {
    this.objSize = objSize;
  }

  public String getMethod() {
    return method;
  }

  public void setMethod(String method) {
    this.method = method;
  }

  public String getResource() {
    return resource;
  }

  public void setResource(String resource) {
    this.resource = resource;
  }

  public String getProtocol() {
    return protocol;
  }

  public void setProtocol(String protocol) {
    this.protocol = protocol;
  }

  public Long getEpoch() {
    return epoch;
  }

  public void setEpoch(Long epoch) {
    this.epoch = epoch;
  }

}
