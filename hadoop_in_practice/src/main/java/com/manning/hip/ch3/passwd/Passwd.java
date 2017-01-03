package com.manning.hip.ch3.passwd;

import org.apache.commons.lang.builder.CompareToBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.hadoop.io.WritableComparable;
import org.apache.hadoop.io.WritableUtils;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;

public class Passwd implements WritableComparable<Passwd>, Cloneable  {
  String username;
  String password;
  Long uid;
  Long gid;
  String uidInfo;
  String homeDir;
  String shell;

  public Passwd() {
  }

  public Passwd(String username,
                String password,
                Long uid,
                Long gid,
                String uidInfo,
                String homeDir,
                String shell) {
    this.username = username;
    this.password = password;
    this.uid = uid;
    this.gid = gid;
    this.uidInfo = uidInfo;
    this.homeDir = homeDir;
    this.shell = shell;
  }

  @Override
  public void write(DataOutput out) throws IOException {
    WritableUtils.writeString(out, username);
    WritableUtils.writeString(out, password);
    writeLong(out, uid);
    writeLong(out, gid);
    WritableUtils.writeString(out, uidInfo);
    WritableUtils.writeString(out, homeDir);
    WritableUtils.writeString(out, shell);
 }

  @Override
  public void readFields(DataInput in) throws IOException {
    username = WritableUtils.readString(in);
    password = WritableUtils.readString(in);
    uid = readLong(in);
    gid = readLong(in);
    uidInfo = WritableUtils.readString(in);
    homeDir = WritableUtils.readString(in);
    shell = WritableUtils.readString(in);
  }

  public static void writeLong(DataOutput out, Long i)
      throws IOException {
    // WritableUtils only support primivite types
    out.writeBoolean(i != null);
    if (i != null) {
      WritableUtils.writeVLong(out, i);
    }
  }

  public static Long readLong(DataInput in)
      throws IOException {
    Long i = null;
    if(in.readBoolean()) {
      i = WritableUtils.readVLong(in);
    }
    return i;
  }

  @Override
  public int compareTo(Passwd passwd) {
    return CompareToBuilder.reflectionCompare(this, passwd);
  }

  @Override
  public String toString() {
    return ToStringBuilder.reflectionToString(this);
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public String getPassword() {
    return password;
  }

  public void setPassword(String password) {
    this.password = password;
  }

  public Long getUid() {
    return uid;
  }

  public void setUid(Long uid) {
    this.uid = uid;
  }

  public Long getGid() {
    return gid;
  }

  public void setGid(Long gid) {
    this.gid = gid;
  }

  public String getUidInfo() {
    return uidInfo;
  }

  public void setUidInfo(String uidInfo) {
    this.uidInfo = uidInfo;
  }

  public String getHomeDir() {
    return homeDir;
  }

  public void setHomeDir(String homeDir) {
    this.homeDir = homeDir;
  }

  public String getShell() {
    return shell;
  }

  public void setShell(String shell) {
    this.shell = shell;
  }
}
