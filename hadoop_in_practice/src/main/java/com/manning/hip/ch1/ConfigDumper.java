package com.manning.hip.ch1;

public final class ConfigDumper {
  public static void main(String... args) throws Exception {
    System.out.println(new ConfigComparer("core-default.xml", "core-site.xml").multiLineOutput());
    System.out.println(new ConfigComparer("hdfs-default.xml", "hdfs-site.xml").multiLineOutput());
    System.out.println(new ConfigComparer("mapred-default.xml", "mapred-site.xml").multiLineOutput());
  }
}
