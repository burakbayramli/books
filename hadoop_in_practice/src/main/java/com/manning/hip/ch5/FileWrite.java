package com.manning.hip.ch5;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.io.IOUtils;

import java.io.*;

public class FileWrite {
  public static void main(String... args) throws Exception {
    Configuration config = new Configuration();       //<co id="ch02_hdfs_writes_java_comment1"/>
    FileSystem hdfs = FileSystem.get(config);         //<co id="ch02_hdfs_writes_java_comment2"/>

    InputStream is = new FileInputStream(args[0]);    //<co id="ch02_hdfs_writes_java_comment3"/>
    OutputStream os = hdfs.create(new Path(args[1])); //<co id="ch02_hdfs_writes_java_comment4"/>
    IOUtils.copyBytes(is, os, config, true);          //<co id="ch02_hdfs_writes_java_comment5"/>
  }
}
