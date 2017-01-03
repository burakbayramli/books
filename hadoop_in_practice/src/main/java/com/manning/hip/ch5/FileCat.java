package com.manning.hip.ch5;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.io.IOUtils;

import java.io.InputStream;

public class FileCat {
  public static void main(String... args) throws Exception {
    Configuration config = new Configuration();
    FileSystem hdfs = FileSystem.get(config);

    InputStream is = hdfs.open(new Path(args[0]));     //<co id="ch02_hdfs_reads_java_comment1"/>
    IOUtils.copyBytes(is, System.out, config, false);  //<co id="ch02_hdfs_reads_java_comment2"/>
    is.close();
  }
}
