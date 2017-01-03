package com.manning.hip.common;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IOUtils;

import java.io.OutputStream;

public class StreamToHdfs {
  public static void main(String... args) throws Exception {
    Configuration config = new Configuration();

    FileSystem hdfs = FileSystem.get(config);

    OutputStream os = hdfs.create(new Path(args[0]));

    IOUtils.copyBytes(System.in, os, config, true);

    IOUtils.closeStream(os);
  }
}
