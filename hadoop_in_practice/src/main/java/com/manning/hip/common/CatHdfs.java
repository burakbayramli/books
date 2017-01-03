package com.manning.hip.common;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IOUtils;

import java.io.InputStream;
import java.io.OutputStream;

public class CatHdfs {
  public static void main(String... args) throws Exception {
    Configuration config = new Configuration();

    FileSystem hdfs = FileSystem.get(config);

    InputStream is = hdfs.open(new Path(args[0]));

    IOUtils.copyBytes(is, System.out, config, true);

    IOUtils.closeStream(is);
  }
}
