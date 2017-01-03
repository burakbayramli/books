package com.manning.hip.ch5;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IOUtils;
import org.apache.hadoop.io.compress.CompressionCodec;
import org.apache.hadoop.util.ReflectionUtils;

import java.io.InputStream;

public class CompressedFileRead {
  public static void main(String... args) throws Exception {
    Configuration config = new Configuration();
    FileSystem hdfs = FileSystem.get(config);

    InputStream is = hdfs.open(new Path(args[0]));

    Class<?> codecClass = Class.forName(args[1]);
    CompressionCodec codec = (CompressionCodec)
        ReflectionUtils.newInstance(codecClass, config);

    InputStream cis = codec.createInputStream(is);

    IOUtils.copyBytes(cis, System.out, config, true);

    IOUtils.closeStream(is);
  }
}
