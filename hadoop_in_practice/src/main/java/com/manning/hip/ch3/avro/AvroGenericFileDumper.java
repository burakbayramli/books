package com.manning.hip.ch3.avro;

import org.apache.avro.file.DataFileStream;
import org.apache.avro.generic.GenericDatumReader;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.io.IOUtils;

import java.io.*;

public class AvroGenericFileDumper {

  public static void readFromAvro(InputStream is) throws IOException {
    DataFileStream<Object> reader =
        new DataFileStream<Object>(
            is, new GenericDatumReader<Object>());
    for (Object o : reader) {
      System.out.println(o);
    }
    IOUtils.closeStream(is);
    IOUtils.closeStream(reader);
  }

  public static void main(String... args) throws Exception {
    Configuration config = new Configuration();
    FileSystem hdfs = FileSystem.get(config);

    Path destFile = new Path(args[0]);

    InputStream is = hdfs.open(destFile);
    readFromAvro(is);
  }
}
