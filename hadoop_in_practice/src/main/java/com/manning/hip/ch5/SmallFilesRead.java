package com.manning.hip.ch5;

import org.apache.avro.file.DataFileStream;
import org.apache.avro.generic.*;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.io.IOUtils;

import java.io.*;
import java.nio.ByteBuffer;

public class SmallFilesRead {

  private static final String FIELD_FILENAME = "filename";
  private static final String FIELD_CONTENTS = "contents";

  public static void readFromAvro(InputStream is) throws IOException {
    DataFileStream<Object> reader =                   //<co id="ch02_smallfileread_comment1"/>
        new DataFileStream<Object>(
            is, new GenericDatumReader<Object>());
    for (Object o : reader) {                         //<co id="ch02_smallfileread_comment2"/>
      GenericRecord r = (GenericRecord) o;            //<co id="ch02_smallfileread_comment3"/>
      System.out.println(                             //<co id="ch02_smallfileread_comment4"/>
          r.get(FIELD_FILENAME) +
              ": " +
              DigestUtils.md5Hex(
                  ((ByteBuffer) r.get(FIELD_CONTENTS)).array()));
    }
    IOUtils.cleanup(null, is);
    IOUtils.cleanup(null, reader);
  }

  public static void main(String... args) throws Exception {
    Configuration config = new Configuration();
    FileSystem hdfs = FileSystem.get(config);

    Path destFile = new Path(args[0]);

    InputStream is = hdfs.open(destFile);
    readFromAvro(is);
  }
}
