package com.manning.hip.ch3.binary;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IOUtils;

import java.io.*;
import java.util.Random;

public class CustomBinaryFileRead {

  public static void write(InputStream inputStream)
      throws IOException {
    DataInputStream is = new DataInputStream(inputStream);

    int pos = 0;
    try {
      while(true) {
        int len = is.readInt();
        System.out.println("New data record at " + pos + " len " + len);
        for(int i=0; i < len; i++) {
          byte b = is.readByte();
          if(b != len) {
            System.err.println("Found bad byte: " + b);
            break;
          }
        }
        pos += len + 4;
      }
    } catch(EOFException e) {}
    IOUtils.closeStream(inputStream);
  }

  public static void main(String... args) throws Exception {
    Configuration config = new Configuration();
    FileSystem hdfs = FileSystem.get(config);

    Path file = new Path(args[0]);

    InputStream os = hdfs.open(file);
    write(os);
  }
}
