package com.manning.hip.ch3.binary;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IOUtils;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Random;

public class CustomBinaryFileWrite {

  public static void write(OutputStream outputStream)
      throws IOException {
    DataOutputStream os = new DataOutputStream(outputStream);

    Random r = new Random();
    for(int i=0; i < 3; i++) {
      int len = r.nextInt(Byte.MAX_VALUE) + 1;
      System.out.println("Writing data length " + len);
      os.writeInt(len);
      for(int b=0; b < len; b++) {
        os.writeByte(len);
      }
    }
    IOUtils.closeStream(outputStream);
  }

  public static void main(String... args) throws Exception {
    Configuration config = new Configuration();
    FileSystem hdfs = FileSystem.get(config);

    Path destFile = new Path(args[0]);

    OutputStream os = hdfs.create(destFile);
    write(os);
  }
}
