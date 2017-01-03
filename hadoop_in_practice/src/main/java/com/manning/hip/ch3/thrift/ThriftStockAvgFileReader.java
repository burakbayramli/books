package com.manning.hip.ch3.thrift;

import com.hadoop.compression.lzo.LzopCodec;
import com.twitter.elephantbird.mapreduce.io.*;
import com.twitter.elephantbird.util.TypeRef;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.io.IOUtils;

import java.io.*;

public class ThriftStockAvgFileReader {

  public static void readFromProtoBuf(InputStream inputStream)
      throws IOException {

    ThriftBlockReader<StockAvg> reader =
        new ThriftBlockReader<StockAvg>(
            inputStream, new TypeRef<StockAvg>() {});

    StockAvg stock;
    while((stock = reader.readNext()) != null) {
      System.out.println(ToStringBuilder.reflectionToString(stock));

    }

    IOUtils.closeStream(inputStream);
  }

  public static void main(String... args) throws Exception {
    Configuration config = new Configuration();
    FileSystem hdfs = FileSystem.get(config);

    Path destFile = new Path(args[0]);

    LzopCodec codec = new LzopCodec();
    codec.setConf(config);
    InputStream is = codec.createInputStream(hdfs.open(destFile));

    readFromProtoBuf(is);
  }
}
