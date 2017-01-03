package com.manning.hip.ch3.proto;

import com.hadoop.compression.lzo.LzopCodec;
import com.twitter.elephantbird.mapreduce.io.ProtobufBlockReader;
import com.twitter.elephantbird.util.TypeRef;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IOUtils;

import java.io.IOException;
import java.io.InputStream;

import static com.manning.hip.ch3.proto.StockProtos.Stock;

public class StockProtocolBuffersFileReader {

  public static void readFromProtoBuf(InputStream inputStream)
      throws IOException {

    ProtobufBlockReader<Stock> reader =
        new ProtobufBlockReader<Stock>(
            inputStream, new TypeRef<Stock>() {});

    Stock stock;
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
