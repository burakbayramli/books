package com.manning.hip.ch2;

import com.manning.hip.ch3.avro.gen.Stock;
import org.apache.commons.lang.builder.*;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.hbase.client.Result;
import org.apache.hadoop.hbase.io.ImmutableBytesWritable;
import org.apache.hadoop.io.SequenceFile;

import java.io.IOException;

import static com.manning.hip.ch2.HBaseWriteAvroStock.*;


public class HBaseExportedStockReader {
  public static void main(String... args) throws IOException {
    read(new Path(args[0]));
  }

  public static void read(Path inputPath) throws IOException {
    Configuration conf = new Configuration();
    FileSystem fs = FileSystem.get(conf);

    SequenceFile.Reader reader =
        new SequenceFile.Reader(fs, inputPath, conf);

    HBaseScanAvroStock.AvroStockReader stockReader =
        new HBaseScanAvroStock.AvroStockReader();

    try {
      System.out.println(
          "Is block compressed = " + reader.isBlockCompressed());

      ImmutableBytesWritable key = new ImmutableBytesWritable();
      Result value = new Result();

      while (reader.next(key, value)) {
        Stock stock = stockReader.decode(value.getValue(
            STOCK_DETAILS_COLUMN_FAMILY_AS_BYTES,
            STOCK_COLUMN_QUALIFIER_AS_BYTES));
        System.out.println(new String(key.get()) + ": " +
        ToStringBuilder
              .reflectionToString(stock, ToStringStyle.SIMPLE_STYLE));
      }
    } finally {
      reader.close();
    }
  }
}
