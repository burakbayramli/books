package com.manning.hip.ch3.avro;

import com.manning.hip.ch3.avro.gen.StockAvg;
import org.apache.avro.file.DataFileStream;
import org.apache.avro.specific.SpecificDatumReader;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IOUtils;

import java.io.IOException;
import java.io.InputStream;

public class AvroStockAvgFileRead {

  public static void readFromAvro(InputStream is) throws IOException {
    DataFileStream<StockAvg> reader =     //<co id="ch03_smallfileread_comment1"/>
        new DataFileStream<StockAvg>(
            is,
            new SpecificDatumReader<StockAvg>(StockAvg.class));

    for (StockAvg a : reader) {          //<co id="ch03_smallfileread_comment2"/>
      System.out.println(ToStringBuilder.reflectionToString(a,
          ToStringStyle.SHORT_PREFIX_STYLE
      ));
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
