package com.manning.hip.ch2;

import com.manning.hip.ch3.avro.gen.Stock;
import org.apache.avro.io.*;
import org.apache.avro.specific.SpecificDatumReader;
import org.apache.commons.lang.builder.*;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.hbase.*;
import org.apache.hadoop.hbase.client.*;

import java.io.*;

import static com.manning.hip.ch2.HBaseWriteAvroStock.*;

public class HBaseScanAvroStock {

  public static void main(String[] args) throws Exception {

    Configuration conf = HBaseConfiguration.create();

    HTable htable = new HTable(conf, STOCKS_TABLE_NAME);

    ResultScanner scanner = htable.getScanner(
        STOCK_DETAILS_COLUMN_FAMILY_AS_BYTES,
        STOCK_COLUMN_QUALIFIER_AS_BYTES);

    AvroStockReader reader = new AvroStockReader();

    for(Result result: scanner) {
      String rowkey = new String(result.getRow());

      byte[] value = result.getValue(
          STOCK_DETAILS_COLUMN_FAMILY_AS_BYTES,
          STOCK_COLUMN_QUALIFIER_AS_BYTES);

      Stock stock = reader.decode(value);

      System.out.println("rowkey = '" + rowkey +
          "' stock = '" +
          ToStringBuilder
              .reflectionToString(stock, ToStringStyle.SIMPLE_STYLE));
    }

    htable.close();
  }

  public static class AvroStockReader {
    Stock stock;
    BinaryDecoder decoder;
    SpecificDatumReader<Stock> reader;

    public AvroStockReader() {
      reader = new SpecificDatumReader<Stock>();
      reader.setSchema(Stock.SCHEMA$);
    }

    public Stock decode(byte[] value) throws IOException {
      ByteArrayInputStream bai = new ByteArrayInputStream(value);
      decoder = DecoderFactory.get().directBinaryDecoder(bai, decoder);
      stock = reader.read(stock, decoder);
      return stock;
    }

  }

}
