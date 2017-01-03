package com.manning.hip.ch2;

import com.manning.hip.ch3.avro.AvroStockFileWrite;
import com.manning.hip.ch3.avro.gen.Stock;
import org.apache.avro.io.*;
import org.apache.avro.specific.SpecificDatumWriter;
import org.apache.commons.io.FileUtils;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.hbase.*;
import org.apache.hadoop.hbase.client.*;
import org.apache.hadoop.hbase.util.Bytes;

import java.io.*;

public class HBaseWriteAvroStock {

  public static String STOCKS_TABLE_NAME = "stocks_example";
  public static String STOCK_DETAILS_COLUMN_FAMILY = "details";
  public static byte[] STOCK_DETAILS_COLUMN_FAMILY_AS_BYTES =
      Bytes.toBytes(STOCK_DETAILS_COLUMN_FAMILY);
  public static String STOCK_COLUMN_QUALIFIER = "stockAvro";
  public static byte[] STOCK_COLUMN_QUALIFIER_AS_BYTES = Bytes.toBytes(
      STOCK_COLUMN_QUALIFIER);

  public static void main(String[] args) throws Exception {

    File inputFile = new File(args[0]);

    Configuration conf = HBaseConfiguration.create();

    createTableAndColumn(conf, STOCKS_TABLE_NAME,
        STOCK_DETAILS_COLUMN_FAMILY_AS_BYTES);

    HTable htable = new HTable(conf, STOCKS_TABLE_NAME);
    htable.setAutoFlush(false);
    htable.setWriteBufferSize(1024 * 1024 * 12);


    SpecificDatumWriter<Stock> writer =
        new SpecificDatumWriter<Stock>();
    writer.setSchema(Stock.SCHEMA$);

    ByteArrayOutputStream bao = new ByteArrayOutputStream();
    BinaryEncoder encoder =
        EncoderFactory.get().directBinaryEncoder(bao, null);

    for (String line : FileUtils.readLines(inputFile)) {
      Stock stock = AvroStockFileWrite.createStock(line);
      writer.write(stock, encoder);
      encoder.flush();

      byte[] rowkey = Bytes.add(
          Bytes.toBytes(stock.symbol.toString()),
          Bytes.toBytes(stock.date.toString()));

      byte[] stockAsAvroBytes = bao.toByteArray();

      Put put = new Put(rowkey);
      put.add(STOCK_DETAILS_COLUMN_FAMILY_AS_BYTES,
          STOCK_COLUMN_QUALIFIER_AS_BYTES,
          stockAsAvroBytes);

      htable.put(put);

      bao.reset();
    }

    htable.flushCommits();
    htable.close();
    System.out.println("done");
  }

  public static void createTableAndColumn(Configuration conf,
                                          String table,
                                          byte[] columnFamily)
      throws IOException {
    HBaseAdmin hbase = new HBaseAdmin(conf);
    HTableDescriptor desc = new HTableDescriptor(table);
    HColumnDescriptor meta = new HColumnDescriptor(columnFamily);
    desc.addFamily(meta);
    if (hbase.tableExists(table)) {
      if(hbase.isTableEnabled(table)) {
        hbase.disableTable(table);
      }
      hbase.deleteTable(table);
    }
    hbase.createTable(desc);
  }

}
