package com.manning.hip.ch2;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.hbase.*;
import org.apache.hadoop.hbase.client.*;
import org.apache.hadoop.hbase.util.Bytes;

import java.util.Random;

public class StockHBaseImporter {
  public static void main(String[] args) throws Exception {

    String[] pages = {"/", "/a.html", "/b.html", "/c.html"};

    Configuration conf = HBaseConfiguration.create();

    conf.set("hbase.master", "localhost:60000");

    HBaseAdmin hbase = new HBaseAdmin(conf);
    HTableDescriptor desc = new HTableDescriptor("access_logs");
    HColumnDescriptor meta =
        new HColumnDescriptor("details".getBytes());
    desc.addFamily(meta);
    hbase.createTable(desc);

    HTable htable = new HTable(conf, "access_logs");
    htable.setAutoFlush(false);
    htable.setWriteBufferSize(1024 * 1024 * 12);

    int totalRecords = 100000;
    int maxID = totalRecords / 1000;
    Random rand = new Random();
    System.out.println("importing " + totalRecords + " records ....");
    for (int i = 0; i < totalRecords; i++) {
      int userID = rand.nextInt(maxID) + 1;
      byte[] rowkey =
          Bytes.add(Bytes.toBytes(userID), Bytes.toBytes(i));
      String randomPage = pages[rand.nextInt(pages.length)];
      Put put = new Put(rowkey);
      put.add(Bytes.toBytes("details"), Bytes.toBytes("page"), Bytes
          .toBytes(randomPage));
      htable.put(put);
    }
    htable.flushCommits();
    htable.close();
    System.out.println("done");
  }
}
