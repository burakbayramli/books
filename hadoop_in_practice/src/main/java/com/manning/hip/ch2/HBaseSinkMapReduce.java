package com.manning.hip.ch2;

import com.manning.hip.ch3.StockPriceWritable;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.hbase.client.*;
import org.apache.hadoop.hbase.mapreduce.*;
import org.apache.hadoop.hbase.util.Bytes;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

import java.io.IOException;

import static com.manning.hip.ch2.HBaseWriteAvroStock.*;

public class HBaseSinkMapReduce
    extends Mapper<LongWritable, Text, StockPriceWritable, Put> {
  public static String STOCKS_IMPORT_TABLE_NAME =
      "stocks_example_import";

  @Override
  protected void map(LongWritable key, Text value,
                     Context context)
      throws IOException, InterruptedException {

    StockPriceWritable stock =
        StockPriceWritable.fromLine(value.toString());

    byte[] rowkey = Bytes.add(
        Bytes.toBytes(stock.getSymbol()),
        Bytes.toBytes(stock.getDate()));

    Put put = new Put(rowkey);

    byte[] colValue = Bytes.toBytes(stock.getClose());
    put.add(STOCK_DETAILS_COLUMN_FAMILY_AS_BYTES,
        STOCK_COLUMN_QUALIFIER_AS_BYTES,
        colValue
    );
    context.write(stock, put);
  }

  public static void main(String[] args) throws Exception {
    Configuration conf = new Configuration();

    createTableAndColumn(conf, STOCKS_IMPORT_TABLE_NAME,
        STOCK_DETAILS_COLUMN_FAMILY_AS_BYTES);

    Job job = new Job(conf);

    job.setJarByClass(HBaseSinkMapReduce.class);

    TableMapReduceUtil.initTableReducerJob(
        STOCKS_IMPORT_TABLE_NAME,
        IdentityTableReducer.class,
        job);

    job.setMapperClass(HBaseSinkMapReduce.class);

    job.setMapOutputKeyClass(StockPriceWritable.class);
    job.setMapOutputValueClass(Put.class);

    Path inputPath = new Path(args[0]);
    Path outputPath = new Path(args[1]);

    FileInputFormat.setInputPaths(job, inputPath);
    FileOutputFormat.setOutputPath(job, outputPath);

    outputPath.getFileSystem(conf).delete(outputPath, true);

    job.waitForCompletion(true);
  }
}