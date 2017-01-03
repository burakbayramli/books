package com.manning.hip.ch2;

import com.manning.hip.ch3.avro.gen.Stock;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.hbase.KeyValue;
import org.apache.hadoop.hbase.client.*;
import org.apache.hadoop.hbase.io.ImmutableBytesWritable;
import org.apache.hadoop.hbase.mapreduce.*;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

import java.io.IOException;

import static com.manning.hip.ch2.HBaseWriteAvroStock.*;

public class HBaseSourceMapReduce extends
    TableMapper<Text, DoubleWritable> {

  private HBaseScanAvroStock.AvroStockReader stockReader;
  private Text outputKey = new Text();
  private DoubleWritable outputValue = new DoubleWritable();

  @Override
  protected void setup(
      Context context)
      throws IOException, InterruptedException {
    stockReader = new HBaseScanAvroStock.AvroStockReader();
  }

  @Override
  public void map(ImmutableBytesWritable row, Result columns,
                  Context context)
      throws IOException, InterruptedException {
      for (KeyValue kv : columns.list()) {
        byte[] value = kv.getValue();

        Stock stock = stockReader.decode(value);

        outputKey.set(stock.symbol.toString());
        outputValue.set(stock.close);
        context.write(outputKey, outputValue);
      }
  }

  public static void main(String[] args) throws Exception {
    Configuration conf = new Configuration();

    Scan scan = new Scan();
    scan.addColumn(STOCK_DETAILS_COLUMN_FAMILY_AS_BYTES,
        STOCK_COLUMN_QUALIFIER_AS_BYTES);
    Job job = new Job(conf);

    job.setJarByClass(HBaseSourceMapReduce.class);

    TableMapReduceUtil.initTableMapperJob(
        STOCKS_TABLE_NAME,
        scan,
        HBaseSourceMapReduce.class,
        ImmutableBytesWritable.class,
        Put.class,
        job);

    job.setNumReduceTasks(0);

    job.setMapOutputKeyClass(Text.class);
    job.setMapOutputValueClass(DoubleWritable.class);

    Path outputPath = new Path(args[0]);

    FileOutputFormat.setOutputPath(job, outputPath);

    outputPath.getFileSystem(conf).delete(outputPath, true);

    job.waitForCompletion(true);
  }
}