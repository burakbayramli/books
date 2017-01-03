package com.manning.hip.ch2;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.hbase.KeyValue;
import org.apache.hadoop.hbase.client.*;
import org.apache.hadoop.hbase.io.ImmutableBytesWritable;
import org.apache.hadoop.hbase.mapreduce.*;
import org.apache.hadoop.io.Writable;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

import java.io.IOException;

import static com.manning.hip.ch2.HBaseWriteAvroStock.*;

public class HBaseSourceSinkMapReduce extends
    TableMapper<ImmutableBytesWritable, Writable> {
  public static String STOCKS_IMPORT_TABLE_NAME =
      "stocks_example_import";

  @Override
  public void map(ImmutableBytesWritable row, Result columns,
                  Context context)
      throws IOException, InterruptedException {
      Put put = new Put(row.get());
      for (KeyValue kv : columns.list()) {
        byte[] value = kv.getValue();
        put.add(STOCK_DETAILS_COLUMN_FAMILY_AS_BYTES,
            STOCK_COLUMN_QUALIFIER_AS_BYTES,
            value
            );
        context.write(row, put);
      }
  }


  public static void main(String[] args) throws Exception {
    Configuration conf = new Configuration();

    createTableAndColumn(conf, STOCKS_IMPORT_TABLE_NAME,
        STOCK_DETAILS_COLUMN_FAMILY_AS_BYTES);

    Scan scan = new Scan();
    scan.addColumn(STOCK_DETAILS_COLUMN_FAMILY_AS_BYTES,
        STOCK_COLUMN_QUALIFIER_AS_BYTES);
    Job job = new Job(conf);

    job.setJarByClass(HBaseSourceSinkMapReduce.class);

    TableMapReduceUtil.initTableMapperJob(
        STOCKS_TABLE_NAME,
        scan,
        HBaseSourceSinkMapReduce.class,
        ImmutableBytesWritable.class,
        Put.class,
        job);

    TableMapReduceUtil.initTableReducerJob(
        STOCKS_IMPORT_TABLE_NAME,
        IdentityTableReducer.class,
        job);

    Path outputPath = new Path(args[0]);

    FileOutputFormat.setOutputPath(job, outputPath);

    outputPath.getFileSystem(conf).delete(outputPath, true);

    job.waitForCompletion(true);
  }
}