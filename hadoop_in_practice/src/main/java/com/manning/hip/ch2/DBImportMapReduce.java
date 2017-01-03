package com.manning.hip.ch2;

import com.manning.hip.ch3.StockPriceWritable;
import com.manning.hip.ch3.avro.gen.*;
import com.manning.hip.common.JobHelper;
import org.apache.avro.mapred.*;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.*;
import org.apache.hadoop.io.compress.SnappyCodec;
import org.apache.hadoop.mapred.*;
import org.apache.hadoop.mapred.lib.db.*;

import java.io.IOException;

public final class DBImportMapReduce {

  public static void main(String... args) throws Exception {
    runJob(args[0], args[1]);
  }

  public static void runJob(String mysqlJar, String output)
      throws Exception {

    Configuration conf = new Configuration();

    JobHelper.addJarForJob(conf, mysqlJar);

    DBConfiguration.configureDB(conf, "com.mysql.jdbc.Driver",
        "jdbc:mysql://localhost/sqoop_test" +
            "?user=hip_sqoop_user&password=password");

    JobConf job = new JobConf(conf);
    job.setJarByClass(DBImportMapReduce.class);
    Path outputPath = new Path(output);

    outputPath.getFileSystem(job).delete(outputPath, true);

    job.setInputFormat(DBInputFormat.class);
    job.setOutputFormat(AvroOutputFormat.class);
    AvroJob.setOutputSchema(job, Stock.SCHEMA$);
    job.set(AvroJob.OUTPUT_CODEC, SnappyCodec.class.getName());

    job.setMapperClass(Map.class);

    job.setNumMapTasks(4);
    job.setNumReduceTasks(0);

    job.setMapOutputKeyClass(AvroWrapper.class);
    job.setMapOutputValueClass(NullWritable.class);

    job.setOutputKeyClass(AvroWrapper.class);
    job.setOutputValueClass(NullWritable.class);

    FileOutputFormat.setOutputPath(job, outputPath);

    DBInputFormat.setInput(
        job,
        StockRecord.class,
        "select * from stocks",
        "SELECT COUNT(id) FROM stocks");

    JobClient.runJob(job);
  }

    public static class Map implements
        Mapper<LongWritable, StockRecord, AvroWrapper<Stock>, NullWritable> {

    public void map(LongWritable key,
                    StockRecord value,
                    OutputCollector<AvroWrapper<Stock>, NullWritable> output,
                    Reporter reporter) throws IOException {
      output.collect(
          new AvroWrapper<Stock>(writableToAvro(value)),
          NullWritable.get());
    }

    public void close() throws IOException {
    }

    public void configure(JobConf job) {
    }
  }

  public static Stock writableToAvro(StockPriceWritable writable) {
    Stock avro = new Stock();
    avro.symbol = writable.getSymbol();
    avro.date = writable.getDate();
    avro.open = writable.getOpen();
    avro.high = writable.getHigh();
    avro.low = writable.getLow();
    avro.close = writable.getClose();
    avro.volume = writable.getVolume();
    avro.adjClose = writable.getAdjClose();
    return avro;
  }

}
