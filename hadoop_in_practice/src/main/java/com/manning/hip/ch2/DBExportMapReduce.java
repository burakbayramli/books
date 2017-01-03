package com.manning.hip.ch2;

import com.manning.hip.ch3.StockPriceWritable;
import com.manning.hip.ch3.avro.gen.Stock;
import com.manning.hip.common.JobHelper;
import org.apache.avro.mapred.*;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.mapred.*;
import org.apache.hadoop.mapred.lib.db.*;

import java.io.IOException;

public final class DBExportMapReduce {

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
    job.setJarByClass(DBExportMapReduce.class);
    Path outputPath = new Path(output);

    outputPath.getFileSystem(job).delete(outputPath, true);

    job.set(AvroJob.INPUT_SCHEMA, Stock.SCHEMA$.toString());

    job.setInputFormat(AvroInputFormat.class);

    job.setInputFormat(DBInputFormat.class);
    job.setOutputFormat(DBOutputFormat.class);
    AvroJob.setOutputSchema(job, Stock.SCHEMA$);

    job.setMapperClass(Map.class);

    job.setMapOutputKeyClass(StockRecord.class);
    job.setMapOutputValueClass(NullWritable.class);

    job.setOutputKeyClass(StockRecord.class);
    job.setOutputValueClass(NullWritable.class);

    job.setNumReduceTasks(4);

    DBOutputFormat.setOutput(
        job,
        "stocks_export",
        StockRecord.fields);
    JobClient.runJob(job);
  }

    public static class Map implements
        Mapper<AvroWrapper<Stock>, NullWritable, StockRecord, NullWritable> {

    public void map(AvroWrapper<Stock> key,
                    NullWritable value,
                    OutputCollector<StockRecord, NullWritable> output,
                    Reporter reporter) throws IOException {
//      output.collect(
//          new AvroWrapper<Stock>(writableToAvro(value)),
//          NullWritable.get());
    }

    public void close() throws IOException {
    }

    public void configure(JobConf job) {
    }
  }

  public static Stock avroToWritable(StockPriceWritable writable) {
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
