package com.manning.hip.ch2;


import com.manning.hip.common.JobHelper;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.mapreduce.lib.db.*;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

import java.io.IOException;

public final class DBImportExportMapReduce {

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

    Job job = new Job(conf);
    job.setJarByClass(DBImportExportMapReduce.class);
    job.setMapperClass(Map.class);
    job.setReducerClass(Reduce.class);

    job.setInputFormatClass(DBInputFormat.class);
    job.setOutputFormatClass(DBOutputFormat.class);

    job.setMapOutputKeyClass(StockRecord.class);
    job.setMapOutputValueClass(NullWritable.class);

    job.setOutputKeyClass(StockRecord.class);
    job.setOutputValueClass(NullWritable.class);

    job.getConfiguration().setInt("mapred.map.tasks", 4);
    job.setNumReduceTasks(4);

    DBInputFormat.setInput(
        job,
        StockRecord.class,
        "select * from stocks",
        "SELECT COUNT(id) FROM stocks");

    DBOutputFormat.setOutput(
        job,
        "stocks_export",
        StockRecord.fields);

    Path outputPath = new Path(output);

    FileOutputFormat.setOutputPath(job, outputPath);

    outputPath.getFileSystem(conf).delete(outputPath, true);

    job.waitForCompletion(true);
  }

  public static class Map
      extends
      Mapper<LongWritable, StockRecord, StockRecord, NullWritable> {

    @Override
    protected void map(LongWritable key, StockRecord value,
                       Context context)
        throws IOException, InterruptedException {
      context.write(value, NullWritable.get());
    }
  }

  public static class Reduce
      extends
      Reducer<StockRecord, NullWritable, StockRecord, NullWritable> {

    public void reduce(StockRecord key, Iterable<NullWritable> values,
                       Context context)
        throws IOException, InterruptedException {
      context.write(key, NullWritable.get());
    }
  }
}
