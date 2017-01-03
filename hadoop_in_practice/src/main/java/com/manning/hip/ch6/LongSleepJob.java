package com.manning.hip.ch6;

import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapred.*;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

public class LongSleepJob {

    public static class Map implements
      Mapper<LongWritable, Text, LongWritable, Text> {

    @Override
    public void configure(JobConf job) {
    }

    @Override
    public void map(LongWritable key, Text value,
                    OutputCollector<LongWritable, Text> output,
                    Reporter reporter) throws IOException {
      try {
        Thread.sleep(TimeUnit.MINUTES.toMillis(5));
      } catch (InterruptedException e) {
      }
    }

    @Override
    public void close() throws IOException {
    }

  }

  public static void main(String... args) throws Exception {

    JobConf job = new JobConf();
    job.setJarByClass(LongSleepJob.class);
    Path input = new Path(args[0]);
    Path output = new Path(args[1]);

    output.getFileSystem(job).delete(output, true);

    job.setMapperClass(Map.class);

    FileInputFormat.setInputPaths(job, input);
    FileOutputFormat.setOutputPath(job, output);

    job.set("mapred.task.timeout", String.valueOf(TimeUnit.MINUTES.toMillis(10)));

    JobClient.runJob(job);
  }
}
