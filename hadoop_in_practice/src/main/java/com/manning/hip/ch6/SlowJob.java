package com.manning.hip.ch6;

import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapred.*;

import java.io.IOException;

public class SlowJob {

    public static class Map implements
      Mapper<LongWritable, Text, LongWritable, Text> {

    @Override
    public void configure(JobConf job) {
    }

    @Override
    public void map(LongWritable key, Text value,
                    OutputCollector<LongWritable, Text> output,
                    Reporter reporter) throws IOException {

      String[] parts = value.toString().split("\\.");
      Text outputValue = new Text(parts[0]);
      output.collect(key, outputValue);
    }

    @Override
    public void close() throws IOException {
    }

  }

  public static void main(String... args) throws Exception {

    JobConf job = new JobConf();
    job.setJarByClass(SlowJob.class);
    Path input = new Path(args[0]);
    Path output = new Path(args[1]);

    output.getFileSystem(job).delete(output, true);

    job.setMapperClass(Map.class);

    FileInputFormat.setInputPaths(job, input);
    FileOutputFormat.setOutputPath(job, output);

    job.setProfileEnabled(true);
    job.setProfileParams(
        "-agentlib:hprof=depth=8,cpu=samples,heap=sites,force=n," +
        "thread=y,verbose=n,file=%s");
    job.setProfileTaskRange(true, "0,1,5-10");
    job.setProfileTaskRange(false, "");

    JobClient.runJob(job);

    System.out.println("Done");

    Thread.sleep(20000);
  }


}
