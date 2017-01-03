package com.manning.hip.ch6;

import org.apache.commons.lang.StringUtils;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapred.*;

import java.io.IOException;

public class FilterProjectJob {

  public static class Map implements
      Mapper<LongWritable, Text, Text, Text> {

    Text outputKey = new Text();
    Text outputValue = new Text();

    @Override
    public void configure(JobConf job) {
    }

    @Override
    public void map(LongWritable key, Text value,
                    OutputCollector<Text, Text> output,
                    Reporter reporter) throws IOException {

      String v = value.toString();
      if (!v.startsWith("10.")) {
        String[] parts = StringUtils.split(v, ".", 3);
        outputKey.set(parts[0]);
        outputValue.set(parts[1]);
        output.collect(outputKey, outputValue);
      }
    }

    @Override
    public void close() throws IOException {
    }

  }

  public static void main(String... args) throws Exception {

    JobConf job = new JobConf();
    job.setJarByClass(FilterProjectJob.class);
    Path input = new Path(args[0]);
    Path output = new Path(args[1]);

    output.getFileSystem(job).delete(output, true);

    job.setMapperClass(Map.class);
    job.setMapOutputKeyClass(Text.class);

    FileInputFormat.setInputPaths(job, input);
    FileOutputFormat.setOutputPath(job, output);

    JobClient.runJob(job);
  }
}
