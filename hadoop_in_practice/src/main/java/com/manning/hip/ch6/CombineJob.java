package com.manning.hip.ch6;

import org.apache.commons.lang.StringUtils;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapred.*;
import org.apache.hadoop.util.ReflectionUtils;

import java.io.IOException;
import java.util.Iterator;

public class CombineJob {

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

  public static class Combine
      implements Reducer<Text, Text, Text, Text> {

    private JobConf job;

    @Override
    public void configure(JobConf job) {
      this.job = job;
    }

    @Override
    public void reduce(Text key, Iterator<Text> values,
                       OutputCollector<Text,
                           Text> output,
                       Reporter reporter) throws IOException {

      Text prev = null;
      while (values.hasNext()) {
        Text t = values.next();

        if (!t.equals(prev)) {
          output.collect(key, t);
        }
        prev = ReflectionUtils.copy(job, t, prev);
      }
    }

    @Override
    public void close() throws IOException {
    }

  }

  public static void main(String... args) throws Exception {

    JobConf job = new JobConf();
    job.setJarByClass(CombineJob.class);
    Path input = new Path(args[0]);
    Path output = new Path(args[1]);

    output.getFileSystem(job).delete(output, true);

    job.setMapperClass(Map.class);
    job.setCombinerClass(Combine.class);
    job.setMapOutputKeyClass(Text.class);

    FileInputFormat.setInputPaths(job, input);
    FileOutputFormat.setOutputPath(job, output);

    JobClient.runJob(job);
  }
}
