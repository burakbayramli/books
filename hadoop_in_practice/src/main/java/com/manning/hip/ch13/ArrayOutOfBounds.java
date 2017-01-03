package com.manning.hip.ch13;


import org.apache.commons.lang.StringUtils;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.KeyValueTextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

import java.io.IOException;

public final class ArrayOutOfBounds {
  public static void main(String... args) throws Exception {
    runJob(args[0], args[1]);
  }

  public static void runJob(String input, String output)
      throws Exception {
    Configuration conf = new Configuration();

    conf.set("keep.failed.task.files", "true");

    Job job = new Job(conf);
    job.setJarByClass(ArrayOutOfBounds.class);
    job.setMapperClass(Map.class);
    job.setNumReduceTasks(0);

    job.setInputFormatClass(KeyValueTextInputFormat.class);
    job.setMapOutputKeyClass(Text.class);
    job.setMapOutputValueClass(Text.class);

    Path outputPath = new Path(output);

    FileInputFormat.setInputPaths(job, input);
    FileOutputFormat.setOutputPath(job, outputPath);

    outputPath.getFileSystem(conf).delete(outputPath, true);

    job.waitForCompletion(true);
  }

  public static class Map
      extends Mapper<Text, Text, Text, Text> {
    Text outputValue = new Text();
    @Override
    protected void map(Text key, Text value, Context context)
        throws IOException, InterruptedException {
      String id = StringUtils.split(value.toString())[5];
      outputValue.set(id);
      context.write(key, outputValue);
    }
  }
}
