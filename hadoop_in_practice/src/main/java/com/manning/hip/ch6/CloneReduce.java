package com.manning.hip.ch6;


import com.manning.hip.ch4.sort.secondary.Person;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.mapreduce.lib.input.*;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

import java.io.IOException;
import java.util.*;

public final class CloneReduce {
  public static void main(String... args) throws Exception {
    runSortJob(args[0], args[1]);
  }

  public static void runSortJob(String input, String output)
      throws Exception {
    Configuration conf = new Configuration();

    Job job = new Job(conf);
    job.setJarByClass(CloneReduce.class);

    job.setReducerClass(Reduce.class);

    job.setInputFormatClass(KeyValueTextInputFormat.class);

    job.setMapOutputKeyClass(Person.class);
    job.setMapOutputValueClass(Text.class);

    job.setOutputKeyClass(Text.class);
    job.setOutputValueClass(Text.class);

    job.setSortComparatorClass(PersonBinaryComparator.class);

    Path outputPath = new Path(output);

    FileInputFormat.setInputPaths(job, input);
    FileOutputFormat.setOutputPath(job, outputPath);

    outputPath.getFileSystem(conf).delete(outputPath, true);

    job.waitForCompletion(true);
  }

  public static class Reduce
      extends Reducer<Text, Text, Text, Text> {

    @Override
    public void reduce(Text key, Iterable<Text> values,
                       Context context)
        throws IOException, InterruptedException {
      List<Text> cached = new ArrayList<Text>();
      for (Text value : values) {
        cached.add(WritableUtils.clone(value, context.getConfiguration()));
      }
    }
  }
}
