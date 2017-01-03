package com.manning.hip.ch7.friendsofafriend;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.lib.input.*;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

public class Main {
  public static void main(String... args) throws Exception {

    String inputFile = args[0];
    String calcOutputDir = args[1];
    String sortOutputDir = args[2];

    if (runCalcJob(inputFile, calcOutputDir)) {
      runSortJob(calcOutputDir, sortOutputDir);
    }
  }

  public static boolean runCalcJob(String input, String output)
      throws Exception {
    Configuration conf = new Configuration();

    Job job = new Job(conf);
    job.setJarByClass(Main.class);
    job.setMapperClass(CalcMapReduce.Map.class);
    job.setReducerClass(CalcMapReduce.Reduce.class);

    job.setInputFormatClass(KeyValueTextInputFormat.class);

    job.setMapOutputKeyClass(CalcMapReduce.TextPair.class);
    job.setMapOutputValueClass(IntWritable.class);

    Path outputPath = new Path(output);

    FileInputFormat.setInputPaths(job, input);
    FileOutputFormat.setOutputPath(job, outputPath);

    outputPath.getFileSystem(conf).delete(outputPath, true);

    return job.waitForCompletion(true);
  }

  public static void runSortJob(String input, String output)
      throws Exception {
    Configuration conf = new Configuration();

    Job job = new Job(conf);
    job.setJarByClass(Main.class);
    job.setMapperClass(SortMapReduce.Map.class);
    job.setReducerClass(SortMapReduce.Reduce.class);

    job.setInputFormatClass(KeyValueTextInputFormat.class);

    job.setMapOutputKeyClass(Person.class);
    job.setMapOutputValueClass(Person.class);

    job.setOutputKeyClass(Text.class);
    job.setOutputValueClass(Text.class);

    job.setPartitionerClass(PersonNamePartitioner.class);
    job.setSortComparatorClass(PersonComparator.class);
    job.setGroupingComparatorClass(PersonNameComparator.class);

    Path outputPath = new Path(output);

    FileInputFormat.setInputPaths(job, input);
    FileOutputFormat.setOutputPath(job, outputPath);

    outputPath.getFileSystem(conf).delete(outputPath, true);

    job.waitForCompletion(true);
  }
}
