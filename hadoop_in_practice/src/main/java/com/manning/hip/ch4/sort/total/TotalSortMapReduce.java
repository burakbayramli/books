package com.manning.hip.ch4.sort.total;


import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapred.*;
import org.apache.hadoop.mapred.lib.*;

public final class TotalSortMapReduce {
  public static void main(String... args) throws Exception {
    runSortJob(args);
  }

  public static void runSortJob(String ... args)
      throws Exception {

    int numReducers = 2;
    Path input = new Path(args[0]);
    Path partitionFile = new Path(args[1]);
    Path output = new Path(args[2]);

    InputSampler.Sampler<Text, Text> sampler =
        new InputSampler.RandomSampler<Text,Text>
            (0.1,
             10000,
             10);

    JobConf job = new JobConf();

    job.setNumReduceTasks(numReducers);

    job.setInputFormat(KeyValueTextInputFormat.class);
    job.setOutputFormat(TextOutputFormat.class);
    job.setPartitionerClass(TotalOrderPartitioner.class);

    job.setMapOutputKeyClass(Text.class);
    job.setMapOutputValueClass(Text.class);

    TotalOrderPartitioner.setPartitionFile(job, partitionFile);
    FileInputFormat.setInputPaths(job, input);
    FileOutputFormat.setOutputPath(job, output);

    InputSampler.writePartitionFile(job, sampler);

    job.setJarByClass(TotalSortMapReduce.class);

    output.getFileSystem(job).delete(output, true);

    JobClient.runJob(job);
  }
}
