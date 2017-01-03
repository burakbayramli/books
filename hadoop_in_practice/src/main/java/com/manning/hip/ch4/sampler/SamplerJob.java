package com.manning.hip.ch4.sampler;


import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.lib.input.*;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

public final class SamplerJob {
  public static void main(String... args) throws Exception {
    runSortJob(args[0], args[1]);
  }

  public static void runSortJob(String input, String output)
      throws Exception {
    Configuration conf = new Configuration();

    Job job = new Job(conf);
    job.setJarByClass(SamplerJob.class);

    ReservoirSamplerInputFormat.setInputFormat(job,
        TextInputFormat.class);

    ReservoirSamplerInputFormat.setNumSamples(job, 10);
    ReservoirSamplerInputFormat.setMaxRecordsToRead(job, 10000);
    ReservoirSamplerInputFormat.
        setUseSamplesNumberPerInputSplit(job, true);

    Path outputPath = new Path(output);

    FileInputFormat.setInputPaths(job, input);
    FileOutputFormat.setOutputPath(job, outputPath);

    outputPath.getFileSystem(conf).delete(outputPath, true);

    job.waitForCompletion(true);
  }
}
