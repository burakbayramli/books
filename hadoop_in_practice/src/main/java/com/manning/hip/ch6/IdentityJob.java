package com.manning.hip.ch6;

import org.apache.hadoop.fs.Path;
import org.apache.hadoop.mapred.*;

public class IdentityJob {

  public static void main(String... args) throws Exception {

    JobConf job = new JobConf();
    job.setJarByClass(IdentityJob.class);
    Path input = new Path(args[0]);
    Path output = new Path(args[1]);

    output.getFileSystem(job).delete(output, true);

    FileInputFormat.setInputPaths(job, input);
    FileOutputFormat.setOutputPath(job, output);

    JobClient.runJob(job);
  }
}
