package com.manning.hip.ch4.joins.contribjoin;

import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapred.*;

public class Main {
  public static void main(String... args) throws Exception {

    JobConf job = new JobConf();
    job.setJarByClass(Main.class);

    String input = args[0];
    Path output = new Path(args[1]);

    output.getFileSystem(job).delete(output, true);

    job.setMapperClass(Map.class);
    job.setReducerClass(Reduce.class);

    job.setMapOutputKeyClass(Text.class);
    job.setMapOutputValueClass(TextTaggedMapOutput.class);
    job.setOutputKeyClass(Text.class);
    job.setOutputValueClass(Text.class);

    FileInputFormat.setInputPaths(job, input);
    FileOutputFormat.setOutputPath(job, output);

    JobClient.runJob(job);
  }
}
