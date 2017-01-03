package com.manning.hip.ch6;

import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapred.*;

import java.io.IOException;

public class SampleJob {

    public static void main(String... args) throws Exception {
    runSortJob(args);
  }

  public static void runSortJob(String ... args)
      throws Exception {

    Path input = new Path(args[0]);
    Path output = new Path(args[1]);

    JobConf job = new JobConf();

    job.setNumReduceTasks(2);

    job.setInputFormat(KeyValueTextInputFormat.class);
    job.setOutputFormat(TextOutputFormat.class);

    job.setMapOutputKeyClass(Text.class);
    job.setMapOutputValueClass(Text.class);

    FileInputFormat.setInputPaths(job, input);
    FileOutputFormat.setOutputPath(job, output);

    job.setJarByClass(SampleJob.class);

    output.getFileSystem(job).delete(output, true);

    JobClient jc = new JobClient(job);
    JobClient.setTaskOutputFilter(job, JobClient.TaskStatusFilter.ALL);
    RunningJob rj = jc.submitJob(job);
    try {
      if (!jc.monitorAndPrintJob(job, rj)) {
        System.out.println("Job Failed: " + rj.getFailureInfo());
        throw new IOException("Job failed!");
      }
    } catch (InterruptedException ie) {
      Thread.currentThread().interrupt();
    }

  }

}
