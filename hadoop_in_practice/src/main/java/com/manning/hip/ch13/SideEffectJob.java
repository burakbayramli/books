package com.manning.hip.ch13;


import org.apache.commons.io.IOUtils;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.KeyValueTextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public final class SideEffectJob {
  private static final Logger log = LoggerFactory.getLogger(
      SideEffectJob.class);

  public static void main(String... args) throws Exception {
    runJob(args[0], args[1]);
  }

  public static void runJob(String input, String output)
      throws Exception {
    Configuration conf = new Configuration();

    Job job = new Job(conf);
    job.setJarByClass(SideEffectJob.class);
    job.setMapperClass(Map.class);
    //job.setReducerClass(Reduce.class);
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

    OutputStream sideEffectStream;

    @Override
    protected void setup(Context context)
        throws IOException, InterruptedException {
      Path attemptDir = FileOutputFormat.getWorkOutputPath(context);
      String filename = context.getTaskAttemptID()
          .getTaskID().toString();

      Path sideEffectFile = new Path(attemptDir, filename);

      sideEffectStream = FileSystem.get(context.getConfiguration())
          .create(sideEffectFile);

      log.info("Opened file = {}", sideEffectFile);
    }

    @Override
    protected void map(Text key, Text value, Context context)
        throws IOException, InterruptedException {

      IOUtils.write(key.toString(), sideEffectStream);

      context.write(key, value);
    }

    @Override
    protected void cleanup(Context context)
        throws IOException, InterruptedException {
      sideEffectStream.close();
    }
  }

  public static class Reduce
      extends Reducer<Text, Text, Text, Text> {

    public void reduce(Text key, Iterable<Text> values,
                       Context context)
        throws IOException, InterruptedException {
      if(log.isDebugEnabled()) {
        log.debug("Input K[{}]", key);
      }
      for (Text val : values) {
        if(log.isDebugEnabled()) {
          log.debug("Input V[{}]", val);
          log.debug("Output K[{}],V[{}]", key, val);
        }
        context.write(key, val);
      }
    }
  }
}
