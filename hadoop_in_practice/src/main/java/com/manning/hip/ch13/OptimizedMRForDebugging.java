package com.manning.hip.ch13;


import org.apache.hadoop.conf.Configuration;
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

public final class OptimizedMRForDebugging {
  private static final Logger log = LoggerFactory.getLogger(
      OptimizedMRForDebugging.class);

  public static void main(String... args) throws Exception {
    runJob(args[0], args[1]);
  }

  public static void runJob(String input, String output)
      throws Exception {
    Configuration conf = new Configuration();

    conf.set("keep.failed.task.files", "true");

    Job job = new Job(conf);
    job.setJarByClass(OptimizedMRForDebugging.class);
    job.setMapperClass(Map.class);
    job.setReducerClass(Reduce.class);
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
    protected int failedRecords;
    public static enum Counters {
      FAILED_RECORDS
    }

    @Override
    protected void setup(Context context)
        throws IOException, InterruptedException {
      super.setup(context);
      log.info("Input split = {}", context.getInputSplit());
    }

    @Override
    protected void map(Text key, Text value, Context context)
        throws IOException, InterruptedException {

      if(log.isDebugEnabled()) {
        log.debug("Input K[{}],V[{}]", key, value);
      }

      try {
        if(log.isDebugEnabled()) {
          log.debug("Output K[{}],V[{}]", key, value);
        }
        context.write(key, value);
      } catch(Throwable t) {
        processError(context, t, key, value);
      }
    }

    protected void processError(Context c, Throwable t, Text k, Text v) {
      log.error("Caught exception processing key[" + k + "], value[" + v + "]", t);
      c.getCounter(Counters.FAILED_RECORDS).increment(1);
      c.setStatus("Records with failures = " + (++failedRecords));
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
