package com.manning.hip.ch3.passwd;


import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;

public final class PasswdMapReduce {
  private static final Logger log = LoggerFactory.getLogger(
      PasswdMapReduce.class);

  public static class Map extends Mapper<LongWritable, Passwd,
      Text, Passwd> {
    private Text redKey = new Text();
    @Override
    protected void map(LongWritable key, Passwd value,
                       Context context)
        throws
        IOException, InterruptedException {
      redKey.set(value.getUsername());
      context.write(redKey, value);
    }
  }

  public static class Reduce
      extends Reducer<Text, Passwd, Passwd, NullWritable> {

    public void reduce(Text key, Iterable<Passwd> values,
                       Context context)
        throws IOException, InterruptedException {
      for (Passwd val : values) {
        log.info(val.toString());
        context.write(val, NullWritable.get());
      }
      //throw new IOException();
    }
  }

  public static void main(String... args) throws Exception {
    runJob(args[0], args[1]);
  }

  public static void runJob(String input,
                            String output)
      throws Exception {
    Configuration conf = new Configuration();

    Job job = new Job(conf);
    job.setJarByClass(PasswdMapReduce.class);
    job.setMapperClass(Map.class);
    job.setReducerClass(Reduce.class);
    job.setInputFormatClass(PasswdInputFormat.class);
    job.setOutputFormatClass(PasswdOutputFormat.class);

    job.setMapOutputKeyClass(Text.class);
    job.setMapOutputValueClass(Passwd.class);

    FileInputFormat.setInputPaths(job, new Path(input));
    Path outPath = new Path(output);
    FileOutputFormat.setOutputPath(job, outPath);

    outPath.getFileSystem(conf).delete(outPath, true);

    job.waitForCompletion(true);
  }
}
