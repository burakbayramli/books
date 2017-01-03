package com.manning.hip.ch13;


import org.apache.commons.lang.StringUtils;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.DoubleWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.FileSplit;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;

public final class SimpleMovingAverage {
  public static void main(String... args) throws Exception {

    runJob(
        Arrays.copyOfRange(args, 0, args.length - 1),
        args[args.length - 1]);
  }

  public static void runJob(String[] input, String output)
      throws Exception {
    Configuration conf = new Configuration();

    Job job = new Job(conf);
    job.setJarByClass(SimpleMovingAverage.class);
    job.setMapperClass(Map.class);
    job.setReducerClass(Reduce.class);

    job.setMapOutputKeyClass(Text.class);
    job.setMapOutputValueClass(Text.class);

    Path outputPath = new Path(output);

    FileInputFormat.setInputPaths(job, StringUtils.join(input, ","));
    FileOutputFormat.setOutputPath(job, outputPath);

    outputPath.getFileSystem(conf).delete(outputPath, true);

    job.waitForCompletion(true);
  }

  public static class Map
      extends Mapper<LongWritable, Text, Text, Text> {

    private Text documentId;
    private Text word = new Text();

    @Override
    protected void setup(Context context) {
      String filename =
          ((FileSplit) context.getInputSplit()).getPath().getName();
      documentId = new Text(filename);
    }

    @Override
    protected void map(LongWritable key, Text value, Context context)
        throws IOException, InterruptedException {
      for (String token : StringUtils.split(value.toString())) {
        word.set(token);
        context.write(word, documentId);
      }
    }
  }

  public static class Reduce
      extends Reducer<Text, DoubleWritable, Text, DoubleWritable> {

    DoubleWritable outValue = new DoubleWritable();
    public void reduce(Text key, Iterable<DoubleWritable> values,
                       Context context)
        throws IOException, InterruptedException {

      double total = 0;
      int instances = 0;
      for (DoubleWritable stockPrice : values) {
        total += stockPrice.get();
        instances++;
      }
      outValue.set(total / (double) instances);
      context.write(key, outValue);
    }
  }

  public static class Reduce2
      extends Reducer<Text, DoubleWritable, Text, DoubleWritable> {

    SMA sma = new SMA();
    DoubleWritable outValue = new DoubleWritable();
    public void reduce(Text key, Iterable<DoubleWritable> values,
                       Context context)
        throws IOException, InterruptedException {
      sma.reset();
      for (DoubleWritable stockPrice : values) {
        sma.add(stockPrice.get());
      }
      outValue.set(sma.calculate());
      context.write(key, outValue);
    }
  }

  public static class SMA {
    protected double total = 0;
    protected int instances = 0;

    public void add(double value) {
      total += value;
      instances ++;
    }

    public double calculate() {
      return total / (double) instances;
    }

    public void reset() {
      total = 0;
      instances = 0;
    }
  }

}
