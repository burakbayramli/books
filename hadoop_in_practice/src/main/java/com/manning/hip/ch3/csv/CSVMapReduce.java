package com.manning.hip.ch3.csv;


import com.manning.hip.ch3.TextArrayWritable;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

import java.io.IOException;

public final class CSVMapReduce {

  public static class Map       //<co id="ch03_comment_csv_mr1"/>
      extends Mapper<LongWritable, TextArrayWritable,
      LongWritable, TextArrayWritable> {

    @Override
    protected void map(LongWritable key, TextArrayWritable value,
                       Context context)
        throws
        IOException, InterruptedException {
      context.write(key, value);
    }
  }

  public static class Reduce     //<co id="ch03_comment_csv_mr2"/>
      extends Reducer<LongWritable, TextArrayWritable,
      TextArrayWritable, NullWritable> {

    public void reduce(LongWritable key,
                       Iterable<TextArrayWritable> values,
                       Context context)
        throws IOException, InterruptedException {
      for (TextArrayWritable val : values) {
        context.write(val, NullWritable.get());
      }
    }
  }

  public static void main(String... args) throws Exception {
    runJob(args[0], args[1]);
  }

  public static void runJob(String input,
                            String output)
      throws Exception {
    Configuration conf = new Configuration();
    conf.set(CSVInputFormat.CSV_TOKEN_SEPARATOR_CONFIG, ",");  //<co id="ch03_comment_csv_mr3"/>
    conf.set(CSVOutputFormat.CSV_TOKEN_SEPARATOR_CONFIG, ":"); //<co id="ch03_comment_csv_mr4"/>

    Job job = new Job(conf);
    job.setJarByClass(CSVMapReduce.class);
    job.setMapperClass(Map.class);
    job.setReducerClass(Reduce.class);
    job.setInputFormatClass(CSVInputFormat.class); //<co id="ch03_comment_csv_mr5"/>
    job.setOutputFormatClass(CSVOutputFormat.class); //<co id="ch03_comment_csv_mr6"/>

    job.setMapOutputKeyClass(LongWritable.class);
    job.setMapOutputValueClass(TextArrayWritable.class);

    job.setOutputKeyClass(TextArrayWritable.class);
    job.setOutputValueClass(NullWritable.class);

    FileInputFormat.setInputPaths(job, new Path(input));
    Path outPath = new Path(output);
    FileOutputFormat.setOutputPath(job, outPath);

    outPath.getFileSystem(conf).delete(outPath, true);

    job.waitForCompletion(true);
  }
}
