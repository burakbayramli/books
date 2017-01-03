package com.manning.hip.ch1;


import org.apache.commons.lang.StringUtils;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
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

public final class InvertedIndexMapReduce {
  public static void main(String... args) throws Exception {

    runJob(
        Arrays.copyOfRange(args, 0, args.length - 1),
        args[args.length - 1]);
  }

  public static void runJob(String[] input, String output)
      throws Exception {
    Configuration conf = new Configuration();

    Job job = new Job(conf);
    job.setJarByClass(InvertedIndexMapReduce.class);
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
      extends Reducer<Text, Text, Text, Text> {

    private Text docIds = new Text();
    public void reduce(Text key, Iterable<Text> values,
                       Context context)
        throws IOException, InterruptedException {

      HashSet<Text> uniqueDocIds = new HashSet<Text>();
      for (Text docId : values) {
        uniqueDocIds.add(new Text(docId));
      }
      docIds.set(new Text(StringUtils.join(uniqueDocIds, ",")));
      context.write(key, docIds);
    }
  }
}
