package com.manning.hip.ch11;


import org.apache.commons.lang.StringUtils;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.mapreduce.lib.input.*;
import org.apache.hadoop.mapreduce.lib.output.*;

import java.io.IOException;
import java.util.*;

public final class LogMapReduce {

  public static class Map extends Mapper<Text, Text,
      Text, Text> {
    private Text outputKey = new Text();
    @Override
    protected void map(Text key, Text value,
                       Context context)
        throws
        IOException, InterruptedException {

      String parts[] = StringUtils.split(value.toString());

      outputKey.set(parts[1]);
      context.write(outputKey, key);
    }
  }

  public static class Reduce
      extends Reducer<Text, Text, Text, Text> {
    private Text outputVal = new Text();
    public void reduce(Text key, Iterable<Text> values,
                       Context context)
        throws IOException, InterruptedException {
      Set<String> ips = new HashSet<String>();
      for (Text val : values) {
        ips.add(val.toString());
      }
      outputVal.set(StringUtils.join(ips, ":"));
      context.write(key, outputVal);
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
    job.setJarByClass(LogMapReduce.class);
    job.setMapperClass(Map.class);
    job.setReducerClass(Reduce.class);
    job.setInputFormatClass(KeyValueTextInputFormat.class);
    job.setOutputFormatClass(TextOutputFormat.class);

    job.setMapOutputKeyClass(Text.class);
    job.setMapOutputValueClass(Text.class);

    job.setOutputKeyClass(Text.class);
    job.setOutputValueClass(Text.class);

    FileInputFormat.setInputPaths(job, new Path(input));
    Path outPath = new Path(output);
    FileOutputFormat.setOutputPath(job, outPath);

    outPath.getFileSystem(conf).delete(outPath, true);

    job.waitForCompletion(true);
  }
}
