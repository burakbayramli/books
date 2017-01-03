package com.manning.hip.ch7.bloom;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.filecache.DistributedCache;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.mapreduce.lib.input.*;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.util.bloom.*;

import java.io.*;

public class BloomJoin {
  public static void main(String... args) throws Exception {
    runJob(args[0], new Path(args[1]), new Path(args[2]));
  }

  public static void runJob(String inputPath,
                            Path outputPath,
                            Path bloomFilterPath)
      throws Exception {

    Configuration conf = new Configuration();

    DistributedCache.addCacheFile(bloomFilterPath.toUri(), conf);

    Job job = new Job(conf);

    job.setJarByClass(BloomJoin.class);
    job.setMapperClass(Map.class);

    job.setInputFormatClass(KeyValueTextInputFormat.class);

    job.setNumReduceTasks(0);

    outputPath.getFileSystem(conf).delete(outputPath, true);

    FileInputFormat.setInputPaths(job, inputPath);
    FileOutputFormat.setOutputPath(job, outputPath);

    job.waitForCompletion(true);
  }

  public static class Map extends Mapper<Text, Text, Text, Text> {
    BloomFilter filter;

    @Override
    protected void setup(
        Context context)
        throws IOException, InterruptedException {

      Path[] files = DistributedCache.getLocalCacheFiles(context.getConfiguration());
      filter = BloomFilterDumper.fromFile(
          new File(files[0].toString()));

      System.out.println("Filter = " + filter);
    }

    @Override
    protected void map(Text key, Text value, Context context)
        throws IOException, InterruptedException {
      System.out.println("K[" + key + "]");
      if(filter.membershipTest(new Key(key.toString().getBytes()))) {
        context.write(key, value);
      }
    }
  }

}
