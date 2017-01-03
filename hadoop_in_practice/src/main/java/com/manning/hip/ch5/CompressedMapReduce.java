package com.manning.hip.ch5;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.compress.CompressionCodec;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;

public class CompressedMapReduce {

  public static void main(String[] args) throws Exception {
    Configuration conf = new Configuration();

    Path inputFile = new Path(args[0]);
    Path outputFile = new Path(args[1]);

    FileSystem hdfs = outputFile.getFileSystem(conf);

    hdfs.delete(outputFile, true);

    Class<?> codecClass = Class.forName(args[2]);

    conf.setBoolean("mapred.output.compress", true);
    conf.setClass("mapred.output.compression.codec",
        codecClass,
        CompressionCodec.class);

    conf.setBoolean("mapred.compress.map.output", true);
    conf.setClass("mapred.map.output.compression.codec",
        codecClass,
        CompressionCodec.class);

    Job job = new Job(conf);
    job.setJarByClass(CompressedMapReduce.class);

    job.setMapperClass(Mapper.class);
    job.setReducerClass(Reducer.class);

    job.setInputFormatClass(TextInputFormat.class);
    job.setOutputFormatClass(TextOutputFormat.class);

    FileInputFormat.addInputPath(job, inputFile);
    FileOutputFormat.setOutputPath(job, outputFile);

    job.waitForCompletion(true);
  }


}
