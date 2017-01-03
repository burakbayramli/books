package com.manning.hip.ch7.bloom;

import com.manning.hip.ch3.avro.AvroBytesRecord;
import org.apache.avro.generic.GenericRecord;
import org.apache.avro.mapred.*;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.*;
import org.apache.hadoop.io.compress.SnappyCodec;
import org.apache.hadoop.mapred.*;
import org.apache.hadoop.util.bloom.*;
import org.apache.hadoop.util.hash.Hash;

import java.io.IOException;
import java.util.*;

public class BloomFilterCreator {

  public static void main(String... args) throws Exception {

    JobConf job = new JobConf();
    job.setJarByClass(BloomFilterCreator.class);
    Path input = new Path(args[0]);
    Path output = new Path(args[1]);

    output.getFileSystem(job).delete(output, true);

    job.set(AvroJob.OUTPUT_SCHEMA, AvroBytesRecord.SCHEMA.toString());
    job.set(AvroJob.OUTPUT_CODEC, SnappyCodec.class.getName());

    job.setInputFormat(KeyValueTextInputFormat.class);
    job.setOutputFormat(AvroOutputFormat.class);

    job.setMapperClass(Map.class);
    job.setReducerClass(Reduce.class);

    job.setMapOutputKeyClass(NullWritable.class);
    job.setMapOutputValueClass(BloomFilter.class);

    job.setOutputKeyClass(NullWritable.class);
    job.setOutputValueClass(BloomFilter.class);

    FileInputFormat.setInputPaths(job, input);
    FileOutputFormat.setOutputPath(job, output);

    JobClient.runJob(job);
  }

  public static class Map implements
      Mapper<Text, Text, NullWritable, BloomFilter> {
    private BloomFilter filter =
        new BloomFilter(1000, 5, Hash.MURMUR_HASH);
    OutputCollector<NullWritable, BloomFilter> collector;

    @Override
    public void configure(JobConf job) {
    }

    @Override
    public void map(Text key, Text value,
                    OutputCollector<NullWritable, BloomFilter> output,
                    Reporter reporter) throws IOException {

      System.out.println("K[" + key + "]");

      int age = Integer.valueOf(value.toString());
      if (age > 30) {
        filter.add(new Key(key.toString().getBytes()));
      }
      collector = output;
    }

    @Override
    public void close() throws IOException {
      System.out.println(filter);
      collector.collect(NullWritable.get(), filter);
    }

  }

  public static class Reduce
      implements
      Reducer<NullWritable, BloomFilter, AvroWrapper<GenericRecord>,
          NullWritable> {
    private BloomFilter filter = new BloomFilter(1000, 5, Hash.MURMUR_HASH);
    OutputCollector<AvroWrapper<GenericRecord>, NullWritable>
        collector;

    @Override
    public void reduce(NullWritable key, Iterator<BloomFilter> values,
                       OutputCollector<AvroWrapper<GenericRecord>,
                           NullWritable> output,
                       Reporter reporter) throws IOException {
      while (values.hasNext()) {
        BloomFilter bf = values.next();
        filter.or(bf);
        System.out.println(filter);
      }
      collector = output;
    }

    @Override
    public void close() throws IOException {
      System.out.println(filter);
      collector.collect(
          new AvroWrapper<GenericRecord>(
              AvroBytesRecord.toGenericRecord(filter)),
          NullWritable.get());
    }

    @Override
    public void configure(JobConf job) {

    }
  }


}
