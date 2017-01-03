package com.manning.hip.ch5;

import org.apache.avro.generic.GenericRecord;
import org.apache.avro.mapred.*;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapred.*;

import java.io.IOException;
import java.nio.ByteBuffer;

public class SmallFilesMapReduce {

  public static void main(String... args) throws Exception {
    JobConf job = new JobConf();
    job.setJarByClass(SmallFilesMapReduce.class);
    Path input = new Path(args[0]);
    Path output = new Path(args[1]);

    output.getFileSystem(job).delete(output, true);

    job.set(AvroJob.INPUT_SCHEMA, SmallFilesWrite.SCHEMA.toString());

    job.setInputFormat(AvroInputFormat.class);

    job.setOutputFormat(TextOutputFormat.class);

    job.setMapperClass(Map.class);
    FileInputFormat.setInputPaths(job, input);
    FileOutputFormat.setOutputPath(job, output);

    job.setNumReduceTasks(0);

    JobClient.runJob(job);
  }

   public static class Map
      implements
      Mapper<AvroWrapper<GenericRecord>, NullWritable, Text, Text> {

     private Text outKey = new Text();
     private Text outValue = new Text();

    public void map(AvroWrapper<GenericRecord> key,
                    NullWritable value,
                    OutputCollector<Text, Text> output,
                    Reporter reporter) throws IOException {
      outKey.set(
        key.datum().get(SmallFilesWrite.FIELD_FILENAME).toString());
      outValue.set(DigestUtils.md5Hex(
            ((ByteBuffer) key.datum().get(SmallFilesWrite.FIELD_CONTENTS))
              .array()));

      output.collect(outKey, outValue);
    }

    public void close() throws IOException {
    }

    public void configure(JobConf job) {
    }
  }
}