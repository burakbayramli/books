package com.manning.hip.ch3.avro;

import com.manning.hip.ch5.SmallFilesMapReduce;
import com.manning.hip.ch3.avro.gen.Stock;
import com.manning.hip.ch3.avro.gen.StockAvg;
import org.apache.avro.mapred.AvroInputFormat;
import org.apache.avro.mapred.AvroJob;
import org.apache.avro.mapred.AvroOutputFormat;
import org.apache.avro.mapred.AvroWrapper;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.DoubleWritable;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.io.compress.SnappyCodec;
import org.apache.hadoop.mapred.*;

import java.io.IOException;
import java.util.Iterator;

public class AvroStockMapReduce {

  public static void main(String... args) throws Exception {
    JobConf job = new JobConf();
    job.setJarByClass(SmallFilesMapReduce.class);
    Path input = new Path(args[0]);
    Path output = new Path(args[1]);

    output.getFileSystem(job).delete(output, true);

    job.set(AvroJob.INPUT_SCHEMA, Stock.SCHEMA$.toString());
    job.set(AvroJob.OUTPUT_SCHEMA, StockAvg.SCHEMA$.toString());
    job.set(AvroJob.OUTPUT_CODEC, SnappyCodec.class.getName());

    job.setInputFormat(AvroInputFormat.class);
    job.setOutputFormat(AvroOutputFormat.class);

    job.setMapperClass(Map.class);
    job.setReducerClass(Reduce.class);

    job.setMapOutputKeyClass(Text.class);
    job.setMapOutputValueClass(DoubleWritable.class);

    job.setOutputKeyClass(Text.class);
    job.setOutputValueClass(DoubleWritable.class);

    FileInputFormat.setInputPaths(job, input);
    FileOutputFormat.setOutputPath(job, output);

    JobClient.runJob(job);
  }

  public static class Map
      implements
      Mapper<AvroWrapper<Stock>, NullWritable, Text, DoubleWritable> {

    public void map(AvroWrapper<Stock> key,
                    NullWritable value,
                    OutputCollector<Text, DoubleWritable> output,
                    Reporter reporter) throws IOException {
      output.collect(new Text(key.datum().getSymbol().toString()),
          new DoubleWritable(key.datum().getOpen()));
    }

    public void close() throws IOException {
    }

    public void configure(JobConf job) {
    }
  }

  public static class Reduce
      implements Reducer<Text, DoubleWritable, AvroWrapper<StockAvg>,
      NullWritable> {

    public void reduce(Text key,
                       Iterator<DoubleWritable> values,
                       OutputCollector<AvroWrapper<StockAvg>,
                           NullWritable> output,
                       Reporter reporter) throws IOException {
      double total = 0.0;
      double count = 0;
      while (values.hasNext()) {
        total += values.next().get();
        count++;
      }
      StockAvg avg = new StockAvg();
      avg.setSymbol(key.toString());
      avg.setAvg(total / count);
      output.collect(new AvroWrapper<StockAvg>(avg),
          NullWritable.get());
    }

    public void close() throws IOException {
    }

    public void configure(JobConf job) {
    }
  }
}
