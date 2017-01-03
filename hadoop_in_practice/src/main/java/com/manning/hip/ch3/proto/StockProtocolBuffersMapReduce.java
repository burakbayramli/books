package com.manning.hip.ch3.proto;

import com.hadoop.compression.lzo.LzopCodec;
import com.manning.hip.ch3.csv.CSVParser;
import com.twitter.elephantbird.mapreduce.input.LzoProtobufBlockInputFormat;
import com.twitter.elephantbird.mapreduce.io.*;
import com.twitter.elephantbird.mapreduce.output.LzoProtobufBlockOutputFormat;
import com.twitter.elephantbird.util.TypeRef;
import org.apache.commons.io.FileUtils;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

import java.io.*;

import static com.manning.hip.ch3.proto.StockProtos.*;

public class StockProtocolBuffersMapReduce {

  public static class ProtobufStockWritable
      extends ProtobufWritable<Stock> {
    public ProtobufStockWritable() {
      super(new TypeRef<Stock>() {
      });
    }

    public ProtobufStockWritable(Stock m) {
      super(m, new TypeRef<Stock>() {
      });
    }
  }

  public static void main(String... args) throws Exception {
    File localStocksFile = new File(args[0]);
    Path input = new Path(args[1]);
    Path output = new Path(args[2]);

    if (!input.getName().endsWith(".lzo")) {
      throw new Exception("HDFS stock file must have a .lzo suffix");
    }

    Configuration conf = new Configuration();

    output.getFileSystem(conf).delete(output, true);

    generateInput(conf, localStocksFile, input);

    Job job = new Job(conf);
    job.setJobName(StockProtocolBuffersMapReduce.class.getName());

    job.setJarByClass(StockProtocolBuffersMapReduce.class);
    job.setMapperClass(PBMapper.class);
    job.setReducerClass(PBReducer.class);

    job.setMapOutputKeyClass(Text.class);
    job.setMapOutputValueClass(ProtobufStockWritable.class);

    job.setInputFormatClass(
        LzoProtobufBlockInputFormat
            .getInputFormatClass(Stock.class,
                job.getConfiguration()));

    job.setOutputFormatClass(
        LzoProtobufBlockOutputFormat.getOutputFormatClass(
            StockAvg.class, job.getConfiguration()));

    FileInputFormat.setInputPaths(job, input);
    FileOutputFormat.setOutputPath(job, output);

    job.waitForCompletion(true);
  }

  private static void generateInput(Configuration config,
                                    File inputFile,
                                    Path input) throws IOException {
    FileSystem hdfs = FileSystem.get(config);
    OutputStream os = hdfs.create(input);

    LzopCodec codec = new LzopCodec();
    codec.setConf(config);
    OutputStream lzopOutputStream = codec.createOutputStream(os);

    ProtobufBlockWriter<Stock> writer =
        new ProtobufBlockWriter<Stock>(
            lzopOutputStream, Stock.class);

    for (String line : FileUtils.readLines(inputFile)) {
      Stock stock = createStock(line);
      writer.write(stock);
    }
    writer.finish();
    writer.close();
    IOUtils.closeStream(os);
  }

  static CSVParser parser = new CSVParser();

  public static Stock createStock(String line) throws IOException {
    String parts[] = parser.parseLine(line);
    return Stock.newBuilder()
        .setSymbol(parts[0])
        .setDate(parts[1])
        .setOpen(Double.valueOf(parts[2]))
        .setHigh(Double.valueOf(parts[3]))
        .setLow(Double.valueOf(parts[4]))
        .setClose(Double.valueOf(parts[5]))
        .setVolume(Integer.valueOf(parts[6]))
        .setAdjClose(Double.valueOf(parts[7])).build();
  }


  public static class PBMapper extends
      Mapper<LongWritable, ProtobufWritable<Stock>,
          Text, ProtobufStockWritable> {
    @Override
    protected void map(LongWritable key,
                       ProtobufWritable<Stock> value,
                       Context context) throws IOException,
        InterruptedException {
      context.write(
          new Text(value.get().getSymbol()),
          new ProtobufStockWritable(value.get()));
    }
  }

  public static class PBReducer extends
      Reducer<Text, ProtobufStockWritable,
          NullWritable, ProtobufWritable> {
    private ProtobufWritable<StockAvg> stockAvg =
        new ProtobufWritable<StockAvg>();

    @Override
    protected void reduce(Text symbol,
                          Iterable<ProtobufStockWritable> values,
                          Context context) throws IOException,
        InterruptedException {
      double total = 0.0;
      double count = 0;
      for (ProtobufStockWritable d : values) {
        total += d.get().getOpen();
        count++;
      }
      StockAvg avg = StockAvg.newBuilder()
          .setSymbol(symbol.toString())
          .setAvg(total / count).build();
      stockAvg.set(avg);
      context.write(NullWritable.get(), stockAvg);
    }
  }
}
