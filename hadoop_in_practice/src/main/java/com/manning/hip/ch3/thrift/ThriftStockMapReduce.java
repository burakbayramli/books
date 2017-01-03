package com.manning.hip.ch3.thrift;

import com.hadoop.compression.lzo.LzopCodec;
import com.manning.hip.ch3.csv.CSVParser;
import com.twitter.elephantbird.mapreduce.input.LzoThriftBlockInputFormat;
import com.twitter.elephantbird.mapreduce.io.*;
import com.twitter.elephantbird.mapreduce.output.LzoThriftBlockOutputFormat;
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

public class ThriftStockMapReduce {

    public static class StockWritable
        extends ThriftWritable<Stock> {
      public StockWritable() {
        super(new TypeRef<Stock>() {
        });
      }

      public StockWritable(Stock m) {
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
      job.setJobName(ThriftStockMapReduce.class.getName());

      job.setJarByClass(ThriftStockMapReduce.class);
      job.setMapperClass(ThriftMapper.class);
      job.setReducerClass(ThriftReducer.class);

      job.setMapOutputKeyClass(Text.class);
      job.setMapOutputValueClass(StockWritable.class);

      job.setInputFormatClass(
          LzoThriftBlockInputFormat
              .getInputFormatClass(Stock.class,
                  job.getConfiguration()));

      job.setOutputFormatClass(
          LzoThriftBlockOutputFormat.getOutputFormatClass(
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

      ThriftBlockWriter<Stock> writer =
          new ThriftBlockWriter<Stock>(
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
      return new Stock()
          .setSymbol(parts[0])
          .setDate(parts[1])
          .setOpen(Double.valueOf(parts[2]))
          .setHigh(Double.valueOf(parts[3]))
          .setLow(Double.valueOf(parts[4]))
          .setClose(Double.valueOf(parts[5]))
          .setVolume(Integer.valueOf(parts[6]))
          .setAdjClose(Double.valueOf(parts[7]));
    }


    public static class ThriftMapper extends
        Mapper<LongWritable, ThriftWritable<Stock>,
            Text, StockWritable> {
      StockWritable tWritable = new StockWritable();

      @Override
      protected void map(LongWritable key,
                         ThriftWritable<Stock> stock,
                         Context context) throws IOException,
          InterruptedException {
        tWritable.set(stock.get());
        context.write(
            new Text(stock.get().getSymbol()),
            tWritable);
      }
    }

    public static class ThriftReducer extends
        Reducer<Text, StockWritable,
            NullWritable, ThriftWritable> {
      ThriftWritable<StockAvg> tWritable =
          ThriftWritable.newInstance(StockAvg.class);
      StockAvg avg = new StockAvg();

      @Override
      protected void reduce(Text symbol,
                            Iterable<StockWritable> values,
                            Context context) throws IOException,
          InterruptedException {
        double total = 0.0;
        double count = 0;
        for (ThriftWritable<Stock> d : values) {
          total += d.get().getOpen();
          count++;
        }
        avg.setSymbol(symbol.toString())
            .setAvg(total / count);
        tWritable.set(avg);
        context.write(NullWritable.get(), tWritable);
      }
    }
  }
