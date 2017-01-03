package com.manning.hip.ch5;

import com.hadoop.compression.lzo.*;
import com.hadoop.compression.lzo.LzoCodec;
import com.hadoop.mapreduce.LzoTextInputFormat;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.io.compress.CompressionCodec;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.mapreduce.lib.input.*;
import org.apache.hadoop.mapreduce.lib.output.*;

import java.io.IOException;
import java.util.List;

public class LzopMapReduce {

  public static void main(String[] args) throws Exception {
    Configuration conf = new Configuration();

    Path inputFile = new Path(args[0]);
    Path compressedInputFile = compressAndIndex(inputFile, conf);
    Path outputFile = new Path(args[1]);

    FileSystem hdfs = outputFile.getFileSystem(conf);

    hdfs.delete(outputFile, true);

    conf.setBoolean("mapred.compress.map.output", true);
    conf.setClass("mapred.map.output.compression.codec",
        LzopCodec.class,
        CompressionCodec.class);


    Job job = new Job(conf);
    job.setJarByClass(LzopMapReduce.class);

    job.setMapperClass(Mapper.class);
    job.setReducerClass(Reducer.class);

    job.setInputFormatClass(LzoTextInputFormat.class);
    job.setOutputFormatClass(TextOutputFormat.class);

    job.getConfiguration().setBoolean("mapred.output.compress", true);
    job.getConfiguration().setClass("mapred.output.compression.codec",
          LzopCodec.class, CompressionCodec.class);

    FileInputFormat.addInputPath(job, compressedInputFile);
    FileOutputFormat.setOutputPath(job, outputFile);

    job.waitForCompletion(true);
  }

  public static Path compressAndIndex(Path file, Configuration conf)
      throws IOException {

    Configuration tmpConfig = new Configuration(conf);
    tmpConfig.setLong("dfs.block.size", 512);
    tmpConfig.setInt(LzoCodec.LZO_BUFFER_SIZE_KEY, 512);


    Path compressedFile = LzopFileReadWrite.compress(file, tmpConfig);

    compressedFile.getFileSystem(tmpConfig).delete(new Path(
        compressedFile.toString() + LzoIndex.LZO_INDEX_SUFFIX), false);
    new LzoIndexer(tmpConfig).index(compressedFile);

    LzoIndex index = LzoIndex
        .readIndex(compressedFile.getFileSystem(tmpConfig),
            compressedFile);
    for (int i = 0; i < index.getNumberOfBlocks(); i++) {
      System.out.println("block[" + i + "] = " + index.getPosition(i));
    }

    Job job = new Job(conf);
    job.setInputFormatClass(LzoTextInputFormat.class);
    LzoTextInputFormat inputFormat = new LzoTextInputFormat();
    TextInputFormat.setInputPaths(job, compressedFile);

    List<InputSplit> is = inputFormat.getSplits(job);

    System.out.println("input splits = " + is.size());

    return compressedFile;
  }

}
