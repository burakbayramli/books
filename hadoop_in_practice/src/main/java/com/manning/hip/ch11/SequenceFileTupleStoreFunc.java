package com.manning.hip.ch11;

import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.SequenceFile;
import org.apache.hadoop.io.compress.CompressionCodec;
import org.apache.hadoop.io.compress.DefaultCodec;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.OutputFormat;
import org.apache.hadoop.mapreduce.RecordWriter;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.SequenceFileOutputFormat;
import org.apache.pig.StoreFunc;
import org.apache.pig.data.*;

import java.io.IOException;

/**
 * A basic SequenceFile Store Func.  Handles storing built-in
 * Writables.
 */
public class SequenceFileTupleStoreFunc extends StoreFunc {

  protected RecordWriter writer;

  private final String compressionType;
  private final String compressionCodecClass;

  public SequenceFileTupleStoreFunc() {
    this(null, null);
  }

  public SequenceFileTupleStoreFunc(String compressionType,
                                    String compressionCodecClass) {
    this.compressionType = compressionType;
    this.compressionCodecClass = compressionCodecClass;
  }

  @Override
  public OutputFormat getOutputFormat() throws IOException {
    return new SequenceFileOutputFormat();
  }

  @Override
  public void setStoreLocation(String location, Job job)
      throws IOException {
    job.setOutputKeyClass(NullWritable.class);
    job.setOutputValueClass(DefaultTuple.class);
    if (compressionType != null && compressionCodecClass != null) {
      Class<? extends CompressionCodec> codecClass =
          FileOutputFormat.getOutputCompressorClass(job,
              DefaultCodec.class);
      SequenceFileOutputFormat.
          setOutputCompressorClass(job, codecClass);
      SequenceFileOutputFormat.setOutputCompressionType(job,
          SequenceFile.CompressionType.valueOf(compressionType));
    }
    FileOutputFormat.setOutputPath(job, new Path(location));
  }

  @Override
  public void prepareToWrite(RecordWriter writer) throws IOException {
    this.writer = writer;
  }

    @Override
    public void putNext(Tuple tuple) throws IOException {
      System.out.println(tuple.getClass().getName());
      if (tuple != null) {
        try {
          Tuple t = new DefaultTuple();
          for(Object val: tuple.getAll()) {
            t.append(val);
          }
          writer.write(NullWritable.get(), t);
        } catch (InterruptedException e) {
          // Under what circumstances does this happen?
          throw new IOException(e);
        }
      }
    }
}
