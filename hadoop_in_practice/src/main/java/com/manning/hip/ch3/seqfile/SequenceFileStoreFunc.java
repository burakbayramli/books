package com.manning.hip.ch3.seqfile;

import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.*;
import org.apache.hadoop.io.compress.CompressionCodec;
import org.apache.hadoop.io.compress.DefaultCodec;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.OutputFormat;
import org.apache.hadoop.mapreduce.RecordWriter;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.SequenceFileOutputFormat;
import org.apache.pig.StoreFunc;
import org.apache.pig.backend.BackendException;
import org.apache.pig.data.DataByteArray;
import org.apache.pig.data.DataType;
import org.apache.pig.data.Tuple;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;

import static org.apache.pig.data.DataType.*;

/**
 * A basic SequenceFile Store Func.  Handles storing built-in
 * Writables.
 */
public class SequenceFileStoreFunc extends StoreFunc {

  private static final Logger LOG =
      LoggerFactory.getLogger(SequenceFileStoreFunc.class);

  @SuppressWarnings("rawtypes")
  protected RecordWriter writer;

  private final Class keyClass;
  private final Class valueClass;
  private final String compressionType;
  private final String compressionCodecClass;

  public SequenceFileStoreFunc(String keyClass, String valueClass)
      throws ClassNotFoundException {
    this(keyClass, valueClass, null, null);
  }

  public SequenceFileStoreFunc(String keyClass, String valueClass,
                               String compressionType,
                               String compressionCodecClass)
      throws ClassNotFoundException {
    this.compressionType = compressionType;
    this.compressionCodecClass = compressionCodecClass;
    this.keyClass = Class.forName(keyClass);
    this.valueClass = Class.forName(valueClass);
  }

  @Override
  public OutputFormat getOutputFormat() throws IOException {
    return new SequenceFileOutputFormat();
  }

  @Override
  public void setStoreLocation(String location, Job job)
      throws IOException {
    job.setOutputKeyClass(keyClass);
    job.setOutputValueClass(valueClass);
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
    if (tuple != null && tuple.size() == 2) {
      try {
        writer.write(inferWritable(tuple.get(0)), inferWritable(
            tuple.get(1)));
      } catch (InterruptedException e) {
        // Under what circumstances does this happen?
        throw new IOException(e);
      }
    }
  }

  protected Object inferWritable(Object o) throws BackendException {
    System.out.println("Got object '" + o + "' type " + o.getClass());
    switch (DataType.findType(o)) {
      case BYTEARRAY: {
        return new BytesWritable(((DataByteArray) o).get());
      }
      case CHARARRAY: {
        return new Text(o.toString());
      }
      case INTEGER: {
        return new IntWritable((Integer) o);
      }
      case LONG: {
        return new LongWritable((Long) o);
      }
      case FLOAT: {
        return new FloatWritable((Float) o);
      }
      case DOUBLE: {
        return new DoubleWritable((Double) o);
      }
      case BOOLEAN: {
        return new BooleanWritable((Boolean) o);
      }
      case BYTE: {
        return new ByteWritable((Byte) o);
      }
    }
    throw new BackendException("Unable to translate " + o.getClass() +
        " to a Writable datatype");
  }

}
