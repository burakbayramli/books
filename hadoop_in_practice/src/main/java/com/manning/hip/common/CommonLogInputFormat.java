package com.manning.hip.common;

import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.compress.CompressionCodec;
import org.apache.hadoop.io.compress.CompressionCodecFactory;
import org.apache.hadoop.mapreduce.InputSplit;
import org.apache.hadoop.mapreduce.JobContext;
import org.apache.hadoop.mapreduce.RecordReader;
import org.apache.hadoop.mapreduce.TaskAttemptContext;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.LineRecordReader;

import java.io.IOException;

/**
 * Assumes one line per log entry object
 */
public class CommonLogInputFormat
  extends FileInputFormat<LongWritable, CommonLogEntry> {

  @Override
  public RecordReader<LongWritable, CommonLogEntry> createRecordReader(
    InputSplit split,
    TaskAttemptContext
      context) {
    return new CommonLogRecordReader();
  }

  @Override
  protected boolean isSplitable(JobContext context, Path file) {
    CompressionCodec codec =
      new CompressionCodecFactory(context.getConfiguration())
        .getCodec(file);
    return codec == null;
  }

  public static class CommonLogRecordReader
    extends RecordReader<LongWritable, CommonLogEntry> {

    private LineRecordReader reader = new LineRecordReader();
    private ApacheCommonLogReader logReader = new ApacheCommonLogReader();

    private CommonLogEntry value_ = new CommonLogEntry();

    @Override
    public void initialize(InputSplit split,
                           TaskAttemptContext context)
      throws IOException, InterruptedException {
      reader.initialize(split, context);
    }

    @Override
    public synchronized void close() throws IOException {
      reader.close();
    }

    @Override
    public LongWritable getCurrentKey() throws IOException,
      InterruptedException {
      return reader.getCurrentKey();
    }

    @Override
    public CommonLogEntry getCurrentValue() throws IOException,
      InterruptedException {
      return value_;
    }

    @Override
    public float getProgress()
      throws IOException, InterruptedException {
      return reader.getProgress();
    }

    @Override
    public boolean nextKeyValue()
      throws IOException, InterruptedException {
      while (reader.nextKeyValue()) {
        if ((value_ = logReader.decodeLine(reader.getCurrentValue())) != null) {
          return true;
        }
      }
      return false;
    }

  }
}
