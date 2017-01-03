package com.manning.hip.ch3.binary;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FSDataInputStream;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.BytesWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.mapreduce.InputSplit;
import org.apache.hadoop.mapreduce.RecordReader;
import org.apache.hadoop.mapreduce.TaskAttemptContext;
import org.apache.hadoop.mapreduce.lib.input.FileSplit;

import java.io.DataInputStream;
import java.io.IOException;

public class CustomBinaryRecordReader
    extends RecordReader<LongWritable, BytesWritable> {

  private DataInputStream in;

  private LongWritable key;
  private BytesWritable value;

  private long start;
  private long end;
  private long pos;

  @Override
  public void initialize(InputSplit genericSplit,
                         TaskAttemptContext context)
      throws IOException, InterruptedException {
    FileSplit split = (FileSplit) genericSplit;
    Configuration job = context.getConfiguration();

    System.out.println("Start = " + split.getStart());
    System.out.println("Length = " + split.getLength());

    start = split.getStart();
    end = start + split.getLength();
    final Path file = split.getPath();

    FileSystem fs = file.getFileSystem(job);
    FSDataInputStream fileIn = fs.open(split.getPath());
    fileIn.seek(start);
    in = new DataInputStream(fileIn);
    this.pos = start;
  }

  @Override
  public boolean nextKeyValue()
      throws IOException, InterruptedException {

    System.out.println("nextKeyValue with pos " + pos);
    if(pos >= end) {
      key = null;
      value = null;
      return false;
    }

    if (key == null) {
      key = new LongWritable();
    }
    key.set(pos);
    if (value == null) {
      value = new BytesWritable();
    }

    int len = in.readInt();
    System.out.println("len = " + len);
    byte[] data = new byte[len];
    int read = in.read(data);
    System.out.println("read = " + read);
    value.set(data, 0, data.length);

    pos += 4 + len;

    return true;
  }

  @Override
  public LongWritable getCurrentKey()
      throws IOException, InterruptedException {
    return key;
  }

  @Override
  public BytesWritable getCurrentValue()
      throws IOException, InterruptedException {
    return value;
  }

  @Override
  public float getProgress()
      throws IOException, InterruptedException {
    if (start == end) {
      return 0.0f;
    } else {
      return Math.min(1.0f, (pos - start) / (float)(end - start));
    }
  }

  @Override
  public void close() throws IOException {
    if (in != null) {
      in.close();
    }
  }
}
