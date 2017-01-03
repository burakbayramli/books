package com.manning.hip.ch3.passwd;

import org.apache.commons.lang.StringUtils;
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
 * An {@link org.apache.hadoop.mapreduce.InputFormat} for
 * {@code etc/passwd } plain text files.
 * <p/>
 * Keys are byte offsets in the file, and values
 * are {@link Passwd} objects.
 */
public class PasswdInputFormat extends
    FileInputFormat<LongWritable, Passwd> {

  @Override
  public RecordReader<LongWritable, Passwd>
  createRecordReader(InputSplit split,
                     TaskAttemptContext context) {
    return new PasswdRecordReader();
  }

  @Override
  protected boolean isSplitable(JobContext context, Path file) {
    CompressionCodec codec =
        new CompressionCodecFactory(context.getConfiguration())
            .getCodec(file);
    return codec == null;
  }

  public static class PasswdRecordReader
      extends RecordReader<LongWritable, Passwd> {
    public final static String PASSWD_LINE_SEPARATOR = ":";
    private LineRecordReader reader = new LineRecordReader();
    private Passwd value;

    @Override
    public void initialize(InputSplit split,
                           TaskAttemptContext context)
        throws IOException, InterruptedException {
      reader.initialize(split, context);
    }

    @Override
    public boolean nextKeyValue()
        throws IOException, InterruptedException {
      if (reader.nextKeyValue()) {
        parseLine();
        return true;
      } else {
        value = null;
        return false;
      }
    }

    private void parseLine() {
      String line = reader.getCurrentValue().toString();
      String[] tokens =
          StringUtils.splitPreserveAllTokens(line,
              PASSWD_LINE_SEPARATOR);
      value = new Passwd(
          StringUtils.trimToNull(tokens[0]),
          StringUtils.trimToNull(tokens[1]),
          StringUtils.trimToNull(tokens[2]) == null ?
              null : Long.valueOf(tokens[2]),
          StringUtils.trimToNull(tokens[3]) == null ?
              null : Long.valueOf(tokens[3]),
          StringUtils.trimToNull(tokens[4]),
          StringUtils.trimToNull(tokens[5]),
          StringUtils.trimToNull(tokens[6])
      );
    }

    @Override
    public LongWritable getCurrentKey()
        throws IOException, InterruptedException {
      return reader.getCurrentKey();
    }

    @Override
    public Passwd getCurrentValue()
        throws IOException, InterruptedException {
      return value;
    }

    @Override
    public float getProgress()
        throws IOException, InterruptedException {
      return reader.getProgress();
    }

    @Override
    public void close() throws IOException {
      reader.close();
    }
  }

}
