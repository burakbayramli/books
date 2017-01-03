package com.manning.hip.ch3.csv;

import com.manning.hip.ch3.TextArrayWritable;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FSDataOutputStream;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.io.Writable;
import org.apache.hadoop.io.compress.CompressionCodec;
import org.apache.hadoop.io.compress.GzipCodec;
import org.apache.hadoop.mapreduce.RecordWriter;
import org.apache.hadoop.mapreduce.TaskAttemptContext;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;
import org.apache.hadoop.util.ReflectionUtils;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;

public class CSVOutputFormat extends
    TextOutputFormat<TextArrayWritable, NullWritable> {

  public static String CSV_TOKEN_SEPARATOR_CONFIG =
      "csvoutputformat.token.delimiter";

  @Override
  public RecordWriter getRecordWriter(TaskAttemptContext job)
      throws IOException, InterruptedException {
    Configuration conf = job.getConfiguration();
    boolean isCompressed = getCompressOutput(job);
    String
        keyValueSeparator =
        conf.get(CSV_TOKEN_SEPARATOR_CONFIG, ",");
    CompressionCodec codec = null;
    String extension = "";
    if (isCompressed) {
      Class<? extends CompressionCodec> codecClass =
          getOutputCompressorClass(job, GzipCodec.class);
      codec = ReflectionUtils.newInstance(codecClass, conf);
      extension = codec.getDefaultExtension();
    }
    Path file = getDefaultWorkFile(job, extension);
    FileSystem fs = file.getFileSystem(conf);
    if (!isCompressed) {
      FSDataOutputStream fileOut = fs.create(file, false);
      return new CSVRecordWriter(fileOut,
          keyValueSeparator);
    } else {
      FSDataOutputStream fileOut = fs.create(file, false);
      return new CSVRecordWriter(
          new DataOutputStream(codec.createOutputStream(fileOut)),
          keyValueSeparator);
    }
  }

  protected static class CSVRecordWriter
      extends RecordWriter<TextArrayWritable, NullWritable> {
    private static final String utf8 = "UTF-8";
    private static final byte[] newline;

    static {
      try {
        newline = "\n".getBytes(utf8);
      } catch (UnsupportedEncodingException uee) {
        throw new IllegalArgumentException("can't find " +
            utf8 +
            " encoding");
      }
    }

    protected DataOutputStream out;
    private final String csvSeparator;

    public CSVRecordWriter(DataOutputStream
                               out, String csvSeparator) {
      this.out = out;
      this.csvSeparator = csvSeparator;
    }

    @Override
    public void write(TextArrayWritable key, NullWritable value)
        throws IOException, InterruptedException {
      if (key == null) {
        return;
      }
      boolean first = true;
      for (Writable field : key.get()) {
        writeObject(first, field);
        first = false;
      }
      out.write(newline);
    }

    /**
     * Write the object to the byte stream, handling Text as a special
     * case.
     *
     * @param o the object to print
     * @throws IOException if the write throws, we pass it on
     */
    private void writeObject(boolean first, Writable o) throws IOException {

      if(!first) {
        out.write(csvSeparator.getBytes(utf8));
      }

      boolean encloseQuotes = false;
      if (o.toString().contains(csvSeparator)) {
        encloseQuotes = true;
      }

      if(encloseQuotes) {
        out.write("\"".getBytes(utf8));
      }
        if (o instanceof Text) {
          Text to = (Text) o;
          out.write(to.getBytes(), 0, to.getLength());
        } else {
          out.write(o.toString().getBytes(utf8));
        }
      if(encloseQuotes) {
        out.write("\"".getBytes(utf8));
      }
    }
    public synchronized void close(TaskAttemptContext context)
        throws IOException {
      out.close();
    }
  }

}
