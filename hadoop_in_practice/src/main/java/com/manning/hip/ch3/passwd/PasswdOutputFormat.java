package com.manning.hip.ch3.passwd;

import org.apache.commons.lang.StringUtils;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FSDataOutputStream;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.compress.CompressionCodec;
import org.apache.hadoop.io.compress.GzipCodec;
import org.apache.hadoop.mapreduce.RecordWriter;
import org.apache.hadoop.mapreduce.TaskAttemptContext;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.util.ReflectionUtils;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;

/**
 * An {@link org.apache.hadoop.mapreduce.OutputFormat} that writes
 * files in /etc/passwd format.
 */
public class PasswdOutputFormat<K, V>
    extends FileOutputFormat {

  protected static class PasswdRecordWriter<K, V>
      extends RecordWriter<K, V> {
    private static final String utf8 = "UTF-8";
    private static final byte[] newline;
    private static final byte[] separator;

    static {
      try {
        newline = "\n".getBytes(utf8);
        separator =
        PasswdInputFormat.PasswdRecordReader.
            PASSWD_LINE_SEPARATOR.getBytes(utf8);
      } catch (UnsupportedEncodingException uee) {
        throw new IllegalArgumentException("can't find " +
            utf8 +
            " encoding");
      }
    }

    protected DataOutputStream out;

    public PasswdRecordWriter(DataOutputStream out) {
      this.out = out;
    }

    /**
     * Write the object to the byte stream, handling Text as a special
     * case.
     *
     * @param o the object to print
     * @throws java.io.IOException if the write throws, we pass it on
     */
    private void writeObject(Passwd o) throws IOException {
      out.write(o.getUsername().getBytes(utf8));
      out.write(separator);
      out.write(o.getPassword().getBytes(utf8));
      out.write(separator);
      writeIfNotNull(o.getUid());
      out.write(separator);
      writeIfNotNull(o.getGid());
      out.write(separator);
      writeIfNotNull(o.getUidInfo());
      out.write(separator);
      writeIfNotNull(o.getHomeDir());
      out.write(separator);
      writeIfNotNull(o.getShell());

    }

    private void writeIfNotNull(String s)
        throws IOException {
      if(StringUtils.isNotBlank(s)) {
        out.write(s.getBytes(utf8));
      }
    }

    private void writeIfNotNull(Long l)
        throws IOException {
      if(l != null) {
        out.write(l.toString().getBytes(utf8));
      }
    }

    public synchronized void write(K key, V value)
        throws IOException {

      boolean nullKey = key == null || key instanceof NullWritable;
      boolean nullValue = value == null || value instanceof NullWritable;
      if (nullKey && nullValue) {
        return;
      }
      if(!nullKey && key instanceof Passwd) {
        writeObject((Passwd) key);
        out.write(newline);
      } else if(!nullValue && value instanceof Passwd) {
        writeObject((Passwd) value);
        out.write(newline);
      }
    }

    public synchronized void close(TaskAttemptContext context)
        throws IOException {
      out.close();
    }
  }

  public RecordWriter<K, V>
  getRecordWriter(TaskAttemptContext job
  ) throws IOException, InterruptedException {
    Configuration conf = job.getConfiguration();
    boolean isCompressed = getCompressOutput(job);
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
      return new PasswdRecordWriter<K, V>(fileOut);
    } else {
      FSDataOutputStream fileOut = fs.create(file, false);
      return new PasswdRecordWriter<K, V>(new DataOutputStream
          (codec.createOutputStream(fileOut)));
    }
  }

}

