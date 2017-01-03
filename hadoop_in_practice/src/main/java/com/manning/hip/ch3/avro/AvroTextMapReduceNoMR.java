package com.manning.hip.ch3.avro;

import com.manning.hip.ch5.SmallFilesMapReduce;
import com.manning.hip.ch5.SmallFilesWrite;
import org.apache.avro.Schema;
import org.apache.avro.file.*;
import org.apache.avro.generic.*;
import org.apache.avro.io.*;
import org.apache.avro.mapred.*;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapred.*;

import java.io.*;
import java.nio.ByteBuffer;

public class AvroTextMapReduceNoMR {

  public static final String[] LINES = new String[]{
      "the quick brown fox jumps over the lazy dog",
      "the cow jumps over the moon",
      "the rain in spain falls mainly on the plains"
  };

  public static void writeLinesBytesFile(OutputStream os)
      throws IOException {
    DatumWriter<ByteBuffer>
        writer = new GenericDatumWriter<ByteBuffer>();
    DataFileWriter<ByteBuffer> out =
        new DataFileWriter<ByteBuffer>(writer);
    out.create(Schema.create(Schema.Type.BYTES), os);
    for (String line : LINES) {
      out.append(ByteBuffer.wrap(line.getBytes("UTF-8")));
    }
    out.close();
  }

  /**
   * Uses default mapper with no reduces for a map-only identity job.
   */
  public static void main(String... args) throws Exception {
    JobConf job = new JobConf();
    job.setJarByClass(SmallFilesMapReduce.class);
    Path input = new Path(args[0]);
    Path output = new Path(args[1]);

    output.getFileSystem(job).delete(output, true);

    FileSystem hdfs = FileSystem.get(job);
    OutputStream os = hdfs.create(input);
    writeLinesBytesFile(os);

    FileInputFormat.setInputPaths(job, input);
    FileOutputFormat.setOutputPath(job, output);

    job.setInputFormat(AvroAsTextInputFormat.class);
    job.setOutputFormat(AvroTextOutputFormat.class);
    job.setOutputKeyClass(Text.class);

    JobClient.runJob(job);

    validateSortedFile(output.getFileSystem(job)
        .open(new Path(output, "part-00000.avro")));
  }

   public static class Mapper
      extends AvroMapper<GenericRecord, Pair<Void, Void>> {   //<co id="ch03_smallfilemr_comment3"/>
    @Override
    public void map(GenericRecord r,
                    AvroCollector<Pair<Void, Void>> collector,
                    Reporter reporter) throws IOException {
      System.out.println(
          r.get(SmallFilesWrite.FIELD_FILENAME) +      //<co id="ch03_smallfilemr_comment4"/>
              ": " +
              DigestUtils.md5Hex(
                  ((ByteBuffer) r.get(SmallFilesWrite.FIELD_CONTENTS))
                      .array()));
    }
  }

  public static void validateSortedFile(InputStream is)
      throws Exception {
    DatumReader<ByteBuffer>
        reader = new GenericDatumReader<ByteBuffer>();
    DataFileStream<ByteBuffer> lines =
        new DataFileStream<ByteBuffer>(is, reader);

    for (ByteBuffer line : lines) {
      byte[] b = new byte[line.remaining()];
      line.get(b);
      System.out.println(new String(b, "UTF-8").trim());
    }

    is.close();
  }
}
