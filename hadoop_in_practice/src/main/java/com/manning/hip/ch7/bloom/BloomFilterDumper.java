package com.manning.hip.ch7.bloom;

import com.manning.hip.ch3.avro.AvroBytesRecord;
import org.apache.avro.file.DataFileStream;
import org.apache.avro.generic.*;
import org.apache.commons.io.*;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.util.bloom.BloomFilter;

import java.io.*;

public class BloomFilterDumper {

  public static BloomFilter readFromAvro(InputStream is) throws IOException {
    DataFileStream<Object> reader =
        new DataFileStream<Object>(
            is, new GenericDatumReader<Object>());

    reader.hasNext();
    BloomFilter filter = new BloomFilter();
    AvroBytesRecord
        .fromGenericRecord((GenericRecord) reader.next(), filter);
    IOUtils.closeQuietly(is);
    IOUtils.closeQuietly(reader);

    return filter;
  }

  public static BloomFilter fromFile(File f) throws IOException {
    return readFromAvro(FileUtils.openInputStream(f));
  }

  public static BloomFilter fromPath(Configuration config, Path path) throws IOException {
    FileSystem hdfs = path.getFileSystem(config);

    return readFromAvro(hdfs.open(path));
  }

  public static void main(String... args) throws Exception {
    Configuration config = new Configuration();
    FileSystem hdfs = FileSystem.get(config);

    Path destFile = new Path(args[0]);

    InputStream is = hdfs.open(destFile);
    System.out.println(readFromAvro(is));
  }
}
