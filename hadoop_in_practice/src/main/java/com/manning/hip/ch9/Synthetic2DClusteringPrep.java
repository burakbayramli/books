package com.manning.hip.ch9;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.io.SequenceFile;
import org.apache.hadoop.io.compress.DefaultCodec;
import org.apache.mahout.math.DenseVector;
import org.apache.mahout.math.VectorWritable;

import java.io.File;
import java.io.IOException;

public class Synthetic2DClusteringPrep {

  public static void main(String... args) throws IOException {
    write(new File(args[0]), new Path(args[1]));
  }

  public static void write(File inputFile, Path outputPath)
      throws IOException {
    Configuration conf = new Configuration();
    FileSystem fs = FileSystem.get(conf);

    SequenceFile.Writer writer =
        SequenceFile.createWriter(fs, conf, outputPath, NullWritable.class,
            VectorWritable.class,
            SequenceFile.CompressionType.BLOCK,
            new DefaultCodec());
    try {
      for (String line : FileUtils.readLines(inputFile)) {
        String parts[] = StringUtils.split(line);

        writer.append(NullWritable.get(),
            new VectorWritable(new DenseVector(
                new double[]{
                    Double.valueOf(parts[0]),
                    Double.valueOf(parts[1])
                }
            )));
      }
    } finally {
      writer.close();
    }
  }
}
