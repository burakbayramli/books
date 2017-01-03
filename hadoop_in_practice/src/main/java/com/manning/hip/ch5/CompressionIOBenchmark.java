package com.manning.hip.ch5;

import org.apache.commons.io.IOUtils;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.io.compress.CompressionCodec;
import org.apache.hadoop.util.ReflectionUtils;

import java.io.*;

public class CompressionIOBenchmark {

  public static final String[] codecs = new String[]{
      "org.apache.hadoop.io.compress.DeflateCodec",
      "org.apache.hadoop.io.compress.GzipCodec",
      "com.hadoop.compression.lzo.LzoCodec",
      "com.hadoop.compression.lzo.LzopCodec",
      "org.apache.hadoop.io.compress.SnappyCodec",
      "org.apache.hadoop.io.compress.BZip2Codec"
  };

  public static void main(String... args) throws Exception {

    File textSrcFile = new File(args[0]);
    File binarySrcFile = new File(args[1]);
    File destFile = new File(args[2]);
    File uncompressedDestFile = new File(args[3]);
    int runs = Integer.valueOf(args[4]);

    Configuration conf = new Configuration();

    for (String codec : codecs) {
      preload(conf, codec);
    }

    dumpHeader();

    for (String codec : codecs) {
      test(conf, codec, textSrcFile, destFile, uncompressedDestFile, false, runs, false);
      //test(conf, codec, binarySrcFile, destFile, uncompressedDestFile, true, runs);
    }

  }

  public static void preload(Configuration conf,
                          String codecClazz)
      throws ClassNotFoundException {
    Class<?> codecClass = Class.forName(codecClazz);
    CompressionCodec codec = (CompressionCodec)
        ReflectionUtils.newInstance(codecClass, conf);
  }

  public static void test(Configuration conf,
                          String codecClazz,
                          File srcFile,
                          File destFile,
                          File uncompressedDestFile,
                          boolean binary,
                          int runs,
                          boolean trial)
      throws
      ClassNotFoundException,
      IllegalAccessException,
      InstantiationException, IOException {

    Class<?> codecClass = Class.forName(codecClazz);
    CompressionCodec codec = (CompressionCodec)
        ReflectionUtils.newInstance(codecClass, conf);

    int accumulatedCompressMillis = 0;
    int accumulatedDecompressMillis = 0;
    for(int i=0; i < runs; i++) {
      System.err.println(codecClass + " run " + (i+1) + "/" + runs);
      long start = System.currentTimeMillis();
      compress(srcFile, destFile, codec);
      accumulatedCompressMillis += System.currentTimeMillis() - start;

      start = System.currentTimeMillis();
      decompress(destFile, uncompressedDestFile, codec);
      accumulatedDecompressMillis += System.currentTimeMillis() - start;
    }

    if(!trial) {
      dumpStats(codec,
          runs,
          binary,
          accumulatedCompressMillis / runs,
          accumulatedDecompressMillis / runs,
          destFile.length(),
          srcFile.length());
    }
  }

  public static void dumpHeader() {
    System.out.printf("%-50s %5s %8s %12s %12s %12s %12s %11s\n",
        "codec",
        "runs",
        "type",
        "comp time",
        "decomp time",
        "orig size",
        "comp size",
        "comp per");

  }

  public static void dumpStats(
      CompressionCodec codec,
      int runs,
      boolean binaryFile,
      long compressionMillis,
      long decompressionMillis,
      long compressedFileSize,
      long originalFileSize) {
    System.out.printf("%-50s %5d %8s %12d %12d %12d %12d  %10.2f\n",
        codec.getClass().getName(),
        runs,
        binaryFile ? "binary":"ascii",
        compressionMillis,
        decompressionMillis,
        originalFileSize,
        compressedFileSize,
        100.0 - (double) compressedFileSize * 100 / (double) originalFileSize
        );
  }

  public static void compress(File src, File dest,
                              CompressionCodec codec)
      throws IOException {
    InputStream is = null;
    OutputStream os = null;
    try {
      is = new FileInputStream(src);
      os = codec.createOutputStream(new FileOutputStream(dest));

      IOUtils.copy(is, os);
    } finally {
      IOUtils.closeQuietly(os);
      IOUtils.closeQuietly(is);
    }
  }

  public static void decompress(File src, File dest,
                                CompressionCodec codec)
      throws IOException {
    InputStream is = null;
    OutputStream os = null;
    try {
      is = codec.createInputStream(new FileInputStream(src));
      os = new FileOutputStream(dest);

      IOUtils.copy(is, os);
    } finally {
      IOUtils.closeQuietly(os);
      IOUtils.closeQuietly(is);
    }
  }
}
