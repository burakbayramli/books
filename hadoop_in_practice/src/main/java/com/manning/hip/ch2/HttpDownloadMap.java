package com.manning.hip.ch2;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapred.*;

import java.io.*;
import java.net.*;

public final class HttpDownloadMap
    implements Mapper<LongWritable, Text, Text, Text> {
  private int file = 0;
  private Configuration conf;
  private String jobOutputDir;
  private String taskId;
  private int connTimeoutMillis =
      DEFAULT_CONNECTION_TIMEOUT_MILLIS;
  private int readTimeoutMillis = DEFAULT_READ_TIMEOUT_MILLIS;
  private final static int DEFAULT_CONNECTION_TIMEOUT_MILLIS = 5000;
  private final static int DEFAULT_READ_TIMEOUT_MILLIS = 5000;

  public static final String CONN_TIMEOUT =
      "httpdownload.connect.timeout.millis";

  public static final String READ_TIMEOUT =
      "httpdownload.read.timeout.millis";

  @Override
  public void configure(JobConf job) {
    conf = job;
    jobOutputDir = job.get("mapred.output.dir");
    taskId = conf.get("mapred.task.id");

    if (conf.get(CONN_TIMEOUT) != null) {
      connTimeoutMillis = Integer.valueOf(conf.get(CONN_TIMEOUT));
    }
    if (conf.get(READ_TIMEOUT) != null) {
      readTimeoutMillis = Integer.valueOf(conf.get(READ_TIMEOUT));
    }
  }

  @Override
  public void map(LongWritable key, Text value,
                  OutputCollector<Text, Text> output,
                  Reporter reporter) throws IOException {
    Path httpDest =
        new Path(jobOutputDir, taskId + "_http_" + (file++));

    InputStream is = null;
    OutputStream os = null;
    try {
      URLConnection connection =
          new URL(value.toString()).openConnection();
      connection.setConnectTimeout(connTimeoutMillis);
      connection.setReadTimeout(readTimeoutMillis);
      is = connection.getInputStream();

      os = FileSystem.get(conf).create(httpDest);

      IOUtils.copyBytes(is, os, conf, true);
    } finally {
      IOUtils.closeStream(is);
      IOUtils.closeStream(os);
    }

    output.collect(new Text(httpDest.toString()), value);
  }

  @Override
  public void close() throws IOException {
  }

}
