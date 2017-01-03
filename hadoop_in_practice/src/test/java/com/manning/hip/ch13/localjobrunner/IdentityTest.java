package com.manning.hip.ch13.localjobrunner;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.KeyValueTextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.junit.Test;

import java.io.DataOutputStream;
import java.io.IOException;
import java.util.List;

import static junit.framework.Assert.*;

public class IdentityTest {

  @Test
  public void run() throws Exception {
    Path inputPath = new Path("/tmp/mrtest/input");
    Path outputPath = new Path("/tmp/mrtest/output");

    Configuration conf = new Configuration();

    conf.set("mapred.job.tracker", "local");
    conf.set("fs.default.name", "file:///");

    FileSystem fs = FileSystem.get(conf);
    if (fs.exists(outputPath)) {
      fs.delete(outputPath, true);
    }
    if (fs.exists(inputPath)) {
      fs.delete(inputPath, true);
    }
    fs.mkdirs(inputPath);

    String input = "foo\tbar";
    DataOutputStream file = fs.create(new Path(inputPath, "part-" + 0));
    file.writeBytes(input);
    file.close();

    Job job = runJob(conf, inputPath, outputPath);
    assertTrue(job.isSuccessful());

    List<String> lines =
        IOUtils.readLines(fs.open(new Path(outputPath, "part-r-00000")));

    assertEquals(1, lines.size());
    String[] parts = StringUtils.split(lines.get(0), "\t");
    assertEquals("foo", parts[0]);
    assertEquals("bar", parts[1]);
  }


  public Job runJob(Configuration conf, Path inputPath, Path outputPath)
      throws ClassNotFoundException, IOException, InterruptedException {
    Job job = new Job(conf);

    job.setInputFormatClass(KeyValueTextInputFormat.class);

    job.setMapOutputKeyClass(Text.class);

    FileInputFormat.setInputPaths(job, inputPath);
    FileOutputFormat.setOutputPath(job, outputPath);

    job.waitForCompletion(false);
    return job;
  }
}
