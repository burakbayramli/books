package com.manning.hip.ch13.localjobrunner;

import org.apache.commons.io.IOUtils;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileStatus;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.fs.PathFilter;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import static junit.framework.Assert.*;

public class TextIOJobBuilder {

  protected Configuration config = new Configuration();
  protected List<String> inputs = new ArrayList<String>();
  protected List<String> expectedOutputs = new ArrayList<String>();
  protected String inputSeparator = "\t";
  protected String outputSeparator = "\t";
  protected Path inputPath = new Path("/tmp/mrtest/input");
  protected Path outputPath = new Path("/tmp/mrtest/output");
  protected FileSystem fs;

  public TextIOJobBuilder() throws IOException {
    this(new Configuration());
  }

  public TextIOJobBuilder(Configuration config) throws IOException {
    config.set("mapred.job.tracker", "local");
    config.set("fs.default.name", "file:///");
    fs = FileSystem.get(config);
  }

  public Configuration getConfig() {
    return config;
  }

  public TextIOJobBuilder setInputPath(Path inputPath) {
    this.inputPath = inputPath;
    return this;
  }

  public TextIOJobBuilder setOutputPath(Path outputPath) {
    this.outputPath = outputPath;
    return this;
  }

  public TextIOJobBuilder setInputSeparator(String separator) {
    this.inputSeparator = separator;
    return this;
  }

  public TextIOJobBuilder setOutputSeparator(String separator) {
    this.outputSeparator = separator;
    return this;
  }

  public TextIOJobBuilder addInput(String line) {
    inputs.add(line);
    return this;
  }

  public TextIOJobBuilder addInput(String key, String value) {
    inputs.add(key + inputSeparator + value);
    return this;
  }

  public TextIOJobBuilder addExpectedOutput(String line) {
    expectedOutputs.add(line);
    return this;
  }

  public TextIOJobBuilder addExpectedOutput(String key, String value) {
    expectedOutputs.add(key + inputSeparator + value);
    return this;
  }

  public TextIOJobBuilder writeInputs()
      throws IOException {

    if (fs.exists(outputPath)) {
      fs.delete(outputPath, true);
    }
    if (fs.exists(inputPath)) {
      fs.delete(inputPath, true);
    }
    fs.mkdirs(inputPath);

    DataOutputStream stream = fs.create(new Path(inputPath, "part-0"));

    IOUtils.writeLines(inputs, String.format("%n"), stream);

    stream.close();

    return this;
  }

  public TextIOJobBuilder verifyResults() throws IOException {

    FileStatus[] outputFiles = fs.listStatus(outputPath, new PathFilter() {
      @Override
      public boolean accept(Path path) {
        return path.getName().startsWith("part");
      }
    });

    int i=0;
    for(FileStatus file: outputFiles) {
      List<String> actualLines = readLines(fs, file.getPath());
      assertTrue(actualLines.size() <= expectedOutputs.size() - i);

      for(String actualLine: actualLines) {
        String expectedLine = expectedOutputs.get(i++);
        assertEquals(expectedLine, actualLine);
      }
    }

    return this;
  }

  public static List<String> readLines(FileSystem fs, Path p)
      throws IOException {
    InputStream stream = fs.open(p);
    List<String> lines = IOUtils.readLines(stream);
    stream.close();
    return lines;
  }

  public Path getInputPath() {
    return inputPath;
  }

  public Path getOutputPath() {
    return outputPath;
  }
}
