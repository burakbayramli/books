package com.manning.hip.ch7.pagerank;


import org.apache.commons.io.*;
import org.apache.commons.lang.StringUtils;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.lib.input.*;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

import java.io.*;
import java.util.*;

public final class Main {

  public static void main(String... args) throws Exception {

    String inputFile = args[0];
    String outputDir = args[1];

    iterate(inputFile, outputDir);
  }

  public static void iterate(String input, String output)
      throws Exception {

    Configuration conf = new Configuration();
    Path outputPath = new Path(output);
    outputPath.getFileSystem(conf).delete(outputPath, true);
    outputPath.getFileSystem(conf).mkdirs(outputPath);

    Path inputPath = new Path(outputPath, "input.txt");

    int numNodes = createInputFile(new Path(input), inputPath);

    int iter = 1;
    double desiredConvergence = 0.01;

    while (true) {

      Path jobOutputPath =
          new Path(outputPath, String.valueOf(iter));

      System.out.println("======================================");
      System.out.println("=  Iteration:    " + iter);
      System.out.println("=  Input path:   " + inputPath);
      System.out.println("=  Output path:  " + jobOutputPath);
      System.out.println("======================================");

      if (calcPageRank(inputPath, jobOutputPath, numNodes) <
          desiredConvergence) {
        System.out.println(
            "Convergence is below " + desiredConvergence +
                ", we're done");
        break;
      }
      inputPath = jobOutputPath;
      iter++;
    }
  }

  public static int createInputFile(Path file, Path targetFile)
      throws IOException {
    Configuration conf = new Configuration();
    FileSystem fs = file.getFileSystem(conf);

    int numNodes = getNumNodes(file);
    double initialPageRank = 1.0 / (double) numNodes;

    OutputStream os = fs.create(targetFile);
    LineIterator iter = IOUtils
        .lineIterator(fs.open(file), "UTF8");

    while (iter.hasNext()) {
      String line = iter.nextLine();

      String[] parts = StringUtils.split(line);

      Node node = new Node()
          .setPageRank(initialPageRank)
          .setAdjacentNodeNames(
              Arrays.copyOfRange(parts, 1, parts.length));
      IOUtils.write(parts[0] + '\t' + node.toString() + '\n', os);
    }
    os.close();
    return numNodes;
  }

  public static int getNumNodes(Path file) throws IOException {
    Configuration conf = new Configuration();
    FileSystem fs = file.getFileSystem(conf);

    return IOUtils.readLines(fs.open(file), "UTF8").size();
  }

  public static double calcPageRank(Path inputPath, Path outputPath, int numNodes)
      throws Exception {
    Configuration conf = new Configuration();
    conf.setInt(Reduce.CONF_NUM_NODES_GRAPH, numNodes);

    Job job = new Job(conf);
    job.setJarByClass(Main.class);
    job.setMapperClass(Map.class);
    job.setReducerClass(Reduce.class);

    job.setInputFormatClass(KeyValueTextInputFormat.class);

    job.setMapOutputKeyClass(Text.class);
    job.setMapOutputValueClass(Text.class);

    FileInputFormat.setInputPaths(job, inputPath);
    FileOutputFormat.setOutputPath(job, outputPath);

    if (!job.waitForCompletion(true)) {
      throw new Exception("Job failed");
    }

    long summedConvergence = job.getCounters().findCounter(
        Reduce.Counter.CONV_DELTAS).getValue();
    double convergence =
        ((double) summedConvergence /
            Reduce.CONVERGENCE_SCALING_FACTOR) /
            (double) numNodes;

    System.out.println("======================================");
    System.out.println("=  Num nodes:           " + numNodes);
    System.out.println("=  Summed convergence:  " + summedConvergence);
    System.out.println("=  Convergence:         " + convergence);
    System.out.println("======================================");

    return convergence;
  }


}
