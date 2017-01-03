package com.manning.hip.ch12.crunch;

import com.cloudera.crunch.*;
import com.cloudera.crunch.impl.mr.MRPipeline;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;

import java.io.IOException;

public class InvertedIndex {
  public static void main(String[] args) throws IOException {
    Configuration conf = new Configuration();
    Path output = new Path(args[1]);
    output.getFileSystem(conf).delete(output, true);

    // Create an object to coordinate pipeline creation and execution.
    Pipeline pipeline = new MRPipeline(InvertedIndex.class, conf);

    // Reference a given text file as a collection of Strings.
    PCollection<String> lines = pipeline.readTextFile(args[0]);

    // Define a function that splits each line in a PCollection of Strings into a
    // PCollection made up of the individual words in the file.
    PTable<String, String> wordDocs = CrunchUtils.extractWordFileTable(lines);

    PTable<String, String> result = CrunchUtils.uniqueValues(wordDocs);

    // Instruct the pipeline to write the resulting counts to a text file.
    pipeline.writeTextFile(result, args[1]);
    // Execute the pipeline as a MapReduce.
    pipeline.done();
  }
}
