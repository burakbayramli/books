package com.manning.hip.ch12.crunch;

import com.cloudera.crunch.*;
import com.cloudera.crunch.impl.mr.MRPipeline;
import com.cloudera.crunch.lib.Aggregate;
import com.cloudera.crunch.type.PTypeFamily;
import com.manning.hip.common.CommonLogEntry;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;

import java.io.IOException;

/**
 * Parse Apache logs, extract resources, group them and
 * then order by popularity.
 */
public class PopularLinks {
  public static void main(String[] args) throws IOException {
    Configuration conf = new Configuration();
    Path output = new Path(args[1]);
    output.getFileSystem(conf).delete(output, true);

    // Create an object to coordinate pipeline creation and execution.
    Pipeline pipeline = new MRPipeline(PopularLinks.class, conf);

    // Reference a given text file as a collection of Strings.
    PCollection<String> lines = pipeline.readTextFile(args[0]);

    // Define a function that splits each line in a PCollection of Strings into a
    // PCollection made up of the individual words in the file.
    PCollection<CommonLogEntry> logs = CrunchUtils.logs(lines);

    PCollection<String> resources = extractFilterResources(logs);

    PTable<String, Long> counts = Aggregate.count(resources);

    // Instruct the pipeline to write the resulting counts to a text file.
    pipeline.writeTextFile(counts, args[1]);
    // Execute the pipeline as a MapReduce.
    pipeline.done();
  }

  public static PCollection<String> extractFilterResources(PCollection<CommonLogEntry> logs) {
    PTypeFamily tf = logs.getTypeFamily();
    return logs.parallelDo(
        "resource-extract-filter",
        new DoFn<CommonLogEntry, String>() {
          @Override
          public void process(CommonLogEntry input, Emitter<String> emitter) {
            if (!"127.0.0.1".equals(input.getRemoteAddress())) {
              emitter.emit(input.getResource());
            }
          }
        }, tf.strings());
  }
}
