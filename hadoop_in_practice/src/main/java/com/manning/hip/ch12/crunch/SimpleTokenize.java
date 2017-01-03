package com.manning.hip.ch12.crunch;

import com.cloudera.crunch.*;
import com.cloudera.crunch.impl.mr.MRPipeline;
import com.cloudera.crunch.type.writable.Writables;
import org.apache.commons.lang.StringUtils;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;

import java.io.IOException;

public class SimpleTokenize {
  public static void main(String[] args) throws IOException {
    Configuration conf = new Configuration();
    Path output = new Path(args[1]);
    output.getFileSystem(conf).delete(output, true);

    Pipeline pipeline = new MRPipeline(SimpleTokenize.class, conf);

    PCollection<String> lines = pipeline.readTextFile(args[0]);

    PCollection<String> words = lines.parallelDo(
        "tokenize",
        new DoFn<String, String>() {
          @Override
          public void process(String line,
                              Emitter<String> emitter) {
            for (String word : StringUtils.split(line)) {
              emitter.emit(word);
            }
          }
        }, Writables.strings()); // Indicates the serialization format

    pipeline.writeTextFile(words, args[1]);

    pipeline.done();
  }
}
