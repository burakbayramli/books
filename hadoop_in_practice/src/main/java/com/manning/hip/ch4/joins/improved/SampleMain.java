package com.manning.hip.ch4.joins.improved;

import com.manning.hip.ch4.joins.improved.impl.*;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapred.*;

public class SampleMain {
  public static void main(String... args) throws Exception {

    JobConf job = new JobConf();
    job.setJarByClass(SampleMain.class);

    String input = args[0];
    Path output = new Path(args[1]);

    output.getFileSystem(job).delete(output, true);

    job.setMapperClass(SampleMap.class);
    job.setReducerClass(SampleReduce.class);

    job.setInputFormat(KeyValueTextInputFormat.class);

    job.setMapOutputKeyClass(CompositeKey.class);
    job.setMapOutputValueClass(TextTaggedOutputValue.class);
    job.setOutputKeyClass(Text.class);
    job.setOutputValueClass(Text.class);

    job.setPartitionerClass(CompositeKeyPartitioner.class);
    job.setOutputKeyComparatorClass(CompositeKeyComparator.class);
    job.setOutputValueGroupingComparator(
        CompositeKeyOnlyComparator.class);


    FileInputFormat.setInputPaths(job, input);
    FileOutputFormat.setOutputPath(job, output);

    JobClient.runJob(job);
  }
}
