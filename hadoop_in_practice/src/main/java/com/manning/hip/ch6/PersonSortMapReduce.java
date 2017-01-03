package com.manning.hip.ch6;


import com.manning.hip.ch4.sort.secondary.*;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.mapreduce.lib.input.*;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

import java.io.IOException;

public final class PersonSortMapReduce {
  public static void main(String... args) throws Exception {
    runSortJob(args[0], args[1]);
  }

  public static void runSortJob(String input, String output)
      throws Exception {
    Configuration conf = new Configuration();

    Job job = new Job(conf);
    job.setJarByClass(PersonSortMapReduce.class);

    job.setMapperClass(Map.class);
    job.setReducerClass(Reduce.class);

    job.setInputFormatClass(KeyValueTextInputFormat.class);

    job.setMapOutputKeyClass(Person.class);
    job.setMapOutputValueClass(Text.class);

    job.setOutputKeyClass(Text.class);
    job.setOutputValueClass(Text.class);

    job.setSortComparatorClass(PersonBinaryComparator.class);

    Path outputPath = new Path(output);

    FileInputFormat.setInputPaths(job, input);
    FileOutputFormat.setOutputPath(job, outputPath);

    outputPath.getFileSystem(conf).delete(outputPath, true);

    job.waitForCompletion(true);
  }

  public static class Map
      extends Mapper<Text, Text, Person, Text> {

    private Person outputKey = new Person();

    @Override
    protected void map(Text lastName, Text firstName, Context context)
        throws IOException, InterruptedException {
      outputKey.set(lastName.toString(), firstName.toString());
      context.write(outputKey, firstName);
    }
  }

  public static class Reduce
      extends Reducer<Person, Text, Text, Text> {

    Text lastName = new Text();

    @Override
    public void reduce(Person key, Iterable<Text> values,
                       Context context)
        throws IOException, InterruptedException {
      lastName.set(key.getLastName());
      for (Text firstName : values) {
        context.write(lastName, firstName);
      }
    }
  }
}
