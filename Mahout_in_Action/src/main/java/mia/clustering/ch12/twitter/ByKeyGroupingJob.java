package mia.clustering.ch12.twitter;

import java.io.IOException;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.SequenceFileOutputFormat;
import org.apache.mahout.common.HadoopUtil;
import org.apache.mahout.common.Parameters;

public class ByKeyGroupingJob {
  
  private ByKeyGroupingJob() {}
  
  public static void startJob(Parameters params) throws IOException,
                                                InterruptedException,
                                                ClassNotFoundException {
    Configuration conf = new Configuration();
    
    conf.set("job.parameters", params.toString());
    conf.set("io.serializations",
      "org.apache.hadoop.io.serializer.JavaSerialization,"
          + "org.apache.hadoop.io.serializer.WritableSerialization");
    
    String input = params.get("input");
    Job job = new Job(conf, "Generating dataset based from input"
                            + input);
    job.setJarByClass(ByKeyGroupingJob.class);
    
    job.setOutputKeyClass(Text.class);
    job.setOutputValueClass(Text.class);
    
    FileInputFormat.addInputPath(job, new Path(input));
    Path outPath = new Path(params.get("output"));
    FileOutputFormat.setOutputPath(job, outPath);
    
    HadoopUtil.delete(conf, outPath);
    
    job.setInputFormatClass(TextInputFormat.class);
    job.setMapperClass(ByKeyMapper.class);
    job.setCombinerClass(ByKeyReducer.class);
    job.setReducerClass(ByKeyReducer.class);
    job.setOutputFormatClass(SequenceFileOutputFormat.class);
    
    job.waitForCompletion(true);
  }
}
