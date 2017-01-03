package mia.clustering.ch12.lastfm;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileStatus;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.DefaultStringifier;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.SequenceFile;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.SequenceFileOutputFormat;
import org.apache.hadoop.util.GenericsUtil;
import org.apache.mahout.common.HadoopUtil;
import org.apache.mahout.math.VectorWritable;

public class VectorCreationJob {
  
  private VectorCreationJob() {}
  
  public static void createVectors(Path input,
                                   Path output,
                                   Path dictionaryPath) throws IOException,
                                                       InterruptedException,
                                                       ClassNotFoundException {
    Configuration conf = CreateNewConfiguration();
    
    Map<String,Integer> dictionary = new HashMap<String,Integer>();
    FileSystem fs = FileSystem.get(dictionaryPath.toUri(), conf);
    FileStatus[] outputFiles = fs.globStatus(new Path(dictionaryPath,
        "part-*"));
    int i = 0;
    for (FileStatus fileStatus : outputFiles) {
      Path path = fileStatus.getPath();
      SequenceFile.Reader reader = new SequenceFile.Reader(fs, path,
          conf);
      SequenceFile.Writer writer = new SequenceFile.Writer(fs, conf, new Path(path.toString()+"-dict"),
        Text.class, IntWritable.class);
      Text key = new Text();
      IntWritable value = new IntWritable();
      while (reader.next(key, value)) {
        dictionary.put(key.toString(), Integer.valueOf(i++));
        writer.append(key, new IntWritable(i-1));
      }
      writer.close();
    }
    DefaultStringifier<Map<String,Integer>> mapStringifier = new DefaultStringifier<Map<String,Integer>>(
        conf, GenericsUtil.getClass(dictionary));
    conf.set("dictionary", mapStringifier.toString(dictionary));
    
    Job job = new Job(conf, "Generating dataset based from input"
                            + input);
    job.setJarByClass(VectorCreationJob.class);
    
    job.setOutputKeyClass(Text.class);
    job.setOutputValueClass(VectorWritable.class);
    
    FileInputFormat.addInputPath(job, input);
    FileOutputFormat.setOutputPath(job, output);
    
    HadoopUtil.delete(conf, output);
    
    job.setInputFormatClass(TextInputFormat.class);
    job.setMapperClass(VectorMapper.class);
    job.setCombinerClass(VectorReducer.class);
    job.setReducerClass(VectorReducer.class);
    job.setOutputFormatClass(SequenceFileOutputFormat.class);
    
    job.waitForCompletion(true);
  }
  
  public static void generateDictionary(Path input, Path output) throws IOException,
                                                                InterruptedException,
                                                                ClassNotFoundException {
    Configuration conf = CreateNewConfiguration();
    
    Job job = new Job(conf, "Generating dataset based from input"
                            + input);
    job.setJarByClass(VectorCreationJob.class);
    
    job.setOutputKeyClass(Text.class);
    job.setOutputValueClass(IntWritable.class);
    
    FileInputFormat.addInputPath(job, input);
    FileOutputFormat.setOutputPath(job, output);
    
    HadoopUtil.delete(conf, output);
    
    job.setInputFormatClass(TextInputFormat.class);
    job.setMapperClass(DictionaryMapper.class);
    job.setCombinerClass(DictionaryReducer.class);
    job.setReducerClass(DictionaryReducer.class);
    job.setOutputFormatClass(SequenceFileOutputFormat.class);
    
    job.waitForCompletion(true);
  }
  
  private static Configuration CreateNewConfiguration() {
    Configuration conf = new Configuration();
    
    conf.set("mapred.compress.map.output", "true");
    conf.set("mapred.output.compression.type", "BLOCK");
    conf.set("io.serializations",
      "org.apache.hadoop.io.serializer.JavaSerialization,"
          + "org.apache.hadoop.io.serializer.WritableSerialization");
    return conf;
  }
}
