package com.manning.hip.ch3.seqfile;

import com.manning.hip.ch3.StockPriceWritable;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.SequenceFile;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.io.compress.DefaultCodec;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.SequenceFileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.SequenceFileOutputFormat;

public class SequenceFileStockMapReduce {
  public static void main(String... args) throws Exception {
    runJob(args[0], args[1]);
  }

  public static void runJob(String input,
                            String output)
      throws Exception {
    Configuration conf = new Configuration();
    Job job = new Job(conf);
    job.setJarByClass(SequenceFileStockMapReduce.class);
    job.setOutputKeyClass(Text.class);
    job.setOutputValueClass(StockPriceWritable.class);
    job.setInputFormatClass(
        SequenceFileInputFormat.class); //<co id="ch03_comment_seqfile_mr1"/>
    job.setOutputFormatClass(SequenceFileOutputFormat.class);  //<co id="ch03_comment_seqfile_mr2"/>
    SequenceFileOutputFormat.setCompressOutput(job, true);  //<co id="ch03_comment_seqfile_mr3"/>
    SequenceFileOutputFormat.setOutputCompressionType(job,  //<co id="ch03_comment_seqfile_mr4"/>
        SequenceFile.CompressionType.BLOCK);
    SequenceFileOutputFormat.setOutputCompressorClass(job,  //<co id="ch03_comment_seqfile_mr5"/>
        DefaultCodec.class);

    FileInputFormat.setInputPaths(job, new Path(input));
    Path outPath = new Path(output);
    FileOutputFormat.setOutputPath(job, outPath);
    outPath.getFileSystem(conf).delete(outPath, true);

    job.waitForCompletion(true);
  }
}
