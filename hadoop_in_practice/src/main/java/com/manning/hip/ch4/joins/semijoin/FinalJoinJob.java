package com.manning.hip.ch4.joins.semijoin;

import com.manning.hip.ch4.joins.replicated.*;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.filecache.DistributedCache;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.lib.input.*;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

public class FinalJoinJob {
  public static void main(String... args) throws Exception {
    runJob(new Path(args[0]), new Path(args[1]), new Path(args[2]));
  }

  public static void runJob(Path userLogsPath,
                            Path usersPath,
                            Path outputPath)
      throws Exception {

    Configuration conf = new Configuration();

    FileSystem fs = usersPath.getFileSystem(conf);

    FileStatus usersStatus = fs.getFileStatus(usersPath);

    if(usersStatus.isDir()) {
      for(FileStatus f: fs.listStatus(usersPath)) {
        if(f.getPath().getName().startsWith("part")) {
          DistributedCache.addCacheFile(f.getPath().toUri(), conf);
        }
      }
    } else {
      DistributedCache.addCacheFile(usersPath.toUri(), conf);
    }

    Job job = new Job(conf);

    job.setJarByClass(FinalJoinJob.class);
    job.setMapperClass(GenericReplicatedJoin.class);

    job.setNumReduceTasks(0);

    job.setInputFormatClass(KeyValueTextInputFormat.class);

    outputPath.getFileSystem(conf).delete(outputPath, true);

    FileInputFormat.setInputPaths(job, userLogsPath);
    FileOutputFormat.setOutputPath(job, outputPath);

    if(!job.waitForCompletion(true)) {
      throw new Exception("Job failed");
    }
  }
}
