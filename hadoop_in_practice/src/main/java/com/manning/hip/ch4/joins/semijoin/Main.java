package com.manning.hip.ch4.joins.semijoin;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.*;

public class Main {
  public static void main(String... args) throws Exception {
    runJob(new Path(args[0]), new Path(args[1]), new Path(args[2]));
  }

  public static void runJob(Path smallFilePath,
                            Path largeFilePath,
                            Path workPath)
      throws Exception {

    Configuration conf = new Configuration();

    FileSystem fs = workPath.getFileSystem(conf);
    fs.delete(workPath, true);

    fs.mkdirs(workPath);


    /////////////////////////////////////////////////////
    // JOB 1 - Produce unique keys from the large file
    /////////////////////////////////////////////////////
    Path uniqueKeyOutputPath = new Path(workPath, "unique");
    UniqueHashedKeyJob.runJob(largeFilePath, uniqueKeyOutputPath);

    /////////////////////////////////////////////////////
    // JOB 2 - Use the unique keys from the large file to
    //         retain the contents of the small file that
    //         match
    /////////////////////////////////////////////////////
    Path filteredSmallOutputPath = new Path(workPath, "filtered");
    ReplicatedFilterJob.runJob(smallFilePath, uniqueKeyOutputPath,
        filteredSmallOutputPath);

    /////////////////////////////////////////////////////
    // JOB 3 - The final join
    /////////////////////////////////////////////////////
    Path resultOutputPath = new Path(workPath, "result");
    FinalJoinJob.runJob(largeFilePath, filteredSmallOutputPath, resultOutputPath);
  }

}
