/**
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.manning.hip.ch4.joins.improved.impl;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapred.*;

import java.io.IOException;

/**
 * This class implements the main function for creating a map/reduce
 * job to join data of different sources. To create sucn a job, the 
 * user must implement a mapper class that extends DataJoinMapperBase class,
 * and a reducer class that extends DataJoinReducerBase. 
 * 
 */
public class OptimizedDataJoinJob {

  public static Class getClassByName(String className) {
    Class retv = null;
    try {
      ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
      retv = Class.forName(className, true, classLoader);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
    return retv;
  }

  public static JobConf createDataJoinJob(String args[]) throws IOException {

    String inputDir = args[0];
    String outputDir = args[1];
    Class inputFormat = SequenceFileInputFormat.class;
    if (args[2].compareToIgnoreCase("text") != 0) {
      System.out.println("Using SequenceFileInputFormat: " + args[2]);
    } else {
      System.out.println("Using TextInputFormat: " + args[2]);
      inputFormat = TextInputFormat.class;
    }
    int numOfReducers = Integer.parseInt(args[3]);
    Class mapper = getClassByName(args[4]);
    Class reducer = getClassByName(args[5]);
    Class mapoutputValueClass = getClassByName(args[6]);
    Class outputFormat = TextOutputFormat.class;
    Class outputValueClass = Text.class;
    if (args[7].compareToIgnoreCase("text") != 0) {
      System.out.println("Using SequenceFileOutputFormat: " + args[7]);
      outputFormat = SequenceFileOutputFormat.class;
      outputValueClass = getClassByName(args[7]);
    } else {
      System.out.println("Using TextOutputFormat: " + args[7]);
    }
    long maxNumOfValuesPerGroup = 100;
    String jobName = "";
    if (args.length > 8) {
      maxNumOfValuesPerGroup = Long.parseLong(args[8]);
    }
    if (args.length > 9) {
      jobName = args[9];
    }
    Configuration defaults = new Configuration();
    JobConf job = new JobConf(defaults, OptimizedDataJoinJob.class);
    job.setJobName("DataJoinJob: " + jobName);

    FileSystem fs = FileSystem.get(defaults);
    fs.delete(new Path(outputDir));
    FileInputFormat.setInputPaths(job, inputDir);

    job.setInputFormat(inputFormat);

    job.setMapperClass(mapper);
    FileOutputFormat.setOutputPath(job, new Path(outputDir));
    job.setOutputFormat(outputFormat);
    SequenceFileOutputFormat.setOutputCompressionType(job,
            SequenceFile.CompressionType.BLOCK);
    job.setMapOutputKeyClass(CompositeKey.class);
    job.setMapOutputValueClass(mapoutputValueClass);
    job.setOutputKeyClass(Text.class);
    job.setOutputValueClass(outputValueClass);
    job.setReducerClass(reducer);

    job.setPartitionerClass(CompositeKeyPartitioner.class);
    job.setOutputKeyComparatorClass(CompositeKeyComparator.class);
    job.setOutputValueGroupingComparator(
        CompositeKeyOnlyComparator.class);

    job.setNumMapTasks(1);
    job.setNumReduceTasks(numOfReducers);
    job.setLong("datajoin.maxNumOfValuesPerGroup", maxNumOfValuesPerGroup);
    return job;
  }

  /**
   * Submit/run a map/reduce job.
   * 
   * @param job
   * @return true for success
   * @throws java.io.IOException
   */
  public static boolean runJob(JobConf job) throws IOException {
    JobClient jc = new JobClient(job);
    boolean sucess = true;
    RunningJob running = null;
    try {
      running = jc.submitJob(job);
      JobID jobId = running.getID();
      System.out.println("Job " + jobId + " is submitted");
      while (!running.isComplete()) {
        System.out.println("Job " + jobId + " is still running.");
        try {
          Thread.sleep(60000);
        } catch (InterruptedException e) {
        }
        running = jc.getJob(jobId);
      }
      sucess = running.isSuccessful();
    } finally {
      if (!sucess && (running != null)) {
        running.killJob();
      }
      jc.close();
    }
    return sucess;
  }

  /**
   * @param args
   */
  public static void main(String[] args) {
    boolean success;
    if (args.length < 8 || args.length > 10) {
      System.out.println("usage: DataJoinJob " + "inputdirs outputdir map_input_file_format "
                         + "numofParts " + "mapper_class " + "reducer_class "
                         + "map_output_value_class "
                         + "output_value_class [maxNumOfValuesPerGroup [descriptionOfJob]]]");
      System.exit(-1);
    }

    try {
      JobConf job = OptimizedDataJoinJob.createDataJoinJob(args);
      success = OptimizedDataJoinJob.runJob(job);
      if (!success) {
        System.out.println("Job failed");
      }
    } catch (IOException ioe) {
      ioe.printStackTrace();
    }
  }
}
