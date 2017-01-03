package com.manning.hip.ch6;


import org.apache.commons.lang.reflect.MethodUtils;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.FileUtil;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.fs.PathFilter;
import org.apache.hadoop.mapred.Counters;
import org.apache.hadoop.mapred.DefaultJobHistoryParser;
import org.apache.hadoop.mapred.JobHistory;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.apache.hadoop.mapred.Task.Counter.*;

public final class JobHistoryHelper {

  public static JobHistory.JobInfo getJobInfoFromCliArgs(String ... args)
      throws IOException {
    return getJobInfoFromCliArgs(new Configuration(), args);
  }

  public static JobHistory.JobInfo getJobInfoFromCliArgs(Configuration conf, String ... args)
      throws IOException {
    String usage = "Expected 2 arguments, either --hdfsdir <dir> or --localfile <path>";
    if(args.length != 2) {
      throw new IOException(usage);
    }
    if("--hdfsdir".equals(args[0])) {
      return getJobInfoFromHdfsOutputDir(args[1], conf);
    } else if("--localfile".equals(args[0])) {
      return getJobInfoFromLocalFile(args[1], conf);
    }
    throw new IOException("Unexpected option '" + args[0] + "' \n" + usage);
  }


  public static PathFilter jobLogFileFilter = new PathFilter() {
    public boolean accept(Path path) {
      return !(path.getName().endsWith(".xml"));
    }
  };

  public static JobHistory.JobInfo getJobInfoFromHdfsOutputDir(String outputDir, Configuration conf)
      throws IOException {
    Path output = new Path(outputDir);
    Path historyLogDir = new Path(output, "_logs/history");
      FileSystem fs = output.getFileSystem(conf);
      if (!fs.exists(output)) {
        throw new IOException("History directory " + historyLogDir.toString()
            + " does not exist");
      }
      Path[] jobFiles = FileUtil.stat2Paths(fs.listStatus(historyLogDir,
          jobLogFileFilter));
      if (jobFiles.length == 0) {
        throw new IOException("Not a valid history directory "
            + historyLogDir.toString());
      }
      String[] jobDetails =
          JobHistory.JobInfo.decodeJobHistoryFileName(jobFiles[0].getName()).
              split("_");
      String jobId = jobDetails[2] + "_" + jobDetails[3] + "_" + jobDetails[4];
      JobHistory.JobInfo job = new JobHistory.JobInfo(jobId);
      DefaultJobHistoryParser.parseJobTasks(jobFiles[0].toString(), job, fs);
    return job;
  }

  public static JobHistory.JobInfo getJobInfoFromLocalFile(String outputFile, Configuration conf)
      throws IOException {
    FileSystem fs = FileSystem.getLocal(conf);

    Path outputFilePath = new Path(outputFile);

    String[] jobDetails =
        JobHistory.JobInfo.decodeJobHistoryFileName(outputFilePath.getName()).
            split("_");
    String jobId = jobDetails[2] + "_" + jobDetails[3] + "_" + jobDetails[4];
    JobHistory.JobInfo job = new JobHistory.JobInfo(jobId);
    DefaultJobHistoryParser.parseJobTasks(outputFile, job, fs);
    return job;
  }

  public static List<TaskMetrics> getMapTaskMetrics(
      JobHistory.JobInfo job)
      throws ParseException {
    List<TaskMetrics> metrics = new ArrayList<TaskMetrics>();
    addTask(metrics, job, JobHistory.Values.MAP.name());
    return metrics;
  }

  public static List<TaskMetrics> getReduceTaskMetrics(
      JobHistory.JobInfo job)
      throws ParseException {
    List<TaskMetrics> metrics = new ArrayList<TaskMetrics>();
    addTask(metrics, job, JobHistory.Values.REDUCE.name());
    return metrics;
  }


  public static long extractLongFieldValue(TaskMetrics m,
                                           String fieldName)
      throws IllegalAccessException, InvocationTargetException,
      NoSuchMethodException {
    return (Long) MethodUtils.invokeMethod(m, fieldName, null);
  }

  public static void addTask(List<TaskMetrics> metrics,
                             JobHistory.JobInfo job,
                             String taskType)
      throws ParseException {
    Map<String, JobHistory.Task> tasks = job.getAllTasks();
    for (JobHistory.Task task : tasks.values()) {
      for (JobHistory.TaskAttempt attempt : task.getTaskAttempts()
          .values()) {
        if (taskType.equals(task.get(JobHistory.Keys.TASK_TYPE))) {

          TaskMetrics metric = new TaskMetrics();
          metrics.add(metric);
          metric.setType(taskType)
              .setTaskId(attempt.get(JobHistory.Keys.TASK_ATTEMPT_ID))
              .setHost(attempt.get(JobHistory.Keys.HOSTNAME))
              .setStatus(attempt.get(JobHistory.Keys.TASK_STATUS));

          long taskOverallTime =
              attempt.getLong(JobHistory.Keys.FINISH_TIME) -
                  attempt.getLong(JobHistory.Keys.START_TIME);

          metric.setOverallTimeMillis(taskOverallTime);

          metric.setInputBytes(
              extractNumericCounter(
                  attempt.get(JobHistory.Keys.COUNTERS),
                  MAP_INPUT_BYTES.name(),
                  REDUCE_SHUFFLE_BYTES.name()));

          metric.setOutputBytes(
              extractNumericCounter(
                  attempt.get(JobHistory.Keys.COUNTERS),
                  MAP_OUTPUT_BYTES.name(),
                  "HDFS_BYTES_WRITTEN"));

          metric.setInputRecords(
              extractNumericCounter(
                  attempt.get(JobHistory.Keys.COUNTERS),
                  MAP_INPUT_RECORDS.name(),
                  REDUCE_INPUT_RECORDS.name()));

          metric.setOutputRecords(
              extractNumericCounter(
                  attempt.get(JobHistory.Keys.COUNTERS),
                  MAP_OUTPUT_RECORDS.name(),
                  REDUCE_OUTPUT_RECORDS.name()));

          if (JobHistory.Values.REDUCE.name()
              .equals(task.get(JobHistory.Keys.TASK_TYPE))) {
            long shuffleTime =
                attempt.getLong(JobHistory.Keys.SHUFFLE_FINISHED) -
                    attempt.getLong(JobHistory.Keys.START_TIME);
            long sortTime =
                attempt.getLong(JobHistory.Keys.SORT_FINISHED) -
                    attempt
                        .getLong(JobHistory.Keys.SHUFFLE_FINISHED);

            metric.setShuffleTimeMillis(shuffleTime);
            metric.setSortTimeMillis(sortTime);
          }

        }
      }
    }
  }

  public static long extractNumericCounter(String counterFromHist,
                                           String... counterNames)
      throws ParseException {
    long result = -1;
    String s = extractCounter(counterFromHist, counterNames);
    if (s != null) {
      result = Long.valueOf(s);
    }
    return result;
  }

  public static String extractCounter(String counterFromHist,
                                      String... counterNames)
      throws ParseException {
    Counters counters =
        Counters.fromEscapedCompactString(counterFromHist);
    for (Counters.Group group : counters) {
      for (Counters.Counter counter : group) {
        for (String counterName : counterNames) {
          if (counterName.equals(counter.getName())) {
            return String.valueOf(counter.getCounter());
          }
        }
      }
    }
    return null;
  }

  public static String formatTime(long timeDiffMillis) {
    StringBuilder buf = new StringBuilder();
    long hours = timeDiffMillis / (60 * 60 * 1000);
    long rem = (timeDiffMillis % (60 * 60 * 1000));
    long minutes = rem / (60 * 1000);
    rem = rem % (60 * 1000);
    long seconds = rem / 1000;

    if (hours != 0) {
      buf.append(hours);
      buf.append("h");
    }
    if (hours != 0 || minutes != 0) {
      if (buf.length() > 0) {
        buf.append(" ");
      }
      buf.append(minutes);
      buf.append("m");
    }
    if (buf.length() > 0) {
      buf.append(" ");
    }
    buf.append(seconds)
        .append("s");
    return buf.toString();
  }


}
