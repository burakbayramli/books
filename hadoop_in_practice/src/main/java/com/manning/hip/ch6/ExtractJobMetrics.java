package com.manning.hip.ch6;


import com.manning.hip.common.PaddedTable;
import org.apache.hadoop.mapred.*;

import java.text.ParseException;
import java.util.Map;

import static org.apache.hadoop.mapred.Task.Counter.*;

public final class ExtractJobMetrics {

  public static void main(String... args) throws Exception {
    try {
      dumpTaskTimes(args);
    } catch (Throwable t) {
      t.printStackTrace();
    }
  }

  public static void dumpTaskTimes(String... args)
      throws Exception {

    JobHistory.JobInfo job = JobHistoryHelper.getJobInfoFromCliArgs(args);

    printAllTaskAttempts(job);
  }

  public static void printAllTaskAttempts(JobHistory.JobInfo job)
      throws ParseException {
    PaddedTable table = new PaddedTable();
    table
        .addColumnTitle("Type")
        .addColumnTitle("TaskId")
        .addColumnTitle("Status")
        .addColumnTitle("Host")
        .addColumnTitle("OverallTime(HH:MM:SS)")
        .addColumnTitle("ShuffleTime(HH:MM:SS)")
        .addColumnTitle("SortTime(HH:MM:SS)")
        .addColumnTitle("MapInputBytes")
        .addColumnTitle("MapOutputBytes")
        .addColumnTitle("InputRecords")
        .addColumnTitle("OputputRecords");

    printAllTaskAttempts(table, job, JobHistory.Values.MAP.name());
    printAllTaskAttempts(table, job, JobHistory.Values.REDUCE.name());
    printAllTaskAttempts(table, job, JobHistory.Values.SETUP.name());
    printAllTaskAttempts(table, job, JobHistory.Values.CLEANUP.name());

    System.out.println(table);
  }

  public static void printAllTaskAttempts(PaddedTable table,
                                          JobHistory.JobInfo job,
                                          String taskType)
      throws ParseException {
    Map<String, JobHistory.Task> tasks = job.getAllTasks();
    for (JobHistory.Task task : tasks.values()) {
      for (JobHistory.TaskAttempt attempt : task.getTaskAttempts()
          .values()) {
        if (taskType.equals(task.get(JobHistory.Keys.TASK_TYPE))) {

          long taskOverallTime =
                  attempt.getLong(JobHistory.Keys.FINISH_TIME) -
                      attempt.getLong(JobHistory.Keys.START_TIME);
          long shuffleTime =
                  attempt.getLong(JobHistory.Keys.SHUFFLE_FINISHED) -
                      attempt.getLong(JobHistory.Keys.START_TIME);
          long taskSortTime =
                  attempt.getLong(JobHistory.Keys.SORT_FINISHED) -
                      attempt
                          .getLong(JobHistory.Keys.SHUFFLE_FINISHED);

          table.newRow()
              .addColumnValue(taskType)
              .addColumnValue(
                  attempt.get(JobHistory.Keys.TASK_ATTEMPT_ID))
              .addColumnValue(
                  attempt.get(JobHistory.Keys.TASK_STATUS))
              .addColumnValue(attempt.get(JobHistory.Keys.HOSTNAME))
              .addColumnValue(formatTime(taskOverallTime));

          if (JobHistory.Values.REDUCE.name()
              .equals(task.get(JobHistory.Keys.TASK_TYPE))) {
            table.addColumnValue(formatTime(shuffleTime))
                .addColumnValue(formatTime(taskSortTime));
          } else {
            table.addColumnValue("").addColumnValue("");
          }
          table.addColumnValue(
              extractCounter(attempt.get(JobHistory.Keys.COUNTERS),
                  MAP_INPUT_BYTES.name()));
          table.addColumnValue(
              extractCounter(attempt.get(JobHistory.Keys.COUNTERS),
                  MAP_OUTPUT_BYTES.name()));
          table.addColumnValue(
              extractCounter(attempt.get(JobHistory.Keys.COUNTERS),
                  MAP_INPUT_RECORDS.name(),
                  REDUCE_INPUT_RECORDS.name()));
          table.addColumnValue(
              extractCounter(attempt.get(JobHistory.Keys.COUNTERS),
                  MAP_OUTPUT_RECORDS.name(),
                  REDUCE_OUTPUT_RECORDS.name()));
        }
      }
    }
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
    return "";
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
      buf.append(":");
    }
    if (hours != 0 || minutes != 0) {
      buf.append(minutes);
      buf.append(":");
    }
    buf.append(seconds);
    return buf.toString();
  }


}
