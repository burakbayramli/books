package com.manning.hip.ch6;


import com.manning.hip.common.PaddedTable;
import org.apache.commons.lang.reflect.MethodUtils;
import org.apache.hadoop.mapred.JobHistory;

import java.lang.reflect.InvocationTargetException;
import java.text.ParseException;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

public final class TaskThroughput {

  public static void main(String... args) throws Exception {
    try {
      dumpTaskTimes(args);
    } catch (Throwable t) {
      t.printStackTrace();
    }
  }

  public static void dumpTaskTimes(String... args)
      throws Exception {
    printTasks(JobHistoryHelper.getJobInfoFromCliArgs(args));
  }

  public static void printTasks(JobHistory.JobInfo job)
      throws ParseException, IllegalAccessException,
      InvocationTargetException, NoSuchMethodException {

    List<TaskMetrics> mapMetrics = JobHistoryHelper.getMapTaskMetrics(
        job);
    List<TaskMetrics> reduceMetrics = JobHistoryHelper.getReduceTaskMetrics(
        job);

    decorateHeader("MAP TASKS");

    dumpTasks(mapMetrics, "throughput",
        new TaskMetrics.OverallThroughputComparator(), "getOverallThroughputBytesPerSecond",
        false, false);

    decorateHeader("REDUCE TASKS");

    dumpTasks(reduceMetrics, "throughput",
        new TaskMetrics.OverallThroughputComparator(), "getOverallThroughputBytesPerSecond",
        false, true);
  }

  public static void decorateHeader(String header) {
    System.out.println("****************************************");
    System.out.println(String.format("** %24s%10s **", header, ""));
    System.out.println("****************************************");
  }

  public static void dumpTasks(List<TaskMetrics> metrics,
                               String heading,
                               Comparator<TaskMetrics> comparator,
                               String fieldName,
                               boolean isTime,
                               boolean isReduce)
      throws IllegalAccessException, InvocationTargetException,
      NoSuchMethodException {

    if (metrics.size() == 0) {
      return;
    }
    System.out.println("== Tasks ordered by " + heading + " ==");

    PaddedTable table = new PaddedTable();
    table
        .addColumnTitle("Type")
        .addColumnTitle("TaskId")
        .addColumnTitle("Status")
        .addColumnTitle("Host")
        .addColumnTitle("ExecutionTime");

    if(isReduce) {
      table
          .addColumnTitle("ShuffleTime")
          .addColumnTitle("SortTime");
    }

        table.addColumnTitle("InputBytes")
        .addColumnTitle("OutputBytes")
        .addColumnTitle("InputRecords")
        .addColumnTitle("OputputRecords")
        .addColumnTitle("Overall Throughput (B/s)");

    if(isReduce) {
      table
          .addColumnTitle("ShuffleThroughput")
          .addColumnTitle("SortThroughput")
          .addColumnTitle("ReduceThroughput");
    }

    Collections.sort(metrics, comparator);
    Collections.reverse(metrics);

    Long minVal = null;
    Long maxVal = null;
    long totalVals = 0;

    for (TaskMetrics m : metrics) {
      long v = extractLongFieldValue(m, fieldName);
      minVal = minVal == null ? v : Math.min(minVal, v);
      maxVal = maxVal == null ? v : Math.max(maxVal, v);
      totalVals += v;

      table.newRow();
      table.addColumnValue(m.getType())
          .addColumnValue(m.getTaskId())
          .addColumnValue(m.getStatus())
          .addColumnValue(m.getHost())
          .addColumnValue(JobHistoryHelper.formatTime(
              m.getOverallTimeMillis()));

      if(isReduce) {
        table.addColumnValue(JobHistoryHelper.formatTime(
            m.getShuffleTimeMillis()))
            .addColumnValue(JobHistoryHelper.formatTime(
                m.getSortTimeMillis()));
      }

          table.addColumnValue(m.getInputBytes())
          .addColumnValue(m.getOutputBytes())
          .addColumnValue(m.getInputRecords())
          .addColumnValue(m.getOutputRecords())
          .addColumnValue(m.getOverallThroughputBytesPerSecond());

      if(isReduce) {
        table.addColumnValue(m.getShuffleThroughputBytesPerSecond())
            .addColumnValue(m.getSortThroughputBytesPerSecond())
            .addColumnValue(m.getReduceThroughputBytesPerSecond());
      }
    }

    System.out.println();
    if (isTime) {
      System.out
          .println(String.format("Min/max/avg (HH:MM:SS) = %s/%s/%s",
              JobHistoryHelper.formatTime(minVal),
              JobHistoryHelper.formatTime(maxVal),
              JobHistoryHelper.formatTime(totalVals / metrics.size())));
    } else {
      System.out.println(String.format("Min/max/avg = %d/%d/%d",
          minVal, maxVal, totalVals / metrics.size()));
    }
    System.out.println();
    System.out.println(table);
  }

  public static long extractLongFieldValue(TaskMetrics m,
                                           String fieldName)
      throws IllegalAccessException, InvocationTargetException,
      NoSuchMethodException {
    return (Long) MethodUtils.invokeMethod(m, fieldName, null);
  }
}
