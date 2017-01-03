package com.manning.hip.ch6;


import com.manning.hip.common.PaddedTable;
import org.apache.commons.lang.reflect.MethodUtils;
import org.apache.hadoop.mapred.JobHistory;

import java.lang.reflect.InvocationTargetException;
import java.text.ParseException;
import java.util.List;

public final class MetricSummary {

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
    List<TaskMetrics> reduceMetrics =
        JobHistoryHelper.getReduceTaskMetrics(
            job);

    System.out.println("Job Statistics");
    System.out.println("");

    PaddedTable table = new PaddedTable();
    table
        .addColumnTitle("Item")
        .addColumnTitle("Min")
        .addColumnTitle("Max")
        .addColumnTitle("Median")
        .addColumnTitle("Mean")
        .addColumnTitle("StdDev");

    dumpTasks(table, mapMetrics, "execution time",
        "getOverallTimeMillis", true);
    dumpTasks(table, mapMetrics, "input records", "getInputRecords",
        false);
    dumpTasks(table, mapMetrics, "input bytes", "getInputBytes", false);
    dumpTasks(table, mapMetrics, "output records", "getOutputRecords",
        false);
    dumpTasks(table, mapMetrics, "output bytes", "getOutputBytes",
        false);

    DataSkewMetrics.decorateHeader("MAP TASKS");
    System.out.println("");
    System.out.println("Num Map Tasks:    " + mapMetrics.size());
    System.out.println("");
    System.out.println(table);
    table.clearRows();

    if(reduceMetrics.size() > 0) {

      dumpTasks(table, reduceMetrics, "execution time",
          "getOverallTimeMillis", true);
      dumpTasks(table, reduceMetrics, "shuffle time",
          "getShuffleTimeMillis", true);
      dumpTasks(table, reduceMetrics, "sort time", "getSortTimeMillis",
          true);
      dumpTasks(table, reduceMetrics, "input records", "getInputRecords",
          false);
      dumpTasks(table, reduceMetrics, "input bytes", "getInputBytes",
          false);
      dumpTasks(table, reduceMetrics, "output records",
          "getOutputRecords", false);
      dumpTasks(table, reduceMetrics, "output bytes", "getOutputBytes",
          false);

      DataSkewMetrics.decorateHeader("REDUCE TASKS");
      System.out.println("");
      System.out.println("Num Reduce Tasks: " + reduceMetrics.size());
      System.out.println("");
      System.out.println(table);
    }
  }

  public static void dumpTasks(PaddedTable table,
                               List<TaskMetrics> metrics,
                               String heading,
                               String fieldName,
                               boolean isTime)
      throws IllegalAccessException, InvocationTargetException,
      NoSuchMethodException {

    table.newRow();
    table.addColumnValue(heading);

    if (metrics.size() == 0) {
      table.addColumnValue("")
          .addColumnValue("")
          .addColumnValue("")
          .addColumnValue("");
    }

    Long minVal = null;
    Long maxVal = null;
    long totalVals = 0;

    long vals[] = new long[metrics.size()];
    int i = 0;

    for (TaskMetrics m : metrics) {
      long v = extractLongFieldValue(m, fieldName);
      minVal = minVal == null ? v : Math.min(minVal, v);
      maxVal = maxVal == null ? v : Math.max(maxVal, v);
      totalVals += v;
      vals[i++] = v;
    }

    double mean = totalVals / metrics.size();
    double stddev = stddev(vals);
    double median = median(vals);

    if (isTime) {
      table.addColumnValue(JobHistoryHelper.formatTime(minVal))
          .addColumnValue(JobHistoryHelper.formatTime(maxVal))
          .addColumnValue(JobHistoryHelper.formatTime((long) median))
          .addColumnValue(JobHistoryHelper.formatTime((long) mean))
          .addColumnValue(JobHistoryHelper.formatTime((long) stddev));
    } else {
      table.addColumnValue(minVal)
          .addColumnValue(maxVal)
          .addColumnValue((long) median)
          .addColumnValue((long) mean)
          .addColumnValue((long) stddev);
    }
  }

  public static double stddev(long[] m) {
    long total = 0;
    for(long l: m) {
      total += l;
    }
    double mean = total / m.length;

    long diffMean = 0;
    for(long l: m) {
      diffMean += Math.abs(mean - l);
    }

    return Math.sqrt((double) diffMean / (double) m.length);
  }

  public static double median(long[] m) {
    int middle = m.length / 2;
    if (m.length % 2 == 1) {
      return m[middle];
    } else {
      return (m[middle - 1] + m[middle]) / 2.0;
    }
  }

  public static long extractLongFieldValue(TaskMetrics m,
                                           String fieldName)
      throws IllegalAccessException, InvocationTargetException,
      NoSuchMethodException {
    return (Long) MethodUtils.invokeMethod(m, fieldName, null);
  }
}
