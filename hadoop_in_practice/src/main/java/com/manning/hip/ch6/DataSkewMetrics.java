package com.manning.hip.ch6;


import com.manning.hip.common.PaddedTable;
import org.apache.commons.lang.reflect.MethodUtils;
import org.apache.hadoop.mapred.JobHistory;

import java.lang.reflect.InvocationTargetException;
import java.text.ParseException;
import java.util.*;

public final class DataSkewMetrics {

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

    dumpTasks(mapMetrics, "execution time",
        new TaskMetrics.ExecTimeComparator(), "getOverallTimeMillis",
        true);
    dumpTasks(mapMetrics, "input records",
        new TaskMetrics.InputRecordsComparator(), "getInputRecords",
        false);
    dumpTasks(mapMetrics, "input bytes",
        new TaskMetrics.InputBytesComparator(), "getInputBytes",
        false);

    decorateHeader("REDUCE TASKS");

    dumpTasks(reduceMetrics, "execution time",
        new TaskMetrics.ExecTimeComparator(), "getOverallTimeMillis", true);
    dumpTasks(reduceMetrics, "input records",
        new TaskMetrics.InputRecordsComparator(), "getInputRecords",
        false);
    dumpTasks(reduceMetrics, "input bytes",
        new TaskMetrics.InputBytesComparator(), "getInputBytes",
        false);
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
                               boolean isTime)
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
        .addColumnTitle("ExecutionTime")
        .addColumnTitle("InputBytes")
        .addColumnTitle("OutputBytes")
        .addColumnTitle("InputRecords")
        .addColumnTitle("OputputRecords");

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
              m.getOverallTimeMillis()))
          .addColumnValue(m.getInputBytes())
          .addColumnValue(m.getOutputBytes())
          .addColumnValue(m.getInputRecords())
          .addColumnValue(m.getOutputRecords());
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
