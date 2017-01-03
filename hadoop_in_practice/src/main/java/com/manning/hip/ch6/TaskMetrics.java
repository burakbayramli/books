package com.manning.hip.ch6;

import java.util.Comparator;

public class TaskMetrics {
  String type;
  String taskId;
  String host;
  String status;
  long overallTimeMillis;
  long shuffleTimeMillis;
  long sortTimeMillis;
  long inputBytes;
  long outputBytes;
  long inputRecords;
  long outputRecords;

  public String getType() {
    return type;
  }

  public TaskMetrics setType(String type) {
    this.type = type;
    return this;
  }

  public String getTaskId() {
    return taskId;
  }

  public TaskMetrics setTaskId(String taskId) {
    this.taskId = taskId;
    return this;
  }

  public String getHost() {
    return host;
  }

  public TaskMetrics setHost(String host) {
    this.host = host;
    return this;
  }

  public String getStatus() {
    return status;
  }

  public TaskMetrics setStatus(String status) {
    this.status = status;
    return this;
  }

  public long getOverallTimeMillis() {
    return overallTimeMillis;
  }

  public TaskMetrics setOverallTimeMillis(long overallTimeMillis) {
    this.overallTimeMillis = overallTimeMillis;
    return this;
  }

  public long getShuffleTimeMillis() {
    return shuffleTimeMillis;
  }

  public TaskMetrics setShuffleTimeMillis(long shuffleTimeMillis) {
    this.shuffleTimeMillis = shuffleTimeMillis;
    return this;
  }

  public long getSortTimeMillis() {
    return sortTimeMillis;
  }

  public TaskMetrics setSortTimeMillis(long sortTimeMillis) {
    this.sortTimeMillis = sortTimeMillis;
    return this;
  }

  public long getInputBytes() {
    return inputBytes;
  }

  public TaskMetrics setInputBytes(long inputBytes) {
    this.inputBytes = inputBytes;
    return this;
  }

  public long getOutputBytes() {
    return outputBytes;
  }

  public TaskMetrics setOutputBytes(long outputBytes) {
    this.outputBytes = outputBytes;
    return this;
  }

  public long getInputRecords() {
    return inputRecords;
  }

  public TaskMetrics setInputRecords(long inputRecords) {
    this.inputRecords = inputRecords;
    return this;
  }

  public long getOutputRecords() {
    return outputRecords;
  }

  public TaskMetrics setOutputRecords(long outputRecords) {
    this.outputRecords = outputRecords;
    return this;
  }

  public long getOverallThroughputBytesPerSecond() {
    long overallSecs = overallTimeMillis / 1000;
    if(overallSecs == 0) {
      return 0;
    }
    return inputBytes / overallSecs;
  }

  public long getShuffleThroughputBytesPerSecond() {
    long overallSecs = shuffleTimeMillis / 1000;
    if(overallSecs == 0) {
      return 0;
    }
    return inputBytes / overallSecs;
  }

  public long getSortThroughputBytesPerSecond() {
    long overallSecs = sortTimeMillis / 1000;
    if(overallSecs == 0) {
      return 0;
    }
    return inputBytes / overallSecs;
  }

  public long getReduceThroughputBytesPerSecond() {
    long overallSecs = (overallTimeMillis - (shuffleTimeMillis + sortTimeMillis)) / 1000;
    if(overallSecs == 0) {
      return 0;
    }
    return inputBytes / overallSecs;
  }

  public static int longCompare(long lhs, long rhs) {
    return lhs < rhs ? -1 : ( lhs == rhs ? 0 : 1);
  }

  public static class ExecTimeComparator implements Comparator<TaskMetrics> {
    @Override
    public int compare(TaskMetrics o1, TaskMetrics o2) {
      return longCompare(o1.getOverallTimeMillis(), o2.getOverallTimeMillis());
    }
  }

  public static class InputRecordsComparator implements Comparator<TaskMetrics> {
    @Override
    public int compare(TaskMetrics o1, TaskMetrics o2) {
      return longCompare(o1.getInputRecords(), o2.getInputRecords());
    }
  }

  public static class InputBytesComparator implements Comparator<TaskMetrics> {
    @Override
    public int compare(TaskMetrics o1, TaskMetrics o2) {
      return longCompare(o1.getInputBytes(), o2.getInputBytes());
    }
  }

  public static class OverallThroughputComparator implements Comparator<TaskMetrics> {
    @Override
    public int compare(TaskMetrics o1, TaskMetrics o2) {
      return longCompare(o1.getOverallThroughputBytesPerSecond(), o2.getOverallThroughputBytesPerSecond());
    }
  }
}
