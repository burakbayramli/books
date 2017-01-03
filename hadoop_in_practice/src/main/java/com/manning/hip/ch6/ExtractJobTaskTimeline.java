package com.manning.hip.ch6;


import org.apache.hadoop.mapred.*;

import java.util.*;
import java.util.concurrent.TimeUnit;

public final class ExtractJobTaskTimeline {

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

    long startTime = job.getLong(JobHistory.Keys.LAUNCH_TIME);
    long endTime = job.getLong(JobHistory.Keys.FINISH_TIME);

    List<TimeRange> mapRanges = new ArrayList<TimeRange>();
    List<TimeRange> reduceRanges = new ArrayList<TimeRange>();
    List<TimeRange> shuffleRanges = new ArrayList<TimeRange>();
    List<TimeRange> sortRanges = new ArrayList<TimeRange>();


    Map<String, JobHistory.Task> tasks = job.getAllTasks();
    for (JobHistory.Task task : tasks.values()) {
      for (JobHistory.TaskAttempt attempt : task.getTaskAttempts()
          .values()) {

        String taskId = attempt.get(JobHistory.Keys.TASK_ATTEMPT_ID);
        String taskType = task.get(JobHistory.Keys.TASK_TYPE);
        String taskStatus = task.get(JobHistory.Keys.TASK_STATUS);

        System.out.println(taskId + " " + taskType + " " + taskStatus);


        long taskStartTime =
            attempt.getLong(JobHistory.Keys.START_TIME);
        long taskEndTime =
            attempt.getLong(JobHistory.Keys.FINISH_TIME);

        TimeRange range =
            new TimeRange(TimeUnit.MILLISECONDS, taskStartTime,
                taskEndTime);

          if (JobHistory.Values.MAP.name().equals(taskType)) {
            mapRanges.add(range);
          } else if (JobHistory.Values.REDUCE.name().equals(taskType)) {

            long shuffleEndTime =
                attempt.getLong(JobHistory.Keys.SHUFFLE_FINISHED);
            long sortEndTime =
                attempt.getLong(JobHistory.Keys.SORT_FINISHED);

            shuffleRanges.add(
                new TimeRange(TimeUnit.MILLISECONDS, taskStartTime,
                    shuffleEndTime));
            sortRanges.add(
                new TimeRange(TimeUnit.MILLISECONDS, shuffleEndTime,
                    sortEndTime));
            reduceRanges.add(
                new TimeRange(TimeUnit.MILLISECONDS, sortEndTime,
                    taskEndTime));
          }
      }
    }

    // output the data, tab-separated in the following order:
    // time-offset  #-map-tasks  #-reduce-tasks  #-shuffle-tasks  #-sort-tasks  #-waste-tasks
    // steps of 1 second
    StringBuilder sb = new StringBuilder();
    sb.append("time")
        .append("\tmap")
        .append("\treduce")
        .append("\tshuffle")
        .append("\tsort")
    ;
    System.err.println(sb);

    int timeOffset = 0;
    for (long i = startTime; i <= endTime; i += 1000) {
      sb = new StringBuilder();
      sb.append(timeOffset)
          .append("\t").append(countRangesForTime(mapRanges, i))
          .append("\t").append(countRangesForTime(reduceRanges, i))
          .append("\t").append(countRangesForTime(shuffleRanges, i))
          .append("\t").append(countRangesForTime(sortRanges, i))
      ;

      System.err.println(sb);
      timeOffset++;

    }

  }

  public static int countRangesForTime(List<TimeRange> ranges,
                                       long time) {
    int count = 0;
    for (TimeRange range : ranges) {
      if (range.inRange(TimeUnit.MILLISECONDS, time)) {
        count++;
      }
    }
    return count;
  }

  public static class TimeRange {
    final long startTimeMillis;
    final long endTimeMillis;

    public TimeRange(TimeUnit unit, long start, long end) {
      startTimeMillis = unit.toMillis(start);
      endTimeMillis = unit.toMillis(end);
    }

    public boolean inRange(TimeUnit unit, long value) {
      long millis = unit.toMillis(value);
      return millis >= startTimeMillis && millis <= endTimeMillis;
    }
  }

}
