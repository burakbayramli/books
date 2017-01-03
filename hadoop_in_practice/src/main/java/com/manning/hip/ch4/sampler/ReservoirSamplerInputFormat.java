package com.manning.hip.ch4.sampler;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.util.ReflectionUtils;

import java.io.IOException;
import java.util.*;

public class ReservoirSamplerInputFormat<K extends Writable, V extends Writable>
    extends InputFormat {

  public static final String INPUT_FORMAT_CLASS =
      "reservoir.inputformat.class";
  public static final String SAMPLES_NUMBER =
      "reservoir.samples.number";
  public static final String USE_SAMPLES_NUMBER_PER_INPUT_SPLIT =
      "reservoir.samples.useperinputsplit";
  public static final String MAXRECORDS_READ =
      "reservoir.samples.maxrecordsread";


  public static final int DEFAULT_NUM_SAMPLES = 1000;
  public static final boolean DEFAULT_USE_SAMPLES_PER_INPUT_SPLIT =
      false;
  public static final int DEFAULT_MAX_RECORDS_READ = 100000;

  private InputFormat<K, V> inputFormat;

  /**
   * A helper function to configure the actual InputFormat for the job.
   *
   * @param inputFormat the input format which will be wrapped by
   *                    the sampler
   */
  public static void setInputFormat(Job job,
                                    Class<? extends InputFormat> inputFormat) {
    job.getConfiguration().setClass(INPUT_FORMAT_CLASS, inputFormat,
        InputFormat.class);
    job.setInputFormatClass(ReservoirSamplerInputFormat.class);
  }

  public static void setNumSamples(Job job,
                                   int numSamples) {
    job.getConfiguration().setInt(SAMPLES_NUMBER, numSamples);
  }

  public static void setMaxRecordsToRead(Job job,
                                         int maxRecords) {
    job.getConfiguration().setInt(MAXRECORDS_READ, maxRecords);
  }

  public static void setUseSamplesNumberPerInputSplit(Job job,
                                                      boolean usePerInputSplit) {
    job.getConfiguration().setBoolean(
        USE_SAMPLES_NUMBER_PER_INPUT_SPLIT, usePerInputSplit);
  }

  public static int getNumSamples(Configuration conf) {
    int numSamples = conf.getInt(SAMPLES_NUMBER, DEFAULT_NUM_SAMPLES);
    boolean usePerSample =
        conf.getBoolean(USE_SAMPLES_NUMBER_PER_INPUT_SPLIT,
            DEFAULT_USE_SAMPLES_PER_INPUT_SPLIT);
    if (usePerSample) {
      return numSamples;
    }
    int numMapTasks = conf.getInt("mapred.map.tasks", 1);

    return (int) Math.ceil(numSamples / numMapTasks);
  }

  public static int getMaxRecordsToRead(Configuration conf) {
    return conf.getInt(MAXRECORDS_READ, DEFAULT_MAX_RECORDS_READ);
  }

  @SuppressWarnings("unchecked")
  public InputFormat<K, V> getInputFormat(Configuration conf)
      throws IOException {
    if (inputFormat == null) {
      Class ifClass = conf.getClass(INPUT_FORMAT_CLASS, null);

      if (ifClass == null) {
        throw new IOException("Job must be configured with " +
            INPUT_FORMAT_CLASS);
      }
      inputFormat = (InputFormat<K, V>) ReflectionUtils
          .newInstance(ifClass, conf);
    }
    return inputFormat;
  }

  @Override
  public List<InputSplit> getSplits(JobContext context)
      throws IOException, InterruptedException {
    return getInputFormat(context.getConfiguration())
        .getSplits(context);
  }

  @Override
  @SuppressWarnings("unchecked")
  public RecordReader createRecordReader(InputSplit split,
                                         TaskAttemptContext context)
      throws IOException, InterruptedException {

    Configuration conf = context.getConfiguration();

    return new ReservoirSamplerRecordReader(context,
        getInputFormat(conf).createRecordReader(split, context),
        getNumSamples(conf),
        getMaxRecordsToRead(conf));
  }

  public static class ReservoirSamplerRecordReader<K extends Writable, V extends Writable>
      extends RecordReader {

    private final Configuration conf;
    private final RecordReader<K, V> rr;
    private final int numSamples;
    private final int maxRecords;

    private final ArrayList<K> keys;
    private final ArrayList<V> values;

    private int idx = 0;

    public ReservoirSamplerRecordReader(TaskAttemptContext context,
                                        RecordReader<K, V> rr,
                                        int numSamples,
                                        int maxRecords) {
      this.conf = context.getConfiguration();
      this.rr = rr;
      this.numSamples = numSamples;
      this.maxRecords = maxRecords;
      keys = new ArrayList<K>(numSamples);
      values = new ArrayList<V>(numSamples);
    }

    @Override
    public void initialize(InputSplit split,
                           TaskAttemptContext context)
        throws IOException, InterruptedException {
      rr.initialize(split, context);

      Random rand = new Random();
      for (int i = 0; i < maxRecords; i++) {
        if (!rr.nextKeyValue()) {
          break;
        }
        K key = rr.getCurrentKey();
        V val = rr.getCurrentValue();

        if (keys.size() < numSamples) {
          keys.add(WritableUtils.clone(key, conf));
          values.add(WritableUtils.clone(val, conf));
        } else {
          int r = rand.nextInt(i);
          if (r < numSamples) {
            keys.set(r, WritableUtils.clone(key, conf));
            values.set(r, WritableUtils.clone(val, conf));
          }
        }
      }
    }

    @Override
    public boolean nextKeyValue()
        throws IOException, InterruptedException {
      return idx++ < keys.size();
    }

    @Override
    public K getCurrentKey()
        throws IOException, InterruptedException {
      return keys.get(idx - 1);
    }

    @Override
    public Object getCurrentValue()
        throws IOException, InterruptedException {
      return values.get(idx - 1);
    }

    @Override
    public float getProgress()
        throws IOException, InterruptedException {
      return Math.min(idx, keys.size()) / keys.size();
    }

    @Override
    public void close() throws IOException {
      rr.close();
    }
  }
}
