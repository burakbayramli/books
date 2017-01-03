package com.manning.hip.ch4.joins.improved.impl;

import org.apache.hadoop.mapred.*;

public class CompositeKeyPartitioner implements
    Partitioner<CompositeKey, OutputValue> {

	@Override
	public int getPartition(CompositeKey key, OutputValue value,
			int numPartitions) {
		return Math.abs(key.getKey().hashCode() * 127) % numPartitions;
	}

  @Override
  public void configure(JobConf job) {
  }
}
