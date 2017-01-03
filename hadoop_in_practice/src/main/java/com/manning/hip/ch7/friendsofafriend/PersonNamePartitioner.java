package com.manning.hip.ch7.friendsofafriend;

import org.apache.hadoop.mapreduce.Partitioner;

public class PersonNamePartitioner extends
    Partitioner<Person, Person> {

	@Override
	public int getPartition(Person key, Person value,
			int numPartitions) {
		return Math.abs(key.getName().hashCode() * 127) % numPartitions;
	}
}
