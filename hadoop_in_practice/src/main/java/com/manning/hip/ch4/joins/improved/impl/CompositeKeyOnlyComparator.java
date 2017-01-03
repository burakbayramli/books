package com.manning.hip.ch4.joins.improved.impl;

import org.apache.hadoop.io.*;

public class CompositeKeyOnlyComparator extends WritableComparator {

	protected CompositeKeyOnlyComparator() {
		super(CompositeKey.class, true);
	}

	@Override
	public int compare(WritableComparable o1, WritableComparable o2) {

		CompositeKey p1 = (CompositeKey) o1;
		CompositeKey p2 = (CompositeKey) o2;

		return p1.getKey().compareTo(p2.getKey());

	}
}
