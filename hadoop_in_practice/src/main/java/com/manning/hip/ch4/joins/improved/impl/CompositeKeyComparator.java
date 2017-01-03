package com.manning.hip.ch4.joins.improved.impl;

import org.apache.hadoop.io.*;

public class CompositeKeyComparator extends WritableComparator {
  	protected CompositeKeyComparator() {
		super(CompositeKey.class, true);
	}

	@Override
	public int compare(WritableComparable w1, WritableComparable w2) {

		CompositeKey p1 = (CompositeKey) w1;
		CompositeKey p2 = (CompositeKey) w2;

		int cmp = p1.getKey().compareTo(p2.getKey());
		if (cmp != 0) {
			return cmp;
		}

		return p1.getOrder() == p2.getOrder() ? 0 : (p1
				.getOrder() < p2.getOrder() ? -1 : 1);
	}
}
