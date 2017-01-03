package com.manning.hip.ch4.joins.improved.impl;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.hadoop.io.*;

import java.io.*;

public class CompositeKey implements WritableComparable<CompositeKey> {

	private String key = "";
	private long order = 0;

  public CompositeKey() {
  }

  public CompositeKey(String key, long order) {
    this.key = key;
    this.order = order;
  }

	public String getKey() {
		return this.key;
	}

	public long getOrder() {
		return this.order;
	}

  public void setKey(String key) {
    this.key = key;
  }

  public void setOrder(long order) {
    this.order = order;
  }

  @Override
	public void readFields(DataInput in) throws IOException {
		this.key = in.readUTF();
		this.order = in.readLong();
	}

	@Override
	public void write(DataOutput out) throws IOException {
		out.writeUTF(key);
		out.writeLong(this.order);
	}

	@Override
	public int compareTo(CompositeKey other) {
		if (this.key.compareTo(other.key) != 0) {
			return this.key.compareTo(other.key);
		} else if (this.order != other.order) {
			return order < other.order ? -1 : 1;
		} else {
			return 0;
		}
	}

	public static class CompositeKeyComparator extends WritableComparator {
		public CompositeKeyComparator() {
			super(CompositeKey.class);
		}

		public int compare(byte[] b1, int s1, int l1, byte[] b2, int s2, int l2) {
			return compareBytes(b1, s1, l1, b2, s2, l2);
		}
	}

  @Override
  public String toString() {
    return ToStringBuilder.reflectionToString(this);
  }



	static { // register this comparator
		WritableComparator.define(CompositeKey.class,
				new CompositeKeyComparator());
	}
}
