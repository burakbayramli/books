package com.manning.hip.ch7.friendsofafriend;

import org.apache.hadoop.io.*;

import java.io.*;

public class Person implements WritableComparable<Person> {

	private String name = "";
	private long commonFriends = 0;

  public Person() {
  }

  public Person(String name, long commonFriends) {
    this.name = name;
    this.commonFriends = commonFriends;
  }

  public void set(String name, long commonFriends) {
		this.name = name;
		this.commonFriends = commonFriends;
	}

	public String getName() {
		return this.name;
	}

	public long getCommonFriends() {
		return this.commonFriends;
	}

	@Override
	public void readFields(DataInput in) throws IOException {
		this.name = in.readUTF();
		this.commonFriends = in.readLong();
	}

	@Override
	public void write(DataOutput out) throws IOException {
		out.writeUTF(name);
		out.writeLong(this.commonFriends);
	}

	@Override
	public int compareTo(Person other) {
		if (this.name.compareTo(other.name) != 0) {
			return this.name.compareTo(other.name);
		} else if (this.commonFriends != other.commonFriends) {
			return commonFriends < other.commonFriends ? -1 : 1;
		} else {
			return 0;
		}
	}

	public static class PersonKeyComparator extends WritableComparator {
		public PersonKeyComparator() {
			super(Person.class);
		}

		public int compare(byte[] b1, int s1, int l1, byte[] b2, int s2, int l2) {
			return compareBytes(b1, s1, l1, b2, s2, l2);
		}
	}

	static { // register this comparator
		WritableComparator.define(Person.class,
				new PersonKeyComparator());
	}
}
