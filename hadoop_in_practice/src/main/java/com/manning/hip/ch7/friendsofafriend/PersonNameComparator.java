package com.manning.hip.ch7.friendsofafriend;

import org.apache.hadoop.io.*;

public class PersonNameComparator extends WritableComparator {

	protected PersonNameComparator() {
		super(Person.class, true);
	}

	@Override
	public int compare(WritableComparable o1, WritableComparable o2) {

		Person p1 = (Person) o1;
		Person p2 = (Person) o2;

		return p1.getName().compareTo(p2.getName());

	}
}
