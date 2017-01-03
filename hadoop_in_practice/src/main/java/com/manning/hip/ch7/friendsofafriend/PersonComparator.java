package com.manning.hip.ch7.friendsofafriend;

import org.apache.hadoop.io.*;

public class PersonComparator extends WritableComparator {
  	protected PersonComparator() {
		super(Person.class, true);
	}

	@Override
	public int compare(WritableComparable w1, WritableComparable w2) {

		Person p1 = (Person) w1;
		Person p2 = (Person) w2;

		int cmp = p1.getName().compareTo(p2.getName());
		if (cmp != 0) {
			return cmp;
		}

		return p1.getCommonFriends() == p2.getCommonFriends() ? 0 : (p1
				.getCommonFriends() > p2.getCommonFriends() ? -1 : 1);
	}
}
