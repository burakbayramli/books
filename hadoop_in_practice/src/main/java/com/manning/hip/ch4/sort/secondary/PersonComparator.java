package com.manning.hip.ch4.sort.secondary;

import org.apache.hadoop.io.*;

public class PersonComparator extends WritableComparator {
  protected PersonComparator() {
    super(Person.class, true);
  }

  @Override
  public int compare(WritableComparable w1, WritableComparable w2) {

    Person p1 = (Person) w1;
    Person p2 = (Person) w2;

    int cmp = p1.getLastName().compareTo(p2.getLastName());
    if (cmp != 0) {
      return cmp;
    }

    return p1.getFirstName().compareTo(p2.getFirstName());
  }
}
