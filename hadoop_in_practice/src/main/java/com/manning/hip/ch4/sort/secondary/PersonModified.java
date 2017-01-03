package com.manning.hip.ch4.sort.secondary;

import org.apache.hadoop.io.*;

import java.io.*;

public class PersonModified
    implements WritableComparable<PersonModified> {

  private String firstName;
  private String lastName;

  public PersonModified() {
  }

  public String getFirstName() {
    return firstName;
  }

  public void setFirstName(String firstName) {
    this.firstName = firstName;
  }

  public String getLastName() {
    return lastName;
  }

  public void setLastName(String lastName) {
    this.lastName = lastName;
  }

  @Override
  public void readFields(DataInput in) throws IOException {
    this.firstName = in.readUTF();
    this.lastName = in.readUTF();
  }

  @Override
  public void write(DataOutput out) throws IOException {
    out.writeUTF(firstName);
    out.writeUTF(lastName);
  }

  @Override
  public int compareTo(PersonModified other) {
    int cmp = this.firstName.compareTo(other.firstName);
    if (cmp != 0) {
      return cmp;
    }
    return this.lastName.compareTo(other.lastName);
  }

  public void set(String lastName, String firstName) {
    this.lastName = lastName;
    this.firstName = firstName;
  }

  public static class PersonKeyComparator extends WritableComparator {
    public PersonKeyComparator() {
      super(PersonModified.class);
    }

    public int compare(byte[] b1, int s1, int l1, byte[] b2, int s2,
                       int l2) {
      return compareBytes(b1, s1, l1, b2, s2, l2);
    }
  }

  static { // register this comparator
    WritableComparator.define(PersonModified.class,
        new PersonKeyComparator());
  }
}
