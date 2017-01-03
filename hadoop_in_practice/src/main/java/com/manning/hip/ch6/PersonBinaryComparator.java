package com.manning.hip.ch6;

import com.manning.hip.ch4.sort.secondary.Person;
import org.apache.hadoop.io.WritableComparator;

public class PersonBinaryComparator extends WritableComparator {
  protected PersonBinaryComparator() {
    super(Person.class, true);
  }

  @Override
  public int compare(byte[] b1, int s1, int l1, byte[] b2, int s2,
                     int l2) {

    int lastNameResult = compare(b1, s1, b2, s2);

    if (lastNameResult != 0) {
      return lastNameResult;
    }

    int b1l1 = readUnsignedShort(b1, s1);
    int b2l1 = readUnsignedShort(b2, s2);

    return compare(b1, s1 + b1l1 + 2, b2, s2 + b2l1 + 2);
  }

  public static int compare(byte[] b1, int s1, byte[] b2, int s2) {
    int b1l1 = readUnsignedShort(b1, s1);
    int b2l1 = readUnsignedShort(b2, s2);

    return compareBytes(b1, s1 + 2, b1l1, b2, s2 + 2, b2l1);
  }

  public static int readUnsignedShort(byte[] b, int offset) {
    int ch1 = b[offset];
    int ch2 = b[offset + 1];
    return (ch1 << 8) + (ch2);
  }
}
