package com.manning.hip.common;

public class Range<T extends Comparable> {

  private T from = null;
  private T to = null;

  public Range(T start, T end) {
    this.from = start;
    this.to = end;
  }

  public boolean contains(T value) {
    return
        from.compareTo(value) <= 0 &&
            to.compareTo(value) >= 0;
  }
}
