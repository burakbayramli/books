package com.manning.hip.ch6;

public class StringConcat {
  public static void main(String ... args) {
    plus(args[0], args[1], args[2]);
    concat(args[0], args[1], args[2]);
  }

  public static String plus(String a, String b, String c) {
    return a + b + c;
  }

  public static String concat(String a, String b, String c) {
    return a.concat(b).concat(c);
  }
}
