package com.manning.hip.ch11;

import org.apache.pig.data.Tuple;

import java.io.IOException;

public class PigLongGeolocationUDF extends PigGeolocationUDF {

  public String exec(Tuple input) throws IOException {
    Exception e = new Exception();
    e.printStackTrace();
    if (input == null || input.size() == 0) {
      return null;
    }

    Object object = input.get(0);
    if (object == null) {
      return null;
    }

    Long ip = (Long) object;

    System.out.println("got " + object + " ip form = " + intToIp(ip));

    return super.lookup(intToIp(ip));
  }

  public static String intToIp(long i) {
    return ((i >> 24) & 0xFF) + "." +
      ((i >> 16) & 0xFF) + "." +
      ((i >> 8) & 0xFF) + "." +
      (i & 0xFF);
  }
}
