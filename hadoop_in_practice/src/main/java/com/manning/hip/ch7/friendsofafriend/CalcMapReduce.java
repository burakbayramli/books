package com.manning.hip.ch7.friendsofafriend;


import org.apache.commons.lang.StringUtils;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;

import java.io.IOException;

public final class CalcMapReduce {

  public static class TextPair extends Text {
    public static char separator = '\t';

    public TextPair() {
      super();
    }

    public TextPair(String person1, String person2) {
      super(joinPersonsLexicographically(person1, person2));
    }

    public void set(String person1, String person2) {
      super.set(joinPersonsLexicographically(person1, person2));
    }

    public static String joinPersonsLexicographically(String person1,
                                                      String person2) {
      if (person1.compareTo(person2) < 0) {
        return person1 + separator + person2;
      }
      return person2 + separator + person1;
    }
  }

  public static class Map
      extends Mapper<Text, Text, TextPair, IntWritable> {

    private TextPair pair = new TextPair();
    private IntWritable one = new IntWritable(1);
    private IntWritable two = new IntWritable(2);

    @Override
    protected void map(Text key, Text value, Context context)
        throws IOException, InterruptedException {
      String[] friends = StringUtils.split(value.toString());
      for (int i = 0; i < friends.length; i++) {

        // they already know each other, so emit the pair with
        // a "1" to indicate this
        //
        pair.set(key.toString(), friends[i]);
        context.write(pair, one);

        // go through all the remaining friends in the list
        // and emit the fact that they are 2nd-degree friends
        //
        for (int j = i + 1; j < friends.length; j++) {
          pair.set(friends[i], friends[j]);
          context.write(pair, two);
        }
      }
    }
  }

  public static class Reduce
      extends Reducer<TextPair, IntWritable, TextPair, IntWritable> {

    private IntWritable friendsInCommon = new IntWritable();

    public void reduce(TextPair key, Iterable<IntWritable> values,
                       Context context)
        throws IOException, InterruptedException {

      int commonFriends = 0;
      boolean alreadyFriends = false;
      // if the friends know each other then we'll eventually
      // see a value will be a "1", which will cause us to
      // break out of the loop
      //
      for (IntWritable hops : values) {
        if (hops.get() == 1) {
          alreadyFriends = true;
          break;
        }

        commonFriends++;
      }
      if (!alreadyFriends) {
        friendsInCommon.set(commonFriends);
        context.write(key, friendsInCommon);
      }
    }
  }
}
