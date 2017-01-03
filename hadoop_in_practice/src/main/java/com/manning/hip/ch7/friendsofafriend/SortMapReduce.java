package com.manning.hip.ch7.friendsofafriend;


import org.apache.commons.lang.StringUtils;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.*;

import java.io.IOException;

public final class SortMapReduce {
  public static class Map
      extends Mapper<Text, Text, Person, Person> {

    private Person outputKey = new Person();
    private Person outputValue = new Person();

    @Override
    protected void map(Text key, Text value, Context context)
        throws IOException, InterruptedException {
      String[] parts = StringUtils.split(value.toString());
      String name = parts[0];
      int commonFriends = Integer.valueOf(parts[1]);

      outputKey.set(name, commonFriends);
      outputValue.set(key.toString(), commonFriends);
      context.write(outputKey, outputValue);

      outputValue.set(name, commonFriends);
      outputKey.set(key.toString(), commonFriends);
      context.write(outputKey, outputValue);
    }
  }

  public static class Reduce
      extends Reducer<Person, Person, Text, Text> {

    private Text name = new Text();
    private Text potentialFriends = new Text();

    @Override
    public void reduce(Person key, Iterable<Person> values,
                       Context context)
        throws IOException, InterruptedException {

      StringBuilder sb = new StringBuilder();

      // the 2nd-degree friends will be sorted by the number
      // of common friends, so emit the top 10
      //
      int count = 0;
      for (Person potentialFriend : values) {
        if(sb.length() > 0) {
          sb.append(",");
        }
        sb.append(potentialFriend.getName())
            .append(":")
            .append(potentialFriend.getCommonFriends());

        if (++count == 10) {
          break;
        }
      }

      name.set(key.getName());
      potentialFriends.set(sb.toString());
      context.write(name, potentialFriends);
    }
  }
}
