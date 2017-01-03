package com.manning.hip.ch7.shortestpath;

import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.*;

import java.io.IOException;

public class Reduce
    extends Reducer<Text, Text, Text, Text> {

  public static enum PathCounter {
    TARGET_NODE_DISTANCE_COMPUTED,
    PATH
  }

  private Text outValue = new Text();
  private String targetNode;

  protected void setup(Context context
  ) throws IOException, InterruptedException {
    targetNode = context.getConfiguration().get(
        Main.TARGET_NODE);
  }

  public void reduce(Text key, Iterable<Text> values,
                     Context context)
      throws IOException, InterruptedException {

    int minDistance = Node.INFINITE;

    System.out.println("input -> K[" + key + "]");

    Node shortestAdjacentNode = null;
    Node originalNode = null;

    for (Text textValue : values) {
      System.out.println("  input -> V[" + textValue + "]");

      Node node = Node.fromMR(textValue.toString());

      if(node.containsAdjacentNodes()) {
        // the original data
        //
        originalNode = node;
      }

      if(node.getDistance() < minDistance) {
        minDistance = node.getDistance();
        shortestAdjacentNode = node;
      }
    }

    if(shortestAdjacentNode != null) {
      originalNode.setDistance(minDistance);
      originalNode.setBackpointer(shortestAdjacentNode.getBackpointer());
    }

    outValue.set(originalNode.toString());

    System.out.println(
        "  output -> K[" + key + "],V[" + outValue + "]");
    context.write(key, outValue);

    if (minDistance != Node.INFINITE &&
        targetNode.equals(key.toString())) {
      Counter counter = context.getCounter(
          PathCounter.TARGET_NODE_DISTANCE_COMPUTED);
      counter.increment(minDistance);
      context.getCounter(PathCounter.PATH.toString(),
          shortestAdjacentNode.getBackpointer()).increment(1);
    }
  }
}
