package com.manning.hip.ch7.shortestpath;

import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Mapper;

import java.io.IOException;

public class Map
    extends Mapper<Text, Text, Text, Text> {

  private Text outKey = new Text();
  private Text outValue = new Text();

  @Override
  protected void map(Text key, Text value, Context context)
      throws IOException, InterruptedException {

    Node node = Node.fromMR(value.toString());

    System.out.println("input -> K[" + key + "],V[" + node + "]");

    // output this node's key/value pair again to preserve the information
    //
    System.out.println(
        "  output -> K[" + key + "],V[" + value + "]");
    context.write(key, value);

    // only output the neighbor details if we have an actual distance
    // from the source node
    //
    if (node.isDistanceSet()) {
      // our neighbors are just a hop away
      //
      int neighborDistance = node.getDistance() + 1;

      // create the backpointer, which will append our own
      // node name to the list
      //
      String backpointer = node.constructBackpointer(key.toString());

      // go through all the nodes and propagate the distance to them
      //
      for (int i = 0; i < node.getAdjacentNodeNames().length; i++) {

        String neighbor = node.getAdjacentNodeNames()[i];

        // output the neighbor with the propagated distance and backpointer
        //
        outKey.set(neighbor);

        Node adjacentNode = new Node()
            .setDistance(neighborDistance)
            .setBackpointer(backpointer);

        outValue.set(adjacentNode.toString());
        System.out.println(
            "  output -> K[" + outKey + "],V[" + outValue + "]");
        context.write(outKey, outValue);
      }
    }
  }
}
