package com.manning.hip.ch7.pagerank;

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

    context.write(key, value);

    Node node = Node.fromMR(value.toString());

    if(node.getAdjacentNodeNames() != null &&
        node.getAdjacentNodeNames().length > 0) {
      double outboundPageRank = node.getPageRank() /
          (double)node.getAdjacentNodeNames().length;

      // go through all the nodes and propagate PageRank to them
      //
      for (int i = 0; i < node.getAdjacentNodeNames().length; i++) {

        String neighbor = node.getAdjacentNodeNames()[i];

        outKey.set(neighbor);

        Node adjacentNode = new Node()
            .setPageRank(outboundPageRank);

        outValue.set(adjacentNode.toString());
        System.out.println(
            "  output -> K[" + outKey + "],V[" + outValue + "]");
        context.write(outKey, outValue);
      }
    }
  }
}
