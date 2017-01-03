package com.manning.hip.ch7.shortestpath;

import org.apache.commons.lang.StringUtils;

import java.io.IOException;
import java.util.Arrays;

public class Node {
  private int distance = INFINITE;
  private String backpointer;
  private String[] adjacentNodeNames;

  public static int INFINITE = Integer.MAX_VALUE;
  public static final char fieldSeparator = '\t';

  public int getDistance() {
    return distance;
  }

  public Node setDistance(int distance) {
    this.distance = distance;
    return this;
  }

  public String getBackpointer() {
    return backpointer;
  }

  public Node setBackpointer(String backpointer) {
    this.backpointer = backpointer;
    return this;
  }

  public String constructBackpointer(String name) {
    StringBuilder backpointers = new StringBuilder();
    if (StringUtils.trimToNull(getBackpointer()) != null) {
      backpointers.append(getBackpointer()).append(":");
    }
    backpointers.append(name);
    return backpointers.toString();
  }

  public String[] getAdjacentNodeNames() {
    return adjacentNodeNames;
  }

  public Node setAdjacentNodeNames(String[] adjacentNodeNames) {
    this.adjacentNodeNames = adjacentNodeNames;
    return this;
  }

  public boolean containsAdjacentNodes() {
    return adjacentNodeNames != null;
  }

  public boolean isDistanceSet() {
    return distance != INFINITE;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append(distance)
        .append(fieldSeparator)
        .append(backpointer);

    if (getAdjacentNodeNames() != null) {
      sb.append(fieldSeparator)
          .append(StringUtils
              .join(getAdjacentNodeNames(), fieldSeparator));
    }
    return sb.toString();
  }

  public static Node fromMR(String value) throws IOException {
    String[] parts = StringUtils.splitPreserveAllTokens(
        value, fieldSeparator);
    if (parts.length < 2) {
      throw new IOException(
          "Expected 2 or more parts but received " + parts.length);
    }
    Node node = new Node()
        .setDistance(Integer.valueOf(parts[0]))
        .setBackpointer(StringUtils.trimToNull(parts[1]));
    if (parts.length > 2) {
      node.setAdjacentNodeNames(Arrays.copyOfRange(parts, 2,
          parts.length));
    }
    return node;
  }
}
