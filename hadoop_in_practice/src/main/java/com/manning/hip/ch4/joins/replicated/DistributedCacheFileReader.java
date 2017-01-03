package com.manning.hip.ch4.joins.replicated;

import java.io.*;

public interface DistributedCacheFileReader<K, V> extends Iterable<Pair<K, V>> {
  public void init(File f) throws IOException;
  public void close();
}
