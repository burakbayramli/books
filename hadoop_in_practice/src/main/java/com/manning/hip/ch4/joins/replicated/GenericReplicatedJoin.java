package com.manning.hip.ch4.joins.replicated;

import org.apache.hadoop.filecache.DistributedCache;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.lib.input.FileSplit;

import java.io.*;
import java.util.*;

public class GenericReplicatedJoin
    extends Mapper<Object, Object, Object, Object> {

  private Map<Object, List<Pair>> cachedRecords =
      new HashMap<Object, List<Pair>>();
  private boolean distributedCacheIsSmaller;
  private Path[] distributedCacheFiles;

  /**
   * Transforms a record from the input split to a Pair object.  The
   * input splits are ostensibly larger than the Distributed Cache file.
   * <p/>
   * This implementation works with keys and values produced with the
   * KeyValueTextInputFormat, or any InputFormat which yeilds keys and
   * values with meaningful toString methods.  For other input formats, this method
   * should be overridden to convert the key/value into a Pair where
   * the key is text.
   *
   * @param key   the key emitted by the {@link org.apache.hadoop.mapreduce.InputFormat}
   * @param value the key emitted by the {@link org.apache.hadoop.mapreduce.InputFormat}
   * @return a Pair object, where the key is a Text object containing
   *         the key for joining purposes, and the value contains data which
   *         will be used when creating the composite output key
   */
  public Pair readFromInputFormat(Object key, Object value) {
    return new Pair<String, String>(key.toString(), value.toString());
  }


  /**
   * Get the object which will be used to read data from the
   * Distributed Cache file.  The Distrubuted Cache files are referred
   * to as "R" files.  They are ostensibly the smaller files compared
   * to "L" files.
   * <p/>
   * The default implementation provides an implementation that works
   * with line-based text files, where keys and values are separated
   * with whitespace.
   *
   * @return a reader which can unmarshall data from the Distributed
   *         Cache
   */
  public DistributedCacheFileReader getDistributedCacheReader() {
    return new TextDistributedCacheFileReader();
  }

  /**
   * Join together a record from the input split and Distributed Cache
   * and return a new pair which will be emitted by the Map.
   * <p/>
   * If a null is returned, no output will be produced.
   * <p/>
   * The default implementation assumes that the Pair keys and values
   * are Strings and concatenates them together delimited by the
   * tab character.
   * <p/>
   * This should be overridden in cases where the values aren't Strings,
   * or to change how the output value is created.
   *
   * @param inputSplitPair a record from the input split
   * @param distCachePair  a record from the Distributed Cache
   * @return a composite output value which is compatible with the
   *         expected value type for the {@link org.apache.hadoop.mapreduce.OutputFormat}
   *         used for this job
   */
  public Pair join(Pair inputSplitPair, Pair distCachePair) {
    StringBuilder sb = new StringBuilder();
    if (inputSplitPair.getData() != null) {
      sb.append(inputSplitPair.getData());
    }
    sb.append("\t");
    if (distCachePair.getData() != null) {
      sb.append(distCachePair.getData());
    }
    return new Pair<Text, Text>(
        new Text(inputSplitPair.getKey().toString()),
        new Text(sb.toString()));
  }


  @Override
  protected void setup(
      Context context)
      throws IOException, InterruptedException {

    distributedCacheFiles = DistributedCache.getLocalCacheFiles(
        context.getConfiguration());

    int distCacheSizes = 0;
    for (Path distFile : distributedCacheFiles) {
      File distributedCacheFile = new File(distFile.toString());
      distCacheSizes += distributedCacheFile.length();
    }

    if(context.getInputSplit() instanceof FileSplit) {
      FileSplit split = (FileSplit) context.getInputSplit();

      long inputSplitSize = split.getLength();

      distributedCacheIsSmaller = (distCacheSizes < inputSplitSize);
    } else {
      // if the input split isn't a FileSplit, then assume the
      // distributed cache is smaller than the input split
      //
      distributedCacheIsSmaller = true;
    }


    System.out.println(
        "distributedCacheIsSmaller = " + distributedCacheIsSmaller);

    if (distributedCacheIsSmaller) {
      for (Path distFile : distributedCacheFiles) {
        File distributedCacheFile = new File(distFile.toString());
        DistributedCacheFileReader reader =
            getDistributedCacheReader();
        reader.init(distributedCacheFile);
        for (Pair p : (Iterable<Pair>) reader) {
          addToCache(p);
        }
        reader.close();
      }
    }
  }

  private void addToCache(Pair pair) {
    List<Pair> values = cachedRecords.get(pair.getKey());
    if (values == null) {
      values = new ArrayList<Pair>();
      cachedRecords.put(pair.getKey(), values);
    }
    values.add(pair);
  }

  @Override
  protected void map(Object key, Object value, Context context)
      throws IOException, InterruptedException {
    System.out.println("K[" + key + "]");

    Pair pair = readFromInputFormat(key, value);
    if (distributedCacheIsSmaller) {
      joinAndCollect(pair, context);
    } else {
      addToCache(pair);
    }
  }


  public void joinAndCollect(Pair p, Context context)
      throws IOException, InterruptedException {
    List<Pair> cached = cachedRecords.get(p.getKey());
    if (cached != null) {
      for (Pair cp : cached) {
        Pair result;
        if (distributedCacheIsSmaller) {
          result = join(p, cp);
        } else {
          result = join(cp, p);
        }
        if (result != null) {
          context.write(result.getKey(), result.getData());
        }
      }
    }
  }

  @Override
  protected void cleanup(
      Context context)
      throws IOException, InterruptedException {
    if (!distributedCacheIsSmaller) {
      System.out.println("Outputting in cleanup");

      for (Path distFile : distributedCacheFiles) {
        File distributedCacheFile = new File(distFile.toString());
        DistributedCacheFileReader reader =
            getDistributedCacheReader();
        reader.init(distributedCacheFile);
        for (Pair p : (Iterable<Pair>) reader) {
          joinAndCollect(p, context);
        }
        reader.close();
      }
    }
  }
}
