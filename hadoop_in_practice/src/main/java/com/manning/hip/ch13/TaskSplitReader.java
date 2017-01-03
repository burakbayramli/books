package com.manning.hip.ch13;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FSDataInputStream;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.io.serializer.Deserializer;
import org.apache.hadoop.io.serializer.SerializationFactory;
import org.apache.hadoop.mapreduce.split.JobSplit;

import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.IOException;

public class TaskSplitReader {

  public static void main(String... args) throws IOException {

    String taskSplitFile = args[0];

    Configuration conf = new Configuration();

    DataInputStream is =
        new DataInputStream(new FileInputStream(taskSplitFile));

    JobSplit.TaskSplitIndex taskSplitIndex =
        new JobSplit.TaskSplitIndex();
    taskSplitIndex.readFields(is);
    is.close();

    Object split = getSplitDetails(conf,
        new Path(taskSplitIndex.getSplitLocation()),
        taskSplitIndex.getStartOffset());

    System.out.println(
        "InputSplit instance class = " + split.getClass().getName());
    System.out.println("ToString on split = " + split);
    System.out.println("Reflection fields = " + ToStringBuilder
        .reflectionToString(split, ToStringStyle.SHORT_PREFIX_STYLE));
  }

  public static <T> T getSplitDetails(Configuration conf, Path file,
                                      long offset)
      throws IOException {
    FileSystem fs = file.getFileSystem(conf);
    FSDataInputStream inFile = fs.open(file);
    inFile.seek(offset);
    String className = Text.readString(inFile);
    Class<T> cls;
    try {
      cls = (Class<T>) conf.getClassByName(className);
    } catch (ClassNotFoundException ce) {
      IOException wrap = new IOException("Split class " + className +
          " not found");
      wrap.initCause(ce);
      throw wrap;
    }
    SerializationFactory factory = new SerializationFactory(conf);
    Deserializer<T> deserializer =
        factory.getDeserializer(cls);
    deserializer.open(inFile);
    T split = deserializer.deserialize(null);
    inFile.close();
    return split;
  }
}
