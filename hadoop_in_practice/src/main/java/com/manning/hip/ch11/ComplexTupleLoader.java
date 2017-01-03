/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license agreements. See the
 * NOTICE file distributed with this work for additional information regarding copyright ownership. The ASF
 * licenses this file to you under the Apache License, Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is
 * distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and limitations under the License.
 */

package com.manning.hip.ch11;

import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.mapreduce.lib.input.*;
import org.apache.pig.*;
import org.apache.pig.backend.executionengine.ExecException;
import org.apache.pig.backend.hadoop.executionengine.mapReduceLayer.PigSplit;
import org.apache.pig.data.*;
import org.apache.pig.impl.logicalLayer.schema.Schema;

import java.io.IOException;
import java.util.*;

/**
 * TypedCommonLogLoader is used to load logs based on Apache's
 * Common Log Format (CLF), based on the following format:
 * <p/>
 * "%h %l %u %t \"%r\" %>s %b"
 * <p/>
 * Example of log line:
 * <p/>
 * 127.0.0.1 - frank [10/Oct/2000:13:55:36 -0700] "GET /apache_pb.gif HTTP/1.0" 200 2326
 * <p/>
 * Example use of LoadFunc:
 * <p/>
 * raw = LOAD 'common_log.txt' USING com.manning.hip.ch7.TypedCommonLogLoader;
 * <p/>
 * describe raw;
 * logs: {remoteAddr: chararray,remoteLogname: chararray,userid: chararray,
 * time: chararray,requestLine: chararray,statusCode: long,objSize: long,
 * method: chararray,resource: chararray,protocol: chararray,epoch: long}
 */

public class ComplexTupleLoader extends FileInputLoadFunc
    implements LoadMetadata {

 protected RecordReader reader = null;
  private TupleFactory tupleFactory = TupleFactory.getInstance();
  private BagFactory bagFactory = BagFactory.getInstance();

  @Override
  public Tuple getNext() throws IOException {
    try {
      if(!reader.nextKeyValue()) {
        return null;
      }
    } catch (InterruptedException e) {
      int errCode = 6018;
      String errMsg = "Error while reading input";
      throw new ExecException(errMsg, errCode,
        PigException.REMOTE_ENVIRONMENT, e);
    }    ArrayList<Object> tuple = new ArrayList<Object>();
    tuple.add("127.0.0.1");

    Map<String, Object> header = new HashMap<String, Object>();
    header.put("User-Agent", "Mozilla");
    tuple.add(header);

    ArrayList<Tuple> bodyTuples = new ArrayList<Tuple>();
    bodyTuples.add(newBodyTuple("keyword1"));
    bodyTuples.add(newBodyTuple("keyword2"));
    tuple.add(bagFactory.newDefaultBag(bodyTuples));

    return tupleFactory.newTuple(tuple);
  }

  public Tuple newBodyTuple(String line) {
    return tupleFactory.newTuple(Arrays.asList(line));
  }

  @Override
  public void setLocation(String location, Job job)
      throws IOException {
    FileInputFormat.setInputPaths(job, location);
  }

  @SuppressWarnings("rawtypes")
  @Override
  public InputFormat getInputFormat() throws IOException {
    return new TextInputFormat();
  }

  @Override
  public void prepareToRead(
      @SuppressWarnings("rawtypes") RecordReader reader,
      PigSplit split)
      throws IOException {
    this.reader = reader;
  }

  public ResourceSchema getSchema(String location, Job job)
      throws IOException {

    Schema schema = new Schema();

    Schema headerSchema = new Schema(
        Arrays.asList(
            new Schema.FieldSchema("word", DataType.CHARARRAY)
        ));

    schema.add(new Schema.FieldSchema("ip", DataType.CHARARRAY));
    schema.add(new Schema.FieldSchema("header", DataType.MAP));
    schema.add(new Schema.FieldSchema("keywords", headerSchema));
    return new ResourceSchema(schema);
  }

  public ResourceStatistics getStatistics(String location, Job job)
      throws IOException {
    return null;
  }

  public String[] getPartitionKeys(String location, Job job)
      throws IOException {
    return null;
  }

  public void setPartitionFilter(Expression partitionFilter)
      throws IOException {
  }
}
