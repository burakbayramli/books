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

import com.manning.hip.common.CommonLogEntry;
import com.manning.hip.common.CommonLogInputFormat;
import org.apache.hadoop.mapreduce.InputFormat;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.RecordReader;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.pig.*;
import org.apache.pig.backend.executionengine.ExecException;
import org.apache.pig.backend.hadoop.executionengine.mapReduceLayer.PigSplit;
import org.apache.pig.data.DataType;
import org.apache.pig.data.Tuple;
import org.apache.pig.data.TupleFactory;
import org.apache.pig.impl.logicalLayer.schema.Schema;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

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

public class TypedCommonLogLoader extends FileInputLoadFunc
  implements LoadMetadata, TypedCommonLogLoaderConstants {

  protected CommonLogInputFormat.CommonLogRecordReader in = null;

  private ArrayList<Object> tuple = null;
  private TupleFactory tupleFactory = TupleFactory.getInstance();

  @Override
  public Tuple getNext() throws IOException {

    tuple = new ArrayList<Object>(11);

    for (int i = 0; i < 11; i++) {
      tuple.add(null);
    }

    try {
      if (!in.nextKeyValue()) {
        return null;
      }
      setTuple(in.getCurrentValue());

      return tupleFactory.newTupleNoCopy(tuple);
    } catch (InterruptedException e) {
      int errCode = 6018;
      String errMsg = "Error while reading input";
      throw new ExecException(errMsg, errCode,
        PigException.REMOTE_ENVIRONMENT, e);
    }

  }

  private void setTuple(CommonLogEntry entry) throws IOException {
    tuple.set(0, entry.getRemoteAddress());
    tuple.set(1, entry.getRemoteLogname());
    tuple.set(2, entry.getUserId());
    tuple.set(3, entry.getTime());
    tuple.set(4, entry.getRequestLine());
    tuple.set(5, entry.getStatusCode());
    tuple.set(6, entry.getObjSize());
    tuple.set(7, entry.getMethod());
    tuple.set(8, entry.getResource());
    tuple.set(9, entry.getProtocol());
    tuple.set(10, entry.getEpoch());
  }

  @Override
  public void setLocation(String location, Job job)
    throws IOException {
    FileInputFormat.setInputPaths(job, location);
  }

  @SuppressWarnings("rawtypes")
  @Override
  public InputFormat getInputFormat() throws IOException {
    return new CommonLogInputFormat();
  }

  @Override
  public void prepareToRead(
    @SuppressWarnings("rawtypes") RecordReader reader, PigSplit split)
    throws IOException {
    in = (CommonLogInputFormat.CommonLogRecordReader) reader;
  }

  @Override
  public ResourceSchema getSchema(String location, Job job)
    throws IOException {
    return new ResourceSchema(new Schema(
      Arrays.asList(
        new Schema.FieldSchema(REMOTE_ADDR, DataType.CHARARRAY),
        new Schema.FieldSchema(REMOTE_LOGNAME, DataType.CHARARRAY),
        new Schema.FieldSchema(USERID, DataType.CHARARRAY),
        new Schema.FieldSchema(TIME, DataType.CHARARRAY),
        new Schema.FieldSchema(REQUEST_LINE, DataType.CHARARRAY),
        new Schema.FieldSchema(STATUS_CODE, DataType.LONG),
        new Schema.FieldSchema(OBJ_SIZE, DataType.LONG),
        new Schema.FieldSchema(METHOD, DataType.CHARARRAY),
        new Schema.FieldSchema(RESOURCE, DataType.CHARARRAY),
        new Schema.FieldSchema(PROTOCOL, DataType.CHARARRAY),
        new Schema.FieldSchema(EPOCH, DataType.LONG)
      )));
  }

  @Override
  public ResourceStatistics getStatistics(String location, Job job)
    throws IOException {
    return null;
  }

  @Override
  public String[] getPartitionKeys(String location, Job job)
    throws IOException {
    return null;
  }

  @Override
  public void setPartitionFilter(Expression partitionFilter)
    throws IOException {
  }
}
