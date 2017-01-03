/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.manning.hip.ch3.seqfile;

import com.manning.hip.ch3.StockPriceWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.InputFormat;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.RecordReader;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.SequenceFileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.SequenceFileRecordReader;
import org.apache.pig.FileInputLoadFunc;
import org.apache.pig.backend.hadoop.executionengine.mapReduceLayer.PigSplit;
import org.apache.pig.data.Tuple;
import org.apache.pig.data.TupleFactory;

import java.io.IOException;
import java.util.Arrays;

public class SequenceFileStockLoader extends FileInputLoadFunc {

  private SequenceFileRecordReader<Text, StockPriceWritable> reader;

  @Override
  public Tuple getNext() throws IOException {
    boolean next;
    try {
      next = reader.nextKeyValue();
    } catch (InterruptedException e) {
      throw new IOException(e);
    }

    if (!next) return null;

    Object value = reader.getCurrentValue();

    if (value == null) {
      return null;
    }
    if (!(value instanceof StockPriceWritable)) {
      return null;
    }
    StockPriceWritable w = (StockPriceWritable) value;

    return TupleFactory.getInstance().newTuple(Arrays.asList(
        w.getSymbol(), w.getDate(), w.getOpen(),
        w.getHigh(), w.getLow(), w.getClose(),
        w.getVolume(), w.getAdjClose()
    ));
  }

  @SuppressWarnings("unchecked")
  @Override
  public InputFormat getInputFormat() throws IOException {
    return new SequenceFileInputFormat<Text, StockPriceWritable>();
  }

  @SuppressWarnings("unchecked")
  @Override
  public void prepareToRead(RecordReader reader, PigSplit split)
      throws IOException {
    this.reader = (SequenceFileRecordReader) reader;
  }

  @Override
  public void setLocation(String location, Job job)
      throws IOException {
    FileInputFormat.setInputPaths(job, location);
  }
}
