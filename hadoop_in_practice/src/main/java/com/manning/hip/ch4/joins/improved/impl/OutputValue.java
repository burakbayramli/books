/**
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

package com.manning.hip.ch4.joins.improved.impl;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapred.JobConf;

/**
 * This abstract class serves as the base class for the values that 
 * flow from the mappers to the reducers in a data join job. 
 * Typically, in such a job, the mappers will compute the source
 * tag of an input record based on its attributes or based on the 
 * file name of the input file. This tag will be used by the reducers
 * to re-group the values of a given key according to their source tags.
 * 
 */
public abstract class OutputValue implements Writable {
  protected BooleanWritable smaller;

  public OutputValue() {
    this.smaller = new BooleanWritable(false);
  }

  public BooleanWritable isSmaller() {
    return smaller;
  }

  public void setSmaller(BooleanWritable smaller) {
    this.smaller = smaller;
  }

  public abstract Writable getData();
  
  public OutputValue clone(JobConf job) {
    return WritableUtils.clone(this, job);
  }

  @Override
  public String toString() {
    return ToStringBuilder.reflectionToString(this);
  }
}
