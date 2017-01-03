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

import org.apache.commons.logging.*;
import org.apache.hadoop.mapred.*;

import java.util.*;
import java.util.Map.Entry;

/**
 * A common base implementing some statics collecting mechanisms that are
 * commonly used in a typical map/reduce job.
 * 
 */
public abstract class OptimizedJobBase implements Mapper, Reducer {

  public static final Log LOG = LogFactory.getLog("optimizeddatajoin.job");

  private SortedMap<Object, Long> longCounters = null;

  /**
   * Increment the given counter by the given incremental value If the counter
   * does not exist, one is created with value 0.
   * 
   * @param name
   *          the counter name
   * @param inc
   *          the incremental value
   * @return the updated value.
   */
  protected Long addLongValue(Object name, long inc) {
    Long val = this.longCounters.get(name);
    Long retv;
    if (val == null) {
      retv = inc;
    } else {
      retv = val + inc;
    }
    this.longCounters.put(name, retv);
    return retv;
  }

  /**
   * log the counters
   * 
   */
  protected String getReport() {
    StringBuilder sb = new StringBuilder();

    for (Entry<Object, Long> e: this.longCounters.entrySet()) {
      sb.append(e.getKey().toString())
          .append("\t")
          .append(e.getValue())
          .append("\n");
    }
    return sb.toString();
  }

  /**
   * Initializes a new instance from a {@link org.apache.hadoop.mapred.JobConf}.
   * 
   * @param job
   *          the configuration
   */
  public void configure(JobConf job) {
    this.longCounters = new TreeMap<Object, Long>();
  }
}
