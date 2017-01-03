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
package com.manning.hip.ch11;

import com.manning.hip.common.Range;
import org.apache.commons.lang.StringUtils;
import org.apache.pig.FilterFunc;
import org.apache.pig.FuncSpec;
import org.apache.pig.data.DataType;
import org.apache.pig.data.Tuple;
import org.apache.pig.impl.logicalLayer.FrontendException;
import org.apache.pig.impl.logicalLayer.schema.Schema;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * This function removes tuples that contain a private IP address.
 */
public class IsPrivateIP extends FilterFunc {

  protected List<Range<Long>> ipRanges;

  public IsPrivateIP() {
    ipRanges = new ArrayList<Range<Long>>();
    ipRanges.add(getRange("10.0.0.0", "10.255.255.255"));
    ipRanges.add(getRange("172.16.0.0", "172.31.255.255"));
    ipRanges.add(getRange("192.168.0.0", "192.168.255.255"));
  }

  @Override
  public List<FuncSpec> getArgToFuncMapping()
      throws FrontendException {
    List<FuncSpec> funcList = new ArrayList<FuncSpec>();
    funcList.add(new FuncSpec(this.getClass().getName(),
        new Schema(new Schema.FieldSchema(null, DataType.CHARARRAY))));

    return funcList;
  }

  @Override
  public Boolean exec(Tuple t) throws IOException {
    if (t == null || t.size() == 0)
      return false;

    String address = extractFieldAsString(t, 0);
    return address != null && matchesIp(ipToInt(address));
  }

  public static Range<Long> getRange(String startIp, String endIp) {
    return new Range<Long>(ipToInt(startIp), ipToInt(endIp));
  }

  public boolean matchesIp(long ip) {
    for(Range<Long> range: ipRanges) {
      if(range.contains(ip)) {
        return true;
      }
    }
    return false;
  }

  public String extractFieldAsString(Tuple t, int field) {
    String query;
    try {
      query = (String) t.get(field);
      if (query == null)
        return null;
      query = query.trim();
    } catch (Exception e) {
      System.err.println(
          "PrivateIPFilter: failed to process input; error - " +
              e.getMessage());
      return null;
    }

    if (query.equals("")) {
      return null;
    }
    return query;
  }


  public static long ipToInt(String addr) {
    String[] octets = StringUtils.split(addr, ".");
    long ip = 0;
    for (String octet : octets) {
      ip = (ip << 8) + Integer.valueOf(octet);
    }
    return ip;
  }

}
