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

import org.apache.pig.*;
import org.apache.pig.data.*;
import org.apache.pig.impl.logicalLayer.FrontendException;
import org.apache.pig.impl.logicalLayer.schema.Schema;

import java.io.IOException;
import java.util.*;
import java.util.regex.*;

/**
 * This function removes tuples that match the supplied regex.
 */
public class RegExExclusionFilter extends FilterFunc {

  Pattern pattern;

  public RegExExclusionFilter(String pattern) {
    this.pattern = Pattern.compile(pattern);
  }

  public Boolean exec(Tuple t) throws IOException {
    if (t == null || t.size() == 0)
      return false;

    String query = extractFieldAsString(t, 0);
    if (query == null) {
      return false;
    }
    Matcher m = pattern.matcher(query);
    return (!m.find());
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
          "RegExExclusionFilter: failed to process input; error - " +
              e.getMessage());
      return null;
    }

    if (query.equals("")) {
      return null;
    }
    return query;
  }

  @Override
  public List<FuncSpec> getArgToFuncMapping()
      throws FrontendException {
    List<FuncSpec> funcList = new ArrayList<FuncSpec>();
    funcList.add(new FuncSpec(this.getClass().getName(),
        new Schema(new Schema.FieldSchema(null, DataType.CHARARRAY))));

    return funcList;
  }

}
