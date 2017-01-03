package com.manning.hip.ch4.joins.improved;

import com.manning.hip.ch4.joins.improved.impl.*;
import org.apache.hadoop.io.Text;

public class SampleMap extends OptimizedDataJoinMapperBase {

  private boolean smaller;

  @Override
  protected Text generateInputTag(String inputFile) {
    // tag the row with input file name (data source)
    smaller = inputFile.contains("users.txt");
    return new Text(inputFile);
  }

  @Override
  protected String genGroupKey(Object key, OutputValue output) {
    return key.toString();
  }

  @Override
  protected boolean isInputSmaller(String inputFile) {
    return smaller;
  }

  @Override
  protected OutputValue genMapOutputValue(
      Object o) {
    return new TextTaggedOutputValue((Text) o);
  }
}
