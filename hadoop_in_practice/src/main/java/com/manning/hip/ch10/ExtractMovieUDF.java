package com.manning.hip.ch10;

import org.apache.commons.lang.StringUtils;
import org.apache.hadoop.hive.ql.exec.UDF;
import org.apache.hadoop.io.Text;

public class ExtractMovieUDF extends UDF {
  private Text result = new Text();
  public Text evaluate(final Text t) {
    if (t == null) { return null; }
    String s = t.toString();
    String[] parts = StringUtils.split(s, " ");
    if(parts.length != 3) {
      return null;
    }
    String path = parts[1];
    if(!path.startsWith("/movie/")) {
      return null;
    }
    result.set(path.substring(7));
    return result;
  }
}
