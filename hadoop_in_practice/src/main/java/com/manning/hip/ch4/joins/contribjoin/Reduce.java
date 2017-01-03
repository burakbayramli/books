package com.manning.hip.ch4.joins.contribjoin;

import org.apache.commons.lang.StringUtils;
import org.apache.hadoop.contrib.utils.join.*;
import org.apache.hadoop.io.*;

public class Reduce extends DataJoinReducerBase {

  /**
   * Perform an inner join
   *
   * @param tags
   *          a list of source tags
   * @param values
   *          a value per source
   * @return combined value derived from values of the sources
   */
  private TextTaggedMapOutput output = new TextTaggedMapOutput();
  private Text textOutput = new Text();
  protected TaggedMapOutput combine(Object[] tags, Object[] values) {
    // an inner join requires that both sides contain an entry for the
    // join key
    //
    if (tags.length < 2)
       return null;  

    StringBuilder joinedStr = new StringBuilder();
    for (int i=0; i<tags.length; i++) {
      if (i > 0) {
         joinedStr.append("\t");
      }
      // strip first column as it is the key on which we joined
      String line = ((((TaggedMapOutput) values[i]).getData())).toString();
      String[] tokens = StringUtils.split(line, "\t", 2);
      joinedStr.append(tokens[1]);
    }
    textOutput.set(joinedStr.toString());
    output.setData(textOutput);
    //output.setTag((Text) tags[0]);
    return output;
  }
}
