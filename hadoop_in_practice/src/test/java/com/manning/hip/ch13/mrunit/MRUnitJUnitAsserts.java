package com.manning.hip.ch13.mrunit;

import org.apache.hadoop.mrunit.TestDriver;
import org.apache.hadoop.mrunit.types.Pair;

import java.util.List;

import static org.junit.Assert.assertEquals;

public class MRUnitJUnitAsserts {

  public static <K1, V1, K2, V2> void assertOutputs(
      TestDriver<K1, V1, K2, V2> driver, List<Pair<K2, V2>> actuals) {

    List<Pair<K2, V2>> expected = driver.getExpectedOutputs();

    assertEquals("Number of expected records don't match actual number",
        expected.size(), actuals.size());

    // make sure all actual outputs are in the expected set,
    // and at the proper position.
    for (int i = 0; i < expected.size(); i++) {
      Pair<K2, V2> actual = actuals.get(i);
      Pair<K2, V2> expect = expected.get(i);
      assertEquals("Records don't match at position " + i,
          expect, actual);
    }
  }
}
