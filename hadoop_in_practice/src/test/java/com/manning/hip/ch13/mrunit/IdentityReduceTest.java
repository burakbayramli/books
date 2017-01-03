package com.manning.hip.ch13.mrunit;

import junit.framework.TestCase;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mrunit.mapreduce.ReduceDriver;
import org.apache.hadoop.mrunit.types.Pair;
import org.junit.Before;
import org.junit.Test;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

/**
 * Example test of the IdentityMapper to demonstrate proper MapDriver
 * usage in a test case.
 */
public class IdentityReduceTest extends TestCase {

  private Reducer<Text, Text, Text, Text> reducer;
  private ReduceDriver<Text, Text, Text, Text> driver;

  @Before
  public void setUp() {
    reducer = new Reducer<Text, Text, Text, Text>();
    driver = new ReduceDriver<Text, Text, Text, Text>(reducer);
  }

  @Test
  public void testIdentityMapper() throws IOException {
    List<Pair<Text, Text>> results = driver
        .withInput(new Text("foo"), Arrays.asList(new Text("bar1"), new Text("bar2")))
        .withOutput(new Text("foo"), new Text("bar1"))
        .withOutput(new Text("foo"), new Text("bar2"))
        .run();

    MRUnitJUnitAsserts.assertOutputs(driver, results);
  }
}

