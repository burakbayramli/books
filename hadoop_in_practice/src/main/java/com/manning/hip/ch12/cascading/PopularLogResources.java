/*
 * Copyright (c) 2007-2010 Concurrent, Inc. All Rights Reserved.
 *
 * Project and contact information: http://www.cascading.org/
 *
 * This file is part of the Cascading project.
 *
 * Cascading is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Cascading is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Cascading.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.manning.hip.ch12.cascading;

import cascading.flow.Flow;
import cascading.flow.FlowConnector;
import cascading.operation.Aggregator;
import cascading.operation.aggregator.Count;
import cascading.operation.regex.RegexParser;
import cascading.pipe.Each;
import cascading.pipe.Every;
import cascading.pipe.GroupBy;
import cascading.pipe.Pipe;
import cascading.scheme.TextLine;
import cascading.tap.Hfs;
import cascading.tap.SinkMode;
import cascading.tap.Tap;
import cascading.tuple.Fields;

import java.util.Properties;

/**
 *
 */
public class PopularLogResources {
  public static void main(String[] args) {
    String inputPath = args[0];
    String outputPath = args[1];

    // define what the input file looks like, "offset" is bytes from beginning
    TextLine input = new TextLine(new Fields("offset", "line"));

    // create SOURCE tap to read a resource from HDFS
    Tap logTap = new Hfs(input, inputPath);

    // create an assembly to parse an Apache log file and store on an HDFS cluster

    // declare the field names we will parse out of the log file
    Fields apacheFields = new Fields("resource");

    // define the regular expression to parse the log file with
    String apacheRegex = "^([^ ]*) +[^ ]* +[^ ]* +\\[([^]]*)\\] +\\\"([^ ]*) ([^ ]*) [^ ]*\\\" ([^ ]*) ([^ ]*).*$";

    // declare the groups from the above regex we want to keep. each regex group will be given
    // a field name from 'apacheFields', above, respectively
    int[] allGroups = {4};

    // create the parser
    RegexParser parser = new RegexParser(apacheFields, apacheRegex, allGroups);

    // create the import pipe element, with the name 'import', and with the input argument named "line"
    // replace the incoming tuple with the parser results
    // "line" -> parser -> "ts"
    Pipe pipeline = new Each("import", new Fields("line"), parser, Fields.RESULTS);


    // group the Tuple stream by the "word" value
    pipeline = new GroupBy(pipeline, new Fields("resource"));

    // For every Tuple group
    // count the number of occurrences of "word" and store result in
    // a field named "count"
    Aggregator count = new Count(new Fields("resource"));
    pipeline = new Every(pipeline, count);


    // create a SINK tap to write to the default filesystem
    // by default, TextLine writes all fields out
    Tap remoteLogTap = new Hfs(new TextLine(), outputPath, SinkMode.REPLACE);

    // set the current job jar
    Properties properties = new Properties();
    FlowConnector.setApplicationJarClass(properties, PopularLogResources.class);

    // connect the assembly to the SOURCE and SINK taps
    Flow parsedLogFlow = new FlowConnector(properties).connect(logTap, remoteLogTap, pipeline);

    // start execution of the flow (either locally or on the cluster
    parsedLogFlow.start();

    // block until the flow completes
    parsedLogFlow.complete();
  }
}
