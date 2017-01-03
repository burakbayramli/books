package com.manning.hip.ch12.crunch;

import com.cloudera.crunch.*;
import com.cloudera.crunch.impl.mr.MRPipeline;
import com.cloudera.crunch.lib.Join;
import com.cloudera.crunch.type.PTypeFamily;
import com.manning.hip.common.CommonLogEntry;
import org.apache.commons.lang.StringUtils;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;

import java.io.IOException;

/**
 * Parse Apache logs and join them with a separate users file.
 */
public class JoinLogsAndUsers {
  public static void main(String[] args) throws IOException {
    Configuration conf = new Configuration();
    Path output = new Path(args[2]);
    output.getFileSystem(conf).delete(output, true);

    // Create an object to coordinate pipeline creation and execution.
    Pipeline pipeline = new MRPipeline(JoinLogsAndUsers.class, conf);

    // Reference a given text file as a collection of Strings.
    PCollection<String> rawLogs = pipeline.readTextFile(args[0]);

    // Reference a given text file as a collection of Strings.
    PCollection<String> rawUsers = pipeline.readTextFile(args[1]);

    // Define a function that splits each line in a PCollection of Strings into a
    // PCollection made up of the individual words in the file.
    PTable<String, CommonLogEntry> logs = logsAsIpTable(CrunchUtils.logs(rawLogs));

    PTable<String, String> ipsAndUsers = ipsAndUsers(rawUsers);

    PTable<String, Pair<String, CommonLogEntry>> joined = Join.join(ipsAndUsers, logs);

    for(Pair<String, Pair<String, CommonLogEntry>> j: joined.materialize()) {
      System.out.println(j.first() + " " + j.second().first());
    }
  }

  public static PTable<String, CommonLogEntry> logsAsIpTable(PCollection<CommonLogEntry> logs) {
    PTypeFamily tf = logs.getTypeFamily();
    return logs.parallelDo(
        "logs-to-ip-table",
        new DoFn<CommonLogEntry, Pair<String, CommonLogEntry>>() {
          @Override
          public void process(CommonLogEntry input, Emitter<Pair<String, CommonLogEntry>> emitter) {
            emitter.emit(Pair.of(input.getRemoteAddress(), input));
          }
        }, tf.tableOf(tf.strings(), tf.records(CommonLogEntry.class)));
  }

  public static PTable<String, String> ipsAndUsers(PCollection<String> ipUsers) {
    PTypeFamily tf = ipUsers.getTypeFamily();
    return ipUsers.parallelDo(
        "extract-users",
        new DoFn<String, Pair<String, String>>() {
          @Override
          public void process(String input, Emitter<Pair<String, String>> emitter) {
            // first token is the IP address, and second is the username
            String[] parts = StringUtils.split(input);
            emitter.emit(Pair.of(parts[0], parts[1]));
          }
        }, tf.tableOf(tf.strings(), tf.strings()));
  }
}
