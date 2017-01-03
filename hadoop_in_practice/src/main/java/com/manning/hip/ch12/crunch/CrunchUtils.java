package com.manning.hip.ch12.crunch;

import com.cloudera.crunch.*;
import com.cloudera.crunch.type.PTypeFamily;
import com.manning.hip.common.ApacheCommonLogReader;
import com.manning.hip.common.CommonLogEntry;
import org.apache.commons.lang.StringUtils;
import org.apache.hadoop.mapreduce.MapContext;
import org.apache.hadoop.mapreduce.TaskInputOutputContext;
import org.apache.hadoop.mapreduce.lib.input.FileSplit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.Nullable;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

public class CrunchUtils {

  /**
   * Returns the largest numerical element from the input collection.
   */
  public static <K> PTable<K, String> uniqueValues(PTable<K, String> collect) {
    return collect.groupByKey().combineValues(new CombineFn<K, String>() {

      @Override
      public void process(Pair<K, Iterable<String>> input,
                          Emitter<Pair<K, String>> emitter) {

        Set<String> filenames = new HashSet<String>();

        for (String filename : input.second()) {
          filenames.add(filename);
        }

        Pair<K, String> pair =
            Pair.of(input.first(), StringUtils.join(filenames, ","));
        emitter.emit(pair);
      }
    });
  }

  public static PTable<String, String> extractWordFileTable(PCollection<String> lines) {
    PTypeFamily tf = lines.getTypeFamily();
    return lines.parallelDo(
        "inverted-index",
        new DoFn<String, Pair<String, String>>() {
          String filename;

          @Override
          public void setContext(TaskInputOutputContext<?, ?, ?, ?> context) {
            super.setContext(context);
            filename = ((FileSplit)
                ((MapContext) context).getInputSplit()).getPath().getName();
          }

          @Override
          public void process(String line,
                              Emitter<Pair<String, String>> emitter) {
            for (String word : StringUtils.split(line)) {
              Pair<String, String> pair =
                  Pair.of(word.toLowerCase(), filename);
              emitter.emit(pair);
            }
          }
        }, tf.tableOf(tf.strings(), tf.strings()));
  }

  public static enum LogCounters {
    LOG_LINE_ERRORS
  }

  public static PCollection<CommonLogEntry> logs(PCollection<String> lines) {
    PTypeFamily tf = lines.getTypeFamily();
    return lines
        .parallelDo(new DoFn<String, CommonLogEntry>() {
          transient ApacheCommonLogReader logReader;
          transient Logger log;

          @Override
          public void initialize() {
            logReader = new ApacheCommonLogReader();
            log = LoggerFactory.getLogger(CrunchUtils.class);
          }

          @Override
          public void process(String input, Emitter<CommonLogEntry> emitter) {
            try {
              CommonLogEntry log = logReader.decodeLine(input);
              if(log != null) {
                emitter.emit(log);
              } else {
                processingError(input, null);
              }
            } catch (IOException e) {
              processingError(input, e);
            }
          }

          void processingError(String line, @Nullable Throwable t) {
            super.getCounter(LogCounters.LOG_LINE_ERRORS).increment(1);
            log.error("Hit exception parsing line '" + line + "'", t);
          }
        }, tf.records(CommonLogEntry.class));
  }

}
