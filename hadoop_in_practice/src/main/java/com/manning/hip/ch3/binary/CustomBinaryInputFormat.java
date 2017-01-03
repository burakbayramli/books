package com.manning.hip.ch3.binary;

import org.apache.commons.lang.ArrayUtils;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.BytesWritable;
import org.apache.hadoop.io.IOUtils;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.mapreduce.InputSplit;
import org.apache.hadoop.mapreduce.JobContext;
import org.apache.hadoop.mapreduce.RecordReader;
import org.apache.hadoop.mapreduce.TaskAttemptContext;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.FileSplit;

import java.io.DataInputStream;
import java.io.EOFException;
import java.io.IOException;
import java.util.*;

public class CustomBinaryInputFormat extends
    FileInputFormat<LongWritable, BytesWritable> {

  @Override
  public RecordReader<LongWritable, BytesWritable>
  createRecordReader(InputSplit split,
                     TaskAttemptContext context) {
    return new CustomBinaryRecordReader();
  }

  public long[] getFileOffest(Configuration conf, Path input)
      throws IOException {
    List<Long> offsets = new ArrayList<Long>();
    DataInputStream is = new DataInputStream(
        input.getFileSystem(conf).open(input));
    try {
      long offset = 0;
      while(true) {
        int dataLen = is.readInt();
        is.skipBytes(dataLen);
        offsets.add(offset);
        // set the offset for the next
        offset += dataLen + 4;
      }
    } catch(EOFException e) {
      // we're done
    } finally {
      IOUtils.closeStream(is);
    }
    return ArrayUtils.toPrimitive(offsets.toArray(new Long[]{}));
  }

  public long alignSliceStartToIndex(long[] offsets, long start, long end) {
    if (start != 0) {
      // find the next block position from
      // the start of the split
      long newStart = findNextPosition(offsets, start);
      if (newStart == -1 || newStart >= end) {
        return -1;
      }
      start = newStart;
    }
    return start;
  }

    public long alignSliceEndToIndex(long[] offsets, long end, long fileSize) {
    long newEnd = findNextPosition(offsets, end);
    if (newEnd != -1) {
      end = newEnd;
    } else {
      // didn't find the next position
      // we have hit the end of the file
      end = fileSize;
    }
    return end;
  }

  public long findNextPosition(long[] offsets, long pos) {
    int block = Arrays.binarySearch(offsets, pos);

    if (block >= 0) {
      // direct hit on a block start position
      return offsets[block];
    } else {
      block = Math.abs(block) - 1;
      if (block >= offsets.length) {
        return -1;
      }
      return offsets[block];
    }
  }



  @Override
  public List<InputSplit> getSplits(JobContext job) throws IOException {
    List<InputSplit> defaultSplits = super.getSplits(job);

    List<InputSplit> result = new ArrayList<InputSplit>();

    Path prevPath = null;
    long[] offsets = null;

    for (InputSplit genericSplit : defaultSplits) {
      FileSplit fileSplit = (FileSplit)genericSplit;
      Path file = fileSplit.getPath();

      if(prevPath == null || !prevPath.equals(file)) {
        prevPath = file;
        // Load the offsets in the file
        offsets = getFileOffest(job.getConfiguration(), file);
      }

      long start = fileSplit.getStart();
      long end = start + fileSplit.getLength();

      long newStart = alignSliceStartToIndex(offsets, start, end);
      long newEnd = alignSliceEndToIndex(offsets, end,
          file.getFileSystem(job.getConfiguration())
              .getFileStatus(file).getLen());

      if(newStart != -1 || newEnd != -1) {
        result.add(new FileSplit(file, newStart, newEnd - newStart, fileSplit.getLocations()));
        System.out.println("Adding split start = " + newStart + " end = " + newEnd);
      }
    }

    return result;
  }

  @Override
  protected boolean isSplitable(JobContext context, Path file) {
    return super.isSplitable(context, file);
  }
}
