package com.manning.hip.common;

import org.apache.commons.lang.StringUtils;

import java.util.*;

public class PaddedTable {
  private final int maxColLength;

  private final List<String> columns = new ArrayList<String>();
  private final List<Integer> maxColumnLengths = new ArrayList<Integer>();
  private final List<List<String>> rows = new ArrayList<List<String>>();

  public PaddedTable() {
    maxColLength = Integer.MAX_VALUE;
  }

  public PaddedTable(int maxColLength) {
    this.maxColLength = maxColLength;
  }

  public PaddedTable addColumnTitle(String title) {
    title = abbreviate(title);
    columns.add(title);
    maxColumnLengths.add(title.length());
    return this;
  }

  public PaddedTable newRow() {
    rows.add(new ArrayList<String>());
    return this;
  }

  public PaddedTable clearRows() {
    rows.clear();
    return this;
  }

  public PaddedTable addColumnValue(String value) {
    addColumnValueNoAbbreviate(abbreviate(value));
    return this;
  }

  public PaddedTable addColumnValueNoAbbreviate(String value) {
    rows.get(rows.size()-1).add(value);
    int colIndex = rows.get(rows.size()-1).size()-1;
    maxColumnLengths.set(colIndex,
        Math.max(maxColumnLengths.get(colIndex), value.length()));
    return this;
  }

  public PaddedTable addColumnValue(int value) {
    String valueAsString = String.valueOf(value);

    addColumnValueNoAbbreviate(valueAsString);
    return this;
  }

  public PaddedTable addColumnValue(long value) {
    String valueAsString = String.valueOf(value);

    addColumnValueNoAbbreviate(valueAsString);
    return this;
  }

  public PaddedTable addColumnValue(double value) {
    String valueAsString = String.format("%.2f", value);

    addColumnValueNoAbbreviate(valueAsString);
    return this;
  }

  public String abbreviate(String s) {
    return StringUtils.abbreviate(s, maxColLength);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    // header first
    for(int i=0; i < columns.size(); i++) {
      appendToBuilder(sb, i, columns.get(i));
    }
    sb.append(String.format("%n"));
    for (List<String> row : rows) {
      for (int j = 0; j < row.size(); j++) {
        appendToBuilder(sb, j, row.get(j));
      }
      sb.append(String.format("%n"));
    }
    return sb.toString();
  }

  public void appendToBuilder(StringBuilder sb, int columnIdx, String value) {
    int globalPadding = 2;
    int paddingRequired =  globalPadding + maxColumnLengths.get(columnIdx);
    sb.append(String.format("%" + paddingRequired + "s", value));
  }
}
