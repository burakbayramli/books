package com.manning.hip.ch2;

import com.manning.hip.ch3.StockPriceWritable;
import org.apache.hadoop.io.Writable;
import org.apache.hadoop.mapred.lib.db.DBWritable;

import java.sql.*;
import java.text.*;

public class StockRecord extends StockPriceWritable
    implements Writable, DBWritable {

  private final static SimpleDateFormat sdf =
      new SimpleDateFormat("yyyy-MM-dd");
  public static String [] fields = { "symbol", "quote_date" ,
      "open_price", "high_price", "low_price", "close_price",
      "volume", "adj_close_price"};

  @Override
  public void readFields(ResultSet resultSet) throws SQLException {
    int idx=2;
    setSymbol(resultSet.getString(idx++));
    setDate(sdf.format(resultSet.getDate(idx++)));
    setOpen(resultSet.getDouble(idx++));
    setHigh(resultSet.getDouble(idx++));
    setLow(resultSet.getDouble(idx++));
    setClose(resultSet.getDouble(idx++));
    setVolume(resultSet.getInt(idx++));
    setAdjClose(resultSet.getDouble(idx));
  }

  @Override
  public void write(PreparedStatement statement) throws SQLException {
    int idx=1;
    statement.setString(idx++, getSymbol());
    try {
      statement.setDate(idx++,
          new Date(sdf.parse(getDate()).getTime()));
    } catch (ParseException e) {
      throw new SQLException("Failed to convert String to date", e);
    }
    statement.setDouble(idx++, getOpen());
    statement.setDouble(idx++, getHigh());
    statement.setDouble(idx++, getLow());
    statement.setDouble(idx++, getClose());
    statement.setInt(idx++, getVolume());
    statement.setDouble(idx, getAdjClose());
  }
}
