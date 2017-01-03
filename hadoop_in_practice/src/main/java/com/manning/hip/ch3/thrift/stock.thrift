namespace java com.manning.hip.ch3.thrift

struct Stock {
  1: string symbol,
  2: string date,
  3: double open,
  4: double high,
  5: double low,
  6: double close,
  7: i32 volume,
  8: double adjClose
}

struct StockAvg {
  1: string symbol,
  2: double avg
}
