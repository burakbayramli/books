// Stopwatch.h
// A basic stopwatch object
// 2006/10/15
//---------------------------------------------------------
#ifndef NDG__Stopwatch_H__INCLUDED
#define NDG__Stopwatch_H__INCLUDED

#include <ctime>

// Usage:
// void    start() : start timing
// double  stop()  : stop timing
// void    reset() : set elapsed time to 0.0
// double  read()  : read elapsed time (in seconds)


inline double seconds(void)
{
  static const double secs_per_tick = 1.0 / CLOCKS_PER_SEC;
  return ((double)clock()) * secs_per_tick;
}


//---------------------------------------------------------
class stopwatch 
//---------------------------------------------------------
{
private:
  bool    running;
  double  last_time;
  double  total;

public:
  stopwatch() : running(false), last_time(0.0), total(0.0) {}

  void reset() { running = false; last_time = 0.0; total=0.0; }

  void start() {
    if (!running) { 
      last_time = seconds(); 
      running = true;
    }
  }

  double stop() {
    if (running) {
      total += seconds() - last_time; 
      running = false;
    }
    return total; 
  }

  double read() {
    if (running) {
      total+= seconds() - last_time;
      last_time = seconds();
    }
    return total;
  }

};

#endif  // NDG__Stopwatch_H__INCLUDED
