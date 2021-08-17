// RAND.h
//
// 2006/10/15
//---------------------------------------------------------
#ifndef NDG__RAND_H__INCLUDED
#define NDG__RAND_H__INCLUDED

#include <cstdlib>
#include <ctime>
#include <algorithm>


// manage random numbers
//---------------------------------------------------------
class umRAND
//---------------------------------------------------------
{
  double inv_RandMax;

  umRAND() 
    : inv_RandMax(1.0/double(RAND_MAX)) 
  {
    srand ((unsigned int)(time(NULL)));
  }

  double make_rand(double from, double to) const
  {
    static double dMin=0.0, dMax=0.0;
    dMin = std::min(from, to);
    dMax = std::max(from, to);
    double nr = (double)(rand());
  //return dMin + nr * (dMax - dMin) / m_RandMax;
    return dMin + nr * (dMax - dMin) * inv_RandMax;
  }

public:
  ~umRAND() {}

  static double get_rand(double from=0.0, double to=1.0)
  {
    static umRAND r;
    return r.make_rand(from, to);
  }
};

#endif  // NDG__RAND_H__INCLUDED
