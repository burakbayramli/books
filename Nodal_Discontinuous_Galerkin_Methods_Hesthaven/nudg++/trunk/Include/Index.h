// Index.h
// Simple 1-D index range [lo:hi]
// 2006/10/15
//---------------------------------------------------------
#ifndef NDG__umIndex_H__INCLUDED
#define NDG__umIndex_H__INCLUDED

#include "ArrayMacros.h"


//---------------------------------------------------------
class Index1D
//---------------------------------------------------------
{
  int lbound_, ubound_;

public:

  int lbound() const { return lbound_; }
  int     lo() const { return lbound_; }

  int ubound() const { return ubound_; }
  int     hi() const { return ubound_; }

  int      N() const { return 1+(hi()-lo()); }

  Index1D(const Index1D &D)  : lbound_(D.lbound_), ubound_(D.ubound_) {}
  Index1D(int i1=0, int i2=1): lbound_(i1), ubound_(i2) {}

  void reset(int lo, int hi)
  {
    lbound_ = lo;  ubound_ = hi;
  }

  Index1D& operator=(const Index1D &D) {
    lbound_ = D.lbound_;  ubound_ = D.ubound_;
    return *this;
  }

  Index1D& operator += (const Index1D &D) {
    lbound_ += D.lbound(); ubound_ += D.ubound();
    return (*this); 
  }
  Index1D& operator += (int i) {
    lbound_ += i; ubound_ += i;
    return (*this); 
  }

  Index1D& operator -= (const Index1D &D) {
    lbound_ -= D.lbound(); ubound_ -= D.ubound();
    return (*this); 
  }
  Index1D& operator -= (int i) {
    lbound_ -= i; ubound_ -= i;
    return (*this); 
  }
};

inline Index1D operator+(const Index1D &D, int i) { return Index1D(i+D.lbound(), i+D.ubound()); }
inline Index1D operator+(int i, const Index1D &D) { return Index1D(i+D.lbound(), i+D.ubound()); }
inline Index1D operator-(Index1D &D, int i)       { return Index1D(D.lbound()-i, D.ubound()-i); }
inline Index1D operator-(int i, Index1D &D)       { return Index1D(i-D.lbound(), i-D.ubound()); }

#endif  // NDG__umIndex_H__INCLUDED
