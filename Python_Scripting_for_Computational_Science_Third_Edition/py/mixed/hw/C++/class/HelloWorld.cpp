#include "HelloWorld.h"
#include <math.h>

HelloWorld:: HelloWorld()
{ r1 = r2 = 0;  compute(); }

HelloWorld:: ~HelloWorld() {}

void HelloWorld:: compute()
{ s = sin(r1 + r2); }

void HelloWorld:: set(double r1_, double r2_)
{ 
  r1 = r1_;  r2 = r2_; 
  compute();  // compute s
}

void HelloWorld:: message(std::ostream& out) const
{
  out << "Hello, World! sin(" << r1 << " + " 
      << r2 << ")=" << get() << std::endl;
}

std::ostream& 
operator << (std::ostream& out, const HelloWorld& hw)
{ hw.message(out);  return out; }
