#ifndef HELLOWORLD_H
#define HELLOWORLD_H
#include <iostream>

class HelloWorld
{
 protected:
  double r1, r2, s;
  void compute();    // compute s=sin(r1+r2)
 public:
  HelloWorld();
  ~HelloWorld();
 
  void set(double r1, double r2);
  double get() const { return s; }
  void message(std::ostream& out) const;
};

std::ostream& 
operator << (std::ostream& out, const HelloWorld& hw);
#endif
