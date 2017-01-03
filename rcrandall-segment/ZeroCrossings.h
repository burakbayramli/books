// Robert Crandall

#ifndef ZERO_CROSSINGS_H
#define ZERO_CROSSINGS_H

#include "Image.h"
#include <cmath>
using std::abs;

void ZeroCrossings(Image<double>* imageIn,
                   Image<unsigned char>** edges,
                   unsigned char fg,
                   unsigned char bg);

#endif
