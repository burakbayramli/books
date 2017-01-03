// R. Crandall 
// 
// Functions to read/write NetPBM image files

#ifndef READ_WRITE_PBM_H
#define READ_WRITE_PBM_H

#include "Image.h"
#include <fstream>
#include <string>
#include <iostream>
#include <cstdlib>

using namespace std;

// Read/write a P5 image (binary PGM data)
Image<unsigned char>* ReadP5image(string filename);
void WriteP5image(Image<unsigned char>* image, string filename, int grayLevel);

#endif
