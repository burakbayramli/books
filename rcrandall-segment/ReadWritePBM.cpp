// R. Crandall
// Functions to read/write PBM images

#include "ReadWritePBM.h"

// Read a P5 image from a file
Image<unsigned char>* ReadP5image(string filename)
{
  ifstream ifs; 
  char tmp[100];
  int numRows = 0;
  int numCols = 0;
  int grayLevel = 0;
  Image<unsigned char>* image = new Image<unsigned char>();
  
  ifs.open(filename.c_str(), ifstream::in);
  
  if (!ifs)
  {
    cerr << "Error in ReadP5image; could not open file " << filename << endl;
  }
  
  // Read first line of header; should read P5
  ifs.getline(tmp,100,'\n');
  
  if (  (80 != tmp[0])   // P
     || (53 != tmp[1]) ) // 5
  {
    cerr << "Error, " << filename << " is not a valid P5 image\n";
  }
  
  // Skip comments if any, then read numCols and numRows
  ifs.getline(tmp,100,'\n');
  char* next = 0;
  while('#' == tmp[0])
  {
    ifs.getline(tmp,100,'\n');
  }
  numCols = strtol(tmp,&next,10);
  numRows = strtol(next,0,10);
  
  // Get gray level
  ifs.getline(tmp,100,'\n');
  grayLevel = strtol(tmp,0,10); //unused for now
  
  // Read pixel data into image
  image->Allocate(numRows,numCols);
  for (int i = 0; i < numRows; ++i)
  {
    ifs.read( reinterpret_cast<char*>(image->data()[i]),
              numCols*sizeof(unsigned char));
  }
  
  // Check for error, and close file
  if (ifs.fail())
  {
    cerr << "Error reading pixel data in " << filename << ".  "
         << "Image size may not match header.\n";
  }
  ifs.close();
  
  return image;
}

// Write image data to a binary PGM format file
void WriteP5image(Image<unsigned char>* image, string filename, int grayLevel)
{
  ofstream ofs;
  
  // Open file for writing
  ofs.open(filename.c_str(), ofstream::out | ofstream::binary);
  if (!ofs.is_open())
  {
    cerr << "Error, could not open file " << filename << endl;
  }
  else
  {
    // Write ASCII header
    ofs << "P5" << endl
        << image->nCols() << " " << image->nRows() << endl
        << grayLevel << endl;
    
    // Write binary data
    for (unsigned int i = 0; i < image->nRows(); ++i)
    {
      ofs.write( reinterpret_cast<char*>(image->data()[i]),
                 (image->nCols())*sizeof(unsigned char));
    }
    if(ofs.fail())
    {
      cerr << "Failed to write image to file " << filename << endl;
    }
  }
  
  ofs.close();
}


