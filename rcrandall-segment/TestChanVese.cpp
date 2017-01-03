// Robert Crandall

#include "ReadWritePBM.h"
#include "ChanVeseSegmentation.h"
#include "ZeroCrossings.h"

int main()
{
  cout << "Reading in image...\n";
  Image<unsigned char>* img = ReadP5image("test.pgm");
  Image<unsigned char>* imageOut = new Image<unsigned char>(img->nRows(),
                                                            img->nCols());
  Image<double>* phi0 = new Image<double>(img->nRows(),
                                          img->nCols());
  Image<double>* phi = new Image<double>(img->nRows(),
                                         img->nCols());
  Image<unsigned char>* edges = new Image<unsigned char>(img->nRows(),
                                                         img->nCols());
                                         

  // Set up parameters (don't hardcode this!)
  struct CVsetup* pCVinputs = new struct CVsetup;
  pCVinputs->dt = 0.1;
  pCVinputs->h = 1.0;
  pCVinputs->lambda1 = 1.0;
  pCVinputs->lambda2 = 1.0;
  pCVinputs->mu = 0.5;
  pCVinputs->nu = 0;
  pCVinputs->p = 1;

  // Set up initial circular contour for a 256x256 image
  double x;
  double y;
  for (unsigned int i = 0; i < img->nRows(); ++i)
  {
    for (unsigned int j = 0; j < img->nCols(); ++j)
    {
      x = double(i) - img->nRows()/2.0;
      y = double(j) - img->nCols()/2.0;
      phi0->data()[i][j] = 900.0/(900.0 + x*x + y*y) - 0.5;
    }
  }

  cout << "Performing segmentation...\n";
  ChanVeseSegmentation(img,phi0, &phi, pCVinputs);
  cout << "Getting zero crossings...\n";
  ZeroCrossings(phi,&edges,(unsigned char)255,(unsigned char)0);
  
  imageOut->CopyFrom(img);
  
  for (unsigned int i = 0; i < img->nRows(); ++i)
  {
    for (unsigned int j = 0; j < img->nCols(); ++j)
    {
      if (edges->data()[i][j] == (unsigned char)255)
        imageOut->data()[i][j] = (unsigned char)255;
    }
  }
  
  cout << "Writing output...\n";
  WriteP5image(imageOut,"CVoutEdges.pgm",255);
  
  imageOut->CopyFrom(img);
  for (unsigned int i = 0; i < img->nRows(); ++i)
  {
    for (unsigned int j = 0; j < img->nCols(); ++j)
    {
      if (phi->data()[i][j] >= 0)
        imageOut->data()[i][j] = (unsigned char)0;
      else
        imageOut->data()[i][j] = (unsigned char)255;
    }
  }
  WriteP5image(imageOut,"CVoutRegions.pgm",255);
  cout << "Done.\n";
}
