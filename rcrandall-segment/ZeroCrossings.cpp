// Robert Crandall

#include "ZeroCrossings.h"

void ZeroCrossings(Image<double>* imageIn,
                   Image<unsigned char>** edges,
                   unsigned char fg,
                   unsigned char bg)
{
  // Allocate output image if necessary
  if (0 == (*edges))
  {
    (*edges) = new Image<unsigned char>(imageIn->nRows(),imageIn->nCols());
  }
  else
  {
    if ((*edges)->nRows() != imageIn->nRows()
     || (*edges)->nCols() != imageIn->nCols())
    {
      (*edges)->Allocate(imageIn->nRows(),imageIn->nCols());
    } 
  }
  (*edges)->Set(bg);

  for (unsigned int i = 0; i < imageIn->nRows(); ++i)
  {
    for (unsigned int j = 0; j < imageIn->nCols(); ++j)
    {
      // Currently only checking interior pixels to avoid 
      // bounds checking
      if (i > 0 && i < (imageIn->nRows()-1) 
       && j > 0 && j < (imageIn->nCols()-1))
      {
        if (0 == imageIn->data()[i][j])
        {
          if (0 != imageIn->data()[i-1][j-1]
           || 0 != imageIn->data()[i-1][j]
           || 0 != imageIn->data()[i-1][j+1]
           || 0 != imageIn->data()[i][j-1]
           || 0 != imageIn->data()[i][j+1]
           || 0 != imageIn->data()[i+1][j-1]
           || 0 != imageIn->data()[i+1][j]
           || 0 != imageIn->data()[i+1][j+1])
           {
             (*edges)->data()[i][j] = fg;
           }
        }
        else
        {
          if (abs(imageIn->data()[i][j]) < abs(imageIn->data()[i-1][j-1])
           && (imageIn->data()[i][j]>0) != (imageIn->data()[i-1][j-1]>0))
             (*edges)->data()[i][j] = fg;
     else if (abs(imageIn->data()[i][j]) < abs(imageIn->data()[i-1][j])
           && (imageIn->data()[i][j]>0) != (imageIn->data()[i-1][j]>0))
             (*edges)->data()[i][j] = fg;
     else if (abs(imageIn->data()[i][j]) < abs(imageIn->data()[i-1][j+1])
           && (imageIn->data()[i][j]>0) != (imageIn->data()[i-1][j+1]>0))
             (*edges)->data()[i][j] = fg;
     else if (abs(imageIn->data()[i][j]) < abs(imageIn->data()[i][j-1])
           && (imageIn->data()[i][j]>0) != (imageIn->data()[i][j-1]>0))
             (*edges)->data()[i][j] = fg;
     else if (abs(imageIn->data()[i][j]) < abs(imageIn->data()[i][j+1])
           && (imageIn->data()[i][j]>0) != (imageIn->data()[i][j+1]>0))
             (*edges)->data()[i][j] = fg;
     else if (abs(imageIn->data()[i][j]) < abs(imageIn->data()[i+1][j-1])
           && (imageIn->data()[i][j]>0) != (imageIn->data()[i+1][j-1]>0))
             (*edges)->data()[i][j] = fg;
     else if (abs(imageIn->data()[i][j]) < abs(imageIn->data()[i+1][j])
           && (imageIn->data()[i][j]>0) != (imageIn->data()[i+1][j]>0))
             (*edges)->data()[i][j] = fg;
     else if (abs(imageIn->data()[i][j]) < abs(imageIn->data()[i+1][j+1])
           && (imageIn->data()[i][j]>0) != (imageIn->data()[i+1][j+1]>0))
             (*edges)->data()[i][j] = fg;
        }
      }
    }
  }
}