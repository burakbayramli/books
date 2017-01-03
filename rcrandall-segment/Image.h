// Image.h
//
// Author: Robert Crandall
//
// Generic, templated Image class
// Contains pseudo 2d array of type T, and functions
// for memory management

#ifndef IMAGE_H
#define IMAGE_H

#include <assert.h>

template <typename T>
class Image
{
  public:
  
    // Constructors
    Image();
    Image(unsigned int nr, unsigned int nc);
    
    // Destructor; calls Free()
    ~Image(){Free();}
    
    // Free memory allocated by this class
    void Free();
    
    // Set every pixel in the image to a given value
    void Set(T val);
    
    // Reallocate memory for a certain size image
    void Allocate(unsigned int nr, unsigned int nc);
    
    // Return a sub-image of current image
    Image<T>* subImage(unsigned int rowStart,
                       unsigned int colStart,
                       unsigned int rowEnd,
                       unsigned int colEnd);
    
    // Copy data from another image of the same type,
    // resizing if necessary
    void CopyFrom(const Image<T>* imageIn);
    
    // Copy data from an image of smaller or equal size
    // into this image, starting at a given row/col index
    void CopyFrom(const Image<T>* imageIn,
                  unsigned int rowStart,
                  unsigned int colStart);
    
    // Accessors
    T** data() const {return m_Data;}
    unsigned int nRows() const {return m_nRows;}
    unsigned int nCols() const {return m_nCols;}
    
  private:
  
    // Image data
    T** m_Data;
    
    // Image info
    unsigned int m_nRows;
    unsigned int m_nCols;
};

#endif
