// R. Crandall
// Implementation of functions in class Image
#include "Image.h"

// Default constructor.  Creates empty image
template <typename T>
Image<T>::Image()
{
  m_Data = 0;
  m_nRows = 0;
  m_nCols = 0;
}

// Constructor to create an image of a given size
template <typename T> 
Image<T>::Image(unsigned int nr, 
                unsigned int nc)
{
  m_Data = 0;
  Allocate(nr, nc);
}

// Free memory allocated for this image
template <typename T> 
void Image<T>::Free()
{
  // Don't delete if pointer is null
  if (0 != m_Data)
  {
    // Delete row data
    for (unsigned int i = 0; i < m_nRows; ++i)
    {
      if (0 != m_Data[i])
      {
        delete[] m_Data[i];
        m_Data[i] = 0;
      }
    }
    // Delete row pointers
    delete[] m_Data;
    
    m_Data = 0;
  }
  
  m_nRows = 0;
  m_nCols = 0;
}

// Set every image pixel to a certain value
template <typename T>
void Image<T>::Set(T val)
{
  for (unsigned int i = 0; i < m_nRows; ++i)
  {
    for (unsigned int j = 0; j < m_nCols; ++j)
    {
      m_Data[i][j] = val;
    }
  }
}

// Reallocate memory for a certain size image
template <typename T>
void Image<T>::Allocate(unsigned int nr, unsigned int nc)
{
  // Make sure memory isn't already allocated; if it is, do nothing
  if (nr != m_nRows || nc != m_nCols)
  {
    // Free memory if there is any allocated
    if (0 != m_Data)
    {
      Free();
    }
    
    m_nRows = nr;
    m_nCols = nc;
    
    // Allocate memory for row pointers
    m_Data = new T*[m_nRows];
    
    // Allocate memory for row data
    for (unsigned int i = 0; i < m_nRows; ++i)
    {
      m_Data[i] = new T[m_nCols];
    }
  }
}

// Return sub-image of current image
template <typename T>
Image<T>* Image<T>::subImage(unsigned int rowStart,
                             unsigned int colStart,
                             unsigned int rowEnd,
                             unsigned int colEnd)
{
  // Check bounds
  assert(rowStart >= 0 && rowStart < m_nRows
      && rowEnd >= 0 && rowEnd < m_nRows
      && colStart >= 0 && colStart < m_nCols
      && colEnd >= 0 && colEnd < m_nCols);
  
  Image<T>* sub = new Image<T>(rowEnd-rowStart+1,colEnd-colStart+1);
  for (unsigned int i = rowStart; i <= rowEnd; ++i)
  {
    for (unsigned int j = colStart; j <= colEnd; ++j)
    {
      sub->data()[i-rowStart][j-colStart] = m_Data[i][j];
    }
  }
  
  return sub;
}

// Copy data from another image of the same type
template <typename T>
void Image<T>::CopyFrom(const Image<T>* imageIn)
{
  unsigned int nr = imageIn->nRows();
  unsigned int nc = imageIn->nCols();
  
  // Resize and reallocate memory if necessary
  Allocate(nr, nc);
  
  // Copy image data
  for (unsigned int i = 0; i < nr; ++i)
  {
    for (unsigned int j = 0; j < nc; ++j)
    {
      m_Data[i][j] = (imageIn->data())[i][j];
    }
  }
}

// Copy data from an image of smaller or equal size
// into this image, starting at given row/col index
template <typename T>
void Image<T>::CopyFrom(const Image<T>* imageIn,
                        unsigned int rowStart,
                        unsigned int colStart)
{
  unsigned int nr = imageIn->nRows();
  unsigned int nc = imageIn->nCols();
  unsigned int rowEnd = rowStart + nr - 1;
  unsigned int colEnd = colStart + nc - 1;
  
  // Check bounds
  assert(rowStart >= 0 && rowStart < m_nRows
      && rowEnd >= 0 && rowEnd < m_nRows
      && colStart >= 0 && colStart < m_nCols
      && colEnd >= 0 && colStart < m_nCols);
      
  for (unsigned int i = rowStart; i <= rowEnd; ++i)
  {
    for (unsigned int j = colStart; j <= colEnd; ++j)
    {
      m_Data[i][j] = imageIn->data()[i-rowStart][j-rowStart];
    }
  }
}

// Need to do this to avoid undefined reference erros for
// templated classes in GNU make... haven't found a better 
// way yet
template class Image<double>;
template class Image<unsigned char>;
template class Image<unsigned int>;
