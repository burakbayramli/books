// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLTIMAGE2D_H
#define WMLTIMAGE2D_H

#include "WmlTImage.h"

namespace Wml
{

template <class T>
class TImage2D : public TImage<T>
{
public:
    // Construction and destruction.  TImage2D accepts responsibility for
    // deleting the input data array.
    TImage2D (int iXBound, int iYBound, T* atData = 0);
    TImage2D (const TImage2D& rkImage);
    TImage2D (const char* acFilename);

    // data access
    T& operator() (int iX, int iY) const;

    // conversion between 2D coordinates and 1D indexing
    int GetIndex (int iX, int iY) const;
    void GetCoordinates (int iIndex, int& riX, int& riY) const;

    // assignment
    TImage2D& operator= (const TImage2D& rkImage);
    TImage2D& operator= (T tValue);
};

#include "WmlTImage2D.inl"

}

#endif
