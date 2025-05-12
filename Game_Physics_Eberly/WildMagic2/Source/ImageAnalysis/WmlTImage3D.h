// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLTIMAGE3D_H
#define WMLTIMAGE3D_H

#include "WmlTImage.h"

namespace Wml
{

template <class T>
class TImage3D : public TImage<T>
{
public:
    // Construction and destruction.  TImage3D accepts responsibility for
    // deleting the input data array.
    TImage3D (int iXBound, int iYBound, int iZBound, T* atData = 0);
    TImage3D (const TImage3D& rkImage);
    TImage3D (const char* acFilename);

    // data access
    T& operator() (int iX, int iY, int iZ) const;

    // conversion between 3D coordinates and 1D indexing
    int GetIndex (int iX, int iY, int iZ) const;
    void GetCoordinates (int iIndex, int& riX, int& riY, int& riZ) const;

    // assignment
    TImage3D& operator= (const TImage3D& rkImage);
    TImage3D& operator= (T tValue);
};

#include "WmlTImage3D.inl"

}

#endif
