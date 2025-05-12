// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLTIMAGE_H
#define WMLTIMAGE_H

#include "WmlImageConvert.h"
#include "WmlLattice.h"
#include <fstream>

namespace Wml
{

// The class T is intended to be a wrapper of native data (int, float, char,
// etc.).  The code uses memcpy, memcmp, and memset on the array of T values.
// Class T must have the following member functions:
//   T::T ()
//   T& T::operator= (T)
//   static const char* GetRTTI ()
// The static member function returns a string that is used for streaming.


template <class T>
class TImage : public Lattice
{
public:
    // Construction and destruction.  TImage accepts responsibility for
    // deleting the input arrays.
    TImage (int iDimensions, int* aiBound, T* atData = NULL);
    TImage (const TImage& rkImage);
    TImage (const char* acFilename);
    virtual ~TImage ();

    // data access
    T* GetData () const;
    T& operator[] (int i) const;

    // assignment
    TImage& operator= (const TImage& rkImage);
    TImage& operator= (T tValue);

    // comparison
    bool operator== (const TImage& rkImage) const;
    bool operator!= (const TImage& rkImage) const;

    // streaming
    bool Load (const char* acFilename);
    bool Save (const char* acFilename) const;

protected:
    // for deferred creation of bounds
    TImage (int iDimensions);
    void SetData (T* atData);

    T* m_atData;
};

#include "WmlTImage.inl"

}

#endif
