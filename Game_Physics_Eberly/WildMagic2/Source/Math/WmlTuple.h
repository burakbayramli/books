// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLTUPLE_H
#define WMLTUPLE_H

#include "WmlSystem.h"

namespace Wml
{

template <int N>
class Tuple
{
public:
    // construction
    Tuple ();
    Tuple (const Tuple& rkT);

    // coordinate access
    operator const int* () const;
    operator int* ();
    int operator[] (int i) const;
    int& operator[] (int i);

    // assignment
    Tuple& operator= (const Tuple& rkT);

private:
    int m_aiTuple[N];
};

#include "WmlTuple.inl"

}

#endif
