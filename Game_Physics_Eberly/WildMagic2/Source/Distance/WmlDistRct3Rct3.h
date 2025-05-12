// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDISTRCT3RCT3_H
#define WMLDISTRCT3RCT3_H

#include "WmlRectangle3.h"

namespace Wml
{

// squared distance measurements
template <class Real>
WML_ITEM Real SqrDistance (const Rectangle3<Real>& rkRct0,
    const Rectangle3<Real>& rkRct1, Real* pfRct0P0 = NULL,
    Real* pfRct0P1 = NULL, Real* pfRct1P0 = NULL, Real* pfRct1P1 = NULL);

// distance measurements
template <class Real>
WML_ITEM Real Distance (const Rectangle3<Real>& rkRct0,
    const Rectangle3<Real>& rkRct1, Real* pfRct0P0 = NULL,
    Real* pfRct0P1 = NULL, Real* pfRct1P0 = NULL, Real* pfRct1P1 = NULL);

}

#endif
