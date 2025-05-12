// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRTET3TET3_H
#define WMLINTRTET3TET3_H

#include "WmlTetrahedron3.h"
#include <vector>

namespace Wml
{

template <class Real>
WML_ITEM void FindIntersection (const Tetrahedron3<Real>& rkT0,
    const Tetrahedron3<Real>& rkT1, std::vector<Tetrahedron3<Real> >& rkIntr);

}

#endif
