// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLAPPRELLIPSEBYARCS2_H
#define WMLAPPRELLIPSEBYARCS2_H

#include "WmlVector2.h"

namespace Wml
{

// The ellipse is (x/a)^2 + (y/b)^2 = 1, but only the portion in the first
// quadrant (x >= 0 and y >= 0) is approximated.  Generate iNumArcs >= 2 arcs
// by constructing points corresponding to the weighted averages of the
// curvatures at the ellipse points (a,0) and (0,b).  The returned input point
// array has iNumArcs+1 elements and the returned input center and radius
// arrays each have iNumArc elements.  The arc associated with akPoint[i] and
// akPoint[i+1] has center akCenter[i] and radius afRadius[i].

template <class Real>
WML_ITEM void ApproximateEllipseByArcs (Real fA, Real fB, int iNumArcs,
    Vector2<Real>*& rakPoint, Vector2<Real>*& rakCenter, Real*& rafRadius);

}

#endif
