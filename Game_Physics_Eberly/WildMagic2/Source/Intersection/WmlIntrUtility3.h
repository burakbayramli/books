// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRUTILITY3_H
#define WMLINTRUTILITY3_H

#include "WmlVector3.h"

namespace Wml
{

enum WML_ITEM VertexProjectionMap
{
    m2, m11,                // lines
    m3, m21, m12, m111,     // triangles
    m44, m2_2, m1_1         // boxes
};

enum WML_ITEM ContactSide
{
    LEFT,
    RIGHT,
    NONE
};

template <class Real>
class WML_ITEM ContactConfig
{
public:
    // The number/configuration of vertices in Index
    VertexProjectionMap m_kMap;

    // Order of vertices
    int m_aiIndex[8];

    // Interval
    Real m_fMin, m_fMax;
};

template <class Real>
WML_ITEM bool AxisTest (const Vector3<Real>& rkVelocity,
    const Vector3<Real>& rkTestAxis, Real fUMin, Real fUMax, Real fVMin,
    Real fVMax, Real& rfTFirst, Real& rfTLast, Real fTMax);

template <class Real>
WML_ITEM bool AxisFind (const Vector3<Real>& rkVelocity, 
    const Vector3<Real>& rkTestAxis, const ContactConfig<Real>& rrkUC, 
    const ContactConfig<Real>& rrkVC, ContactSide& rkSide,
    ContactConfig<Real>& rkTUC, ContactConfig<Real>& rkTVC, Real& rfTFirst,
    Real& rfTLast, Real fTMax);

// The input and output polygons are stored in akP.  The size of akP is
// assumed to be large enough to store the clipped convex polygon vertices.
// For now the maximum array size is 8 to support the current intersection
// algorithms.
template <class Real>
WML_ITEM void ClipConvexPolygonAgainstPlane (const Vector3<Real>& rkNormal,
    Real fConstant, int& riQuantity, Vector3<Real>* akP);

}

#endif
