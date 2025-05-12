// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTPLINEARNONUNIFORM2_H
#define WMLINTPLINEARNONUNIFORM2_H

// Linear interpolation of a network of triangles whose vertices are of the
// form (x,y,f(x,y)).

#include "WmlDelaunay2.h"

namespace Wml
{

template <class Real>
class WML_ITEM IntpLinearNonuniform2 : public Delaunay2<Real>
{
public:
    // Construction and destruction.
    //
    // The first constructor implicitly creates the triangle network from the
    // input vertices.  This constructor accepts ownership of the input arrays
    // and will delete them during destruction.  The underlying triangle
    // network object also will be deleted.
    //
    // The second constructor shares the input triangle network.  This
    // constructor accepts ownership of the input function array, but does
    // not delete the triangle network on destruction.  The idea is that the
    // network was shared, either from an explicitly created one by the
    // application or from one created by another interpolator.

    IntpLinearNonuniform2 (int iVertexQuantity, Vector2<Real>* akVertex,
        Real* afF);
    IntpLinearNonuniform2 (Delaunay2<Real>& rkNet, Real* afF);
    virtual ~IntpLinearNonuniform2 ();

    // Linear interpolation.  The return value is 'true' if and only if the
    // input point is in the convex hull of the input vertices, in which case
    // the interpolation is valid.
    bool Evaluate (const Vector2<Real>& rkPoint, Real& rfF);

protected:
    Real* m_afF;
};

typedef IntpLinearNonuniform2<float> IntpLinearNonuniform2f;
typedef IntpLinearNonuniform2<double> IntpLinearNonuniform2d;

}

#endif
