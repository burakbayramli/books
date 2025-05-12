// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLFRUSTUM3_H
#define WMLFRUSTUM3_H

#include "WmlVector3.h"

namespace Wml
{

template <class Real>
class WML_ITEM Frustum3
{
public:
    // Orthogonal frustum.  Let E be the origin, L be the left vector, U be
    // the up vector, and D be the direction vector.  Let l > 0 and u > 0 be
    // the extents in the L and U directions, respectively.  Let n and f be
    // the extents in the D direction with 0 < n < f.  The four corners of the
    // frustum in the near plane are E + s0*l*L + s1*u*U + n*D where |s0| =
    // |s1| = 1 (four choices).  The four corners of the frustum in the far
    // plane are E + (f/n)*(s0*l*L + s1*u*U) where |s0| = |s1| = 1 (four
    // choices).

    Frustum3 ();

    Vector3<Real>& Origin ();
    const Vector3<Real>& Origin () const;

    Vector3<Real>& LVector ();
    const Vector3<Real>& LVector () const;
    Vector3<Real>& UVector ();
    const Vector3<Real>& UVector () const;
    Vector3<Real>& DVector ();
    const Vector3<Real>& DVector () const;

    Real& LBound ();
    const Real& LBound () const;
    Real& UBound ();
    const Real& UBound () const;
    Real& DMin ();
    const Real& DMin () const;
    Real& DMax ();
    const Real& DMax () const;

    void Update ();
    Real GetDRatio () const;
    Real GetMTwoLF () const;
    Real GetMTwoUF () const;

    void ComputeVertices (Vector3<Real> akVertex[8]) const;

protected:
    Vector3<Real> m_kOrigin, m_kLVector, m_kUVector, m_kDVector;
    Real m_fLBound, m_fUBound, m_fDMin, m_fDMax;

    // derived quantities
    Real m_fDRatio;
    Real m_fMTwoLF, m_fMTwoUF;
};

typedef Frustum3<float> Frustum3f;
typedef Frustum3<double> Frustum3d;

}

#endif
