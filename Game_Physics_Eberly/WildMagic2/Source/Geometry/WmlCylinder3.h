// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCYLINDER3_H
#define WMLCYLINDER3_H

#include "WmlSegment3.h"

namespace Wml
{

template <class Real>
class WML_ITEM Cylinder3
{
public:
    // Cylinder line segment has end points C-(H/2)*D and C+(H/2)*D where
    // D is a unit-length vector.  H is infinity for infinite cylinder.

    Cylinder3 ();

    Vector3<Real>& Center ();
    const Vector3<Real>& Center () const;

    Vector3<Real>& Direction ();
    const Vector3<Real>& Direction () const;

    Real& Height ();
    Real Height () const;

    Real& Radius ();
    Real Radius () const;

    // A value of 'true' means the cylinder caps (the end disks) are included
    // as part of the cylinder.  A value of 'false' means treat the cylinder
    // as hollow--the end disks are not part of the object.
    bool& Capped ();
    bool Capped () const;

    Segment3<Real> GetSegment () const;

    // Call this function to generate a coordinate system for the cylinder,
    // {U,V,W}, an orthonormal set where W is the unit-length direction of
    // the cylinder axis.  This is necessary for cylinder-cylinder
    // intersection testing to avoid creating U and V for every test.
    void GenerateCoordinateSystem ();
    Vector3<Real>& U ();
    const Vector3<Real>& U () const;
    Vector3<Real>& V ();
    const Vector3<Real>& V () const;
    Vector3<Real>& W ();
    const Vector3<Real>& W () const;

protected:
    Vector3<Real> m_kCenter;
    Vector3<Real> m_kDirection;  // W
    Vector3<Real> m_kU, m_kV;    // U, V
    Real m_fHeight;
    Real m_fRadius;
    bool m_bCapped;
};

typedef Cylinder3<float> Cylinder3f;
typedef Cylinder3<double> Cylinder3d;

}

#endif
