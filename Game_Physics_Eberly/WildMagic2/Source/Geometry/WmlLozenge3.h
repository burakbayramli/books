// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLLOZENGE3_H
#define WMLLOZENGE3_H

#include "WmlRectangle3.h"

namespace Wml
{

template <class Real>
class WML_ITEM Lozenge3
{
public:
    Lozenge3 ();

    Vector3<Real>& Origin ();
    const Vector3<Real>& Origin () const;

    Vector3<Real>& Edge0 ();
    const Vector3<Real>& Edge0 () const;

    Vector3<Real>& Edge1 ();
    const Vector3<Real>& Edge1 () const;

    Real& Radius ();
    const Real& Radius () const;

    Rectangle3<Real>& Rectangle ();
    const Rectangle3<Real>& Rectangle () const;

protected:
    Rectangle3<Real> m_kRectangle;
    Real m_fRadius;
};

typedef Lozenge3<float> Lozenge3f;
typedef Lozenge3<double> Lozenge3d;

}

#endif
