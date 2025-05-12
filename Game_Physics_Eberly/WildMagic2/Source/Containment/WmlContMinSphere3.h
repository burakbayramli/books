// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONTMINSPHERE3_H
#define WMLCONTMINSPHERE3_H

// Compute the minimum volume sphere containing the input set of points.  The
// algorithm randomly permutes the input points so that the construction
// occurs in 'expected' O(N) time.

#include "WmlSphere3.h"

namespace Wml
{

template <class Real>
class WML_ITEM MinSphere3
{
public:
    MinSphere3 (int iQuantity, const Vector3<Real>* akPoint,
        Sphere3<Real>& rkMinimal);

    // Floating point tolerance used for various comparisons.  The default
    // value is 1e-03.
    static const Real EPSILON;

private:
    // indices of points that support current minimum volume sphere
    class WML_ITEM Support
    {
    public:
        bool Contains (int iIndex, Vector3<Real>** apkPoint)
        {
            for (int i = 0; i < Quantity; i++)
            {
                Vector3<Real> kDiff = *apkPoint[iIndex] - *apkPoint[Index[i]];
                if ( kDiff.SquaredLength() < MinSphere3::EPSILON )
                    return true;
            }
            return false;
        }

        int Quantity;
        int Index[4];
    };

    // test if point P is inside sphere S
    static bool Contains (const Vector3<Real>& rkP,
        const Sphere3<Real>& rkS, Real& rfDistDiff);

    static Sphere3<Real> ExactSphere1 (const Vector3<Real>& rkP);
    static Sphere3<Real> ExactSphere2 (const Vector3<Real>& rkP0,
        const Vector3<Real>& rkP1);
    static Sphere3<Real> ExactSphere3 (const Vector3<Real>& rkP0,
        const Vector3<Real>& rkP1, const Vector3<Real>& rkP2);
    static Sphere3<Real> ExactSphere4 (const Vector3<Real>& rkP0,
        const Vector3<Real>& rkP1, const Vector3<Real>& rkP2,
        const Vector3<Real>& rkP3);

    static Sphere3<Real> UpdateSupport1 (int i, Vector3<Real>** apkPerm,
        Support& rkSupp);
    static Sphere3<Real> UpdateSupport2 (int i, Vector3<Real>** apkPerm,
        Support& rkSupp);
    static Sphere3<Real> UpdateSupport3 (int i, Vector3<Real>** apkPerm,
        Support& rkSupp);
    static Sphere3<Real> UpdateSupport4 (int i, Vector3<Real>** apkPerm,
        Support& rkSupp);

    typedef Sphere3<Real> (*UpdateFunction)(int,Vector3<Real>**,Support&);

    static UpdateFunction ms_aoUpdate[5];
    static const Real ONE_PLUS_EPSILON;
};

typedef MinSphere3<float> MinSphere3f;
typedef MinSphere3<double> MinSphere3d;

}

#endif
