// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONTMINCIRCLE2_H
#define WMLCONTMINCIRCLE2_H

// Compute the minimum area circle containing the input set of points.  The
// algorithm randomly permutes the input points so that the construction
// occurs in 'expected' O(N) time.

#include "WmlCircle2.h"

namespace Wml
{

template <class Real>
class WML_ITEM MinCircle2
{
public:
    MinCircle2 (int iQuantity, const Vector2<Real>* akPoint,
        Circle2<Real>& rkMinimal);

    // Floating point tolerance used for various comparisons.  The default
    // value is 1e-05.
    static const Real EPSILON;

private:
    // indices of points that support current minimum area circle
    class WML_ITEM Support
    {
    public:
        bool Contains (int iIndex, Vector2<Real>** apkPoint)
        {
            for (int i = 0; i < Quantity; i++)
            {
                Vector2<Real> kDiff = *apkPoint[iIndex] - *apkPoint[Index[i]];
                if ( kDiff.SquaredLength() < MinCircle2::EPSILON )
                    return true;
            }
            return false;
        }

        int Quantity;
        int Index[3];
    };

    // test if point P is inside circle C
    static bool Contains (const Vector2<Real>& rkP,
        const Circle2<Real>& rkC, Real& rfDistDiff);

    static Circle2<Real> ExactCircle1 (const Vector2<Real>& rkP);
    static Circle2<Real> ExactCircle2 (const Vector2<Real>& rkP0,
        const Vector2<Real>& rkP1);
    static Circle2<Real> ExactCircle3 (const Vector2<Real>& rkP0,
        const Vector2<Real>& rkP1, const Vector2<Real>& rkP2);

    static Circle2<Real> UpdateSupport1 (int i, Vector2<Real>** apkPerm,
        Support& rkSupp);
    static Circle2<Real> UpdateSupport2 (int i, Vector2<Real>** apkPerm,
        Support& rkSupp);
    static Circle2<Real> UpdateSupport3 (int i, Vector2<Real>** apkPerm,
        Support& rkSupp);

    typedef Circle2<Real> (*UpdateFunction)(int,Vector2<Real>**,Support&);

    static UpdateFunction ms_aoUpdate[4];
};

typedef MinCircle2<float> MinCircle2f;
typedef MinCircle2<double> MinCircle2d;

}

#endif
