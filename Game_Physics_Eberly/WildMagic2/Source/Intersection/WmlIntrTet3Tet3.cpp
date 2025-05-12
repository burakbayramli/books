// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrTet3Tet3.h"
using namespace Wml;
using namespace std;

//----------------------------------------------------------------------------
template <class Real>
static void SplitAndDecompose (Tetrahedron3<Real> kTetra,
    const Plane3<Real>& rkPlane, vector<Tetrahedron3<Real> >& rkInside)
{
    // determine on which side of the plane the points of the tetrahedron lie
    Real afC[4];
    int i, aiP[4], aiN[4], aiZ[4];
    int iPositive = 0, iNegative = 0, iZero = 0;

    for (i = 0; i < 4; i++)
    {
        afC[i] = rkPlane.DistanceTo(kTetra[i]);
        if ( afC[i] > 0.0f )
            aiP[iPositive++] = i;
        else if ( afC[i] < 0.0f )
            aiN[iNegative++] = i;
        else
            aiZ[iZero++] = i;
    }

    // For a split to occur, one of the c_i must be positive and one must
    // be negative.

    if ( iNegative == 0 )
    {
        // tetrahedron is completely on the positive side of plane, full clip
        return;
    }

    if ( iPositive == 0 )
    {
        // tetrahedron is completely on the negative side of plane
        rkInside.push_back(kTetra);
        return;
    }

    // Tetrahedron is split by plane.  Determine how it is split and how to
    // decompose the negative-side portion into tetrahedra (6 cases).
    Real fW0, fW1, fInvCDiff;
    Vector3<Real> akIntp[4];

    if ( iPositive == 3 )
    {
        // +++-
        for (i = 0; i < iPositive; i++)
        {
            fInvCDiff = ((Real)1.0)/(afC[aiP[i]] - afC[aiN[0]]);
            fW0 = -afC[aiN[0]]*fInvCDiff;
            fW1 = +afC[aiP[i]]*fInvCDiff;
            kTetra[aiP[i]] = fW0*kTetra[aiP[i]] + fW1*kTetra[aiN[0]];
        }
        rkInside.push_back(kTetra);
    }
    else if ( iPositive == 2 )
    {
        if ( iNegative == 2 )
        {
            // ++--
            for (i = 0; i < iPositive; i++)
            {
                fInvCDiff = ((Real)1.0)/(afC[aiP[i]]-afC[aiN[0]]);
                fW0 = -afC[aiN[0]]*fInvCDiff;
                fW1 = +afC[aiP[i]]*fInvCDiff;
                akIntp[i] = fW0*kTetra[aiP[i]] + fW1*kTetra[aiN[0]];
            }
            for (i = 0; i < iNegative; i++)
            {
                fInvCDiff = ((Real)1.0)/(afC[aiP[i]]-afC[aiN[1]]);
                fW0 = -afC[aiN[1]]*fInvCDiff;
                fW1 = +afC[aiP[i]]*fInvCDiff;
                akIntp[i+2] = fW0*kTetra[aiP[i]] + fW1*kTetra[aiN[1]];
            }

            kTetra[aiP[0]] = akIntp[2];
            kTetra[aiP[1]] = akIntp[1];
            rkInside.push_back(kTetra);

            rkInside.push_back(Tetrahedron3<Real>(kTetra[aiN[1]],akIntp[3],
                akIntp[2],akIntp[1]));

            rkInside.push_back(Tetrahedron3<Real>(kTetra[aiN[0]],akIntp[0],
                akIntp[1],akIntp[2]));
        }
        else
        {
            // ++-0
            for (i = 0; i < iPositive; i++)
            {
                fInvCDiff = ((Real)1.0)/(afC[aiP[i]]-afC[aiN[0]]);
                fW0 = -afC[aiN[0]]*fInvCDiff;
                fW1 = +afC[aiP[i]]*fInvCDiff;
                kTetra[aiP[i]] = fW0*kTetra[aiP[i]] + fW1*kTetra[aiN[0]];
            }
            rkInside.push_back(kTetra);
        }
    }
    else if ( iPositive == 1 )
    {
        if ( iNegative == 3 )
        {
            // +---
            for (i = 0; i < iNegative; i++)
            {
                fInvCDiff = ((Real)1.0)/(afC[aiP[0]]-afC[aiN[i]]);
                fW0 = -afC[aiN[i]]*fInvCDiff;
                fW1 = +afC[aiP[0]]*fInvCDiff;
                akIntp[i] = fW0*kTetra[aiP[0]] + fW1*kTetra[aiN[i]];
            }

            kTetra[aiP[0]] = akIntp[0];
            rkInside.push_back(kTetra);

            rkInside.push_back(Tetrahedron3<Real>(akIntp[0],kTetra[aiN[1]],
                kTetra[aiN[2]],akIntp[1]));

            rkInside.push_back(Tetrahedron3<Real>(kTetra[aiN[2]],akIntp[1],
                akIntp[2],akIntp[0]));
        }
        else if ( iNegative == 2 )
        {
            // +--0
            for (i = 0; i < iNegative; i++)
            {
                fInvCDiff = ((Real)1.0)/(afC[aiP[0]]-afC[aiN[i]]);
                fW0 = -afC[aiN[i]]*fInvCDiff;
                fW1 = +afC[aiP[0]]*fInvCDiff;
                akIntp[i] = fW0*kTetra[aiP[0]] + fW1*kTetra[aiN[i]];
            }

            kTetra[aiP[0]] = akIntp[0];
            rkInside.push_back(kTetra);

            rkInside.push_back(Tetrahedron3<Real>(akIntp[1],kTetra[aiZ[0]],
                kTetra[aiN[1]],akIntp[0]));
        }
        else
        {
            // +-00
            fInvCDiff = ((Real)1.0)/(afC[aiP[0]]-afC[aiN[0]]);
            fW0 = -afC[aiN[0]]*fInvCDiff;
            fW1 = +afC[aiP[0]]*fInvCDiff;
            kTetra[aiP[0]] = fW0*kTetra[aiP[0]] + fW1*kTetra[aiN[0]];
            rkInside.push_back(kTetra);
        }
    }
}
//----------------------------------------------------------------------------
template <class Real>
void Wml::FindIntersection (const Tetrahedron3<Real>& rkT0,
    const Tetrahedron3<Real>& rkT1,  vector<Tetrahedron3<Real> >& rkIntr)
{
    // build planar faces of T0
    Plane3<Real> akPlane[4];
    rkT0.GetPlanes(akPlane);

    // initial object to clip is T1
    rkIntr.clear();
    rkIntr.push_back(rkT1);

    // clip T1 against planes of T0
    for (int iP = 0; iP < 4; iP++)
    {
        vector<Tetrahedron3<Real> > kInside;
        for (int iT = 0; iT < (int)rkIntr.size(); iT++)
            SplitAndDecompose(rkIntr[iT],akPlane[iP],kInside);
        rkIntr = kInside;
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM void FindIntersection<float> (
    const Tetrahedron3<float>&, const Tetrahedron3<float>&,
    vector<Tetrahedron3<float> >&);

template WML_ITEM void FindIntersection<double> (
    const Tetrahedron3<double>&, const Tetrahedron3<double>&,
    vector<Tetrahedron3<double> >&);
}
//----------------------------------------------------------------------------
