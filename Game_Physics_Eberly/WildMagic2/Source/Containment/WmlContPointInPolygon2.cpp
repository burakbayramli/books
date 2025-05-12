// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlContPointInPolygon2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::PointInPolygon (int iQuantity, const Vector2<Real>* akV,
    const Vector2<Real>& rkP)
{
    bool bInside = false;
    for (int i = 0, j = iQuantity-1; i < iQuantity; j = i++)
    {
        const Vector2<Real>& rkU0 = akV[i];
        const Vector2<Real>& rkU1 = akV[j];
        Real fRHS, fLHS;

        if ( rkP.Y() < rkU1.Y() )  // U1 above ray
        {
            if ( rkU0.Y() <= rkP.Y() )  // U0 on or below ray
            {
                fLHS = (rkP.Y()-rkU0.Y())*(rkU1.X()-rkU0.X());
                fRHS = (rkP.X()-rkU0.X())*(rkU1.Y()-rkU0.Y());
                if ( fLHS > fRHS )
                    bInside = !bInside;
            }
        }
        else if ( rkP.Y() < rkU0.Y() )  // U1 on or below ray, U0 above ray
        {
            fLHS = (rkP.Y()-rkU0.Y())*(rkU1.X()-rkU0.X());
            fRHS = (rkP.X()-rkU0.X())*(rkU1.Y()-rkU0.Y());
            if ( fLHS < fRHS )
                bInside = !bInside;
        }
    }
    return bInside;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::PointInConvexOrderN (int iQuantity, const Vector2<Real>* akV,
    const Vector2<Real>& rkP)
{
    for (int i1 = 0, i0 = iQuantity-1; i1 < iQuantity; i0 = i1++)
    {
        Real fNx = akV[i1].Y() - akV[i0].Y();
        Real fNy = akV[i0].X() - akV[i1].X();
        Real fDx = rkP.X() - akV[i0].X();
        Real fDy = rkP.Y() - akV[i0].Y();
        if ( fNx*fDx + fNy*fDy > (Real)0.0 )
            return false;
    }

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
static bool SubContainsPoint (int iQuantity, const Vector2<Real>* akV,
    const Vector2<Real>& rkP, int i0, int i1)
{
    Real fNx, fNy, fDx, fDy;

    int iDiff = i1 - i0;
    if ( iDiff == 1 || (iDiff < 0 && iDiff+iQuantity == 1) )
    {
        fNx = akV[i1].Y() - akV[i0].Y();
        fNy = akV[i0].X() - akV[i1].X();
        fDx = rkP.X() - akV[i0].X();
        fDy = rkP.Y() - akV[i0].Y();
        return fNx*fDx + fNy*fDy <= (Real)0.0;
    }

    // bisect the index range
    int iMid;
    if ( i0 < i1 )
    {
        iMid = (i0 + i1) >> 1;
    }
    else
    {
        iMid = ((i0 + i1 + iQuantity) >> 1);
        if ( iMid >= iQuantity )
            iMid -= iQuantity;
    }

    // determine which side of the splitting line contains the point
    fNx = akV[iMid].Y() - akV[i0].Y();
    fNy = akV[i0].X() - akV[iMid].X();
    fDx = rkP.X() - akV[i0].X();
    fDy = rkP.Y() - akV[i0].Y();
    if ( fNx*fDx + fNy*fDy > (Real)0.0 )
    {
        // P potentially in <V(i0),V(i0+1),...,V(mid-1),V(mid)>
        return SubContainsPoint(iQuantity,akV,rkP,i0,iMid);
    }
    else
    {
        // P potentially in <V(mid),V(mid+1),...,V(i1-1),V(i1)>
        return SubContainsPoint(iQuantity,akV,rkP,iMid,i1);
    }
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::PointInConvexOrderLogN (int iVQuantity, const Vector2<Real>* akV,
    const Vector2<Real>& rkP)
{
    return SubContainsPoint(iVQuantity,akV,rkP,0,0);
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::PointInConvex4 (const Vector2<Real>* akV, const Vector2<Real>& rkP)
{
    Real fNx = akV[2].Y() - akV[0].Y();
    Real fNy = akV[0].X() - akV[2].X();
    Real fDx = rkP.X() - akV[0].X();
    Real fDy = rkP.Y() - akV[0].Y();

    if ( fNx*fDx + fNy*fDy > (Real)0.0 )
    {
        // P potentially in <V0,V1,V2>
        fNx = akV[1].Y() - akV[0].Y();
        fNy = akV[0].X() - akV[1].X();
        if ( fNx*fDx + fNy*fDy > (Real)0.0 )
            return false;

        fNx = akV[2].Y() - akV[1].Y();
        fNy = akV[1].X() - akV[2].X();
        fDx = rkP.X() - akV[1].X();
        fDy = rkP.Y() - akV[1].Y();
        if ( fNx*fDx + fNy*fDy > (Real)0.0 )
            return false;
    }
    else
    {
        // P potentially in <V0,V2,V3>
        fNx = akV[0].Y() - akV[3].Y();
        fNy = akV[3].X() - akV[0].X();
        if ( fNx*fDx + fNy*fDy > (Real)0.0 )
            return false;

        fNx = akV[3].Y() - akV[2].Y();
        fNy = akV[2].X() - akV[3].X();
        fDx = rkP.X() - akV[3].X();
        fDy = rkP.Y() - akV[3].Y();
        if ( fNx*fDx + fNy*fDy > (Real)0.0 )
            return false;
    }
    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool PointInPolygon<float> (int,
    const Vector2<float>*, const Vector2<float>&);
template WML_ITEM bool PointInConvexOrderN<float> (int,
    const Vector2<float>*, const Vector2<float>&);
template WML_ITEM bool PointInConvexOrderLogN<float> (int,
    const Vector2<float>*, const Vector2<float>&);
template WML_ITEM bool PointInConvex4<float> (const Vector2<float>*,
     const Vector2<float>&);

template WML_ITEM bool PointInPolygon<double> (int,
    const Vector2<double>*, const Vector2<double>&);
template WML_ITEM bool PointInConvexOrderN<double> (int,
    const Vector2<double>*, const Vector2<double>&);
template WML_ITEM bool PointInConvexOrderLogN<double> (int,
    const Vector2<double>*, const Vector2<double>&);
template WML_ITEM bool PointInConvex4<double> (const Vector2<double>*,
     const Vector2<double>&);
}
//----------------------------------------------------------------------------
