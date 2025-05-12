// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrBox3Fru3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Box3<Real>& rkBox,
    const Frustum3<Real>& rkFrustum)
{
    // for convenience
    const Vector3<Real>* akA = rkBox.Axes();
    const Real* afE = rkBox.Extents();

    Vector3<Real> kDiff = rkBox.Center() - rkFrustum.Origin();

    Real afA[3], afB[3], afC[3], afD[3];
    Real afNA[3], afNB[3], afNC[3], afND[3];
    Real afNApLC[3], afNAmLC[3], afNBpUC[3], afNBmUC[3];
    Real afLC[3], afLD[3], afUC[3], afUD[3], afLBpUA[3], afLBmUA[3];
    Real fDdD, fR, fP, fMin, fMax, fMTwoLF, fMTwoUF, fLB, fUA, fTmp;
    int i, j;

    // M = D
    afD[2] = kDiff.Dot(rkFrustum.DVector());
    for (i = 0; i < 3; i++)
        afC[i] = akA[i].Dot(rkFrustum.DVector());
    fR = afE[0]*Math<Real>::FAbs(afC[0]) +
         afE[1]*Math<Real>::FAbs(afC[1]) +
         afE[2]*Math<Real>::FAbs(afC[2]);
    if ( afD[2] + fR < rkFrustum.DMin() || afD[2] - fR > rkFrustum.DMax() )
        return false;

    // M = n*L - l*D
    for (i = 0; i < 3; i++)
    {
        afA[i] = akA[i].Dot(rkFrustum.LVector());
        afLC[i] = rkFrustum.LBound()*afC[i];
        afNA[i] = rkFrustum.DMin()*afA[i];
        afNAmLC[i] = afNA[i] - afLC[i];
    }
    afD[0] = kDiff.Dot(rkFrustum.LVector());
    fR = afE[0]*Math<Real>::FAbs(afNAmLC[0]) +
         afE[1]*Math<Real>::FAbs(afNAmLC[1]) +
         afE[2]*Math<Real>::FAbs(afNAmLC[2]);
    afND[0] = rkFrustum.DMin()*afD[0];
    afLD[2] = rkFrustum.LBound()*afD[2];
    fDdD = afND[0] - afLD[2];
    fMTwoLF = rkFrustum.GetMTwoLF();
    if ( fDdD + fR < fMTwoLF || fDdD > fR )
        return false;

    // M = -n*L - l*D
    for (i = 0; i < 3; i++)
        afNApLC[i] = afNA[i] + afLC[i];
    fR = afE[0]*Math<Real>::FAbs(afNApLC[0]) +
         afE[1]*Math<Real>::FAbs(afNApLC[1]) +
         afE[2]*Math<Real>::FAbs(afNApLC[2]);
    fDdD = -(afND[0] + afLD[2]);
    if ( fDdD + fR < fMTwoLF || fDdD > fR )
        return false;

    // M = n*U - u*D
    for (i = 0; i < 3; i++)
    {
        afB[i] = akA[i].Dot(rkFrustum.UVector());
        afUC[i] = rkFrustum.UBound()*afC[i];
        afNB[i] = rkFrustum.DMin()*afB[i];
        afNBmUC[i] = afNB[i] - afUC[i];
    }
    afD[1] = kDiff.Dot(rkFrustum.UVector());
    fR = afE[0]*Math<Real>::FAbs(afNBmUC[0]) +
         afE[1]*Math<Real>::FAbs(afNBmUC[1]) +
         afE[2]*Math<Real>::FAbs(afNBmUC[2]);
    afND[1] = rkFrustum.DMin()*afD[1];
    afUD[2] = rkFrustum.UBound()*afD[2];
    fDdD = afND[1] - afUD[2];
    fMTwoUF = rkFrustum.GetMTwoUF();
    if ( fDdD + fR < fMTwoUF || fDdD > fR )
        return false;

    // M = -n*U - u*D
    for (i = 0; i < 3; i++)
        afNBpUC[i] = afNB[i] + afUC[i];
    fR = afE[0]*Math<Real>::FAbs(afNBpUC[0]) +
         afE[1]*Math<Real>::FAbs(afNBpUC[1]) +
         afE[2]*Math<Real>::FAbs(afNBpUC[2]);
    fDdD = -(afND[1] + afUD[2]);
    if ( fDdD + fR < fMTwoUF || fDdD > fR )
        return false;

    // M = A[i]
    for (i = 0; i < 3; i++)
    {
        fP = rkFrustum.LBound()*Math<Real>::FAbs(afA[i]) +
             rkFrustum.UBound()*Math<Real>::FAbs(afB[i]);
        afNC[i] = rkFrustum.DMin()*afC[i];
        fMin = afNC[i] - fP;
        if ( fMin < 0.0f )
            fMin *= rkFrustum.GetDRatio();
        fMax = afNC[i] + fP;
        if ( fMax > 0.0f )
            fMax *= rkFrustum.GetDRatio();
        fDdD = afA[i]*afD[0] + afB[i]*afD[1] + afC[i]*afD[2];
        if ( fDdD + afE[i] < fMin || fDdD - afE[i] > fMax )
            return false;
    }

    // M = Cross(L,A[i])
    for (i = 0; i < 3; i++)
    {
        fP = rkFrustum.UBound()*Math<Real>::FAbs(afC[i]);
        fMin = afNB[i] - fP;
        if ( fMin < 0.0f )
            fMin *= rkFrustum.GetDRatio();
        fMax = afNB[i] + fP;
        if ( fMax > 0.0f )
            fMax *= rkFrustum.GetDRatio();
        fDdD = -afC[i]*afD[1] + afB[i]*afD[2];
        fR = afE[0]*Math<Real>::FAbs(afB[i]*afC[0]-afB[0]*afC[i]) +
             afE[1]*Math<Real>::FAbs(afB[i]*afC[1]-afB[1]*afC[i]) +
             afE[2]*Math<Real>::FAbs(afB[i]*afC[2]-afB[2]*afC[i]);
        if ( fDdD + fR < fMin || fDdD - fR > fMax )
            return false;
    }

    // M = Cross(U,A[i])
    for (i = 0; i < 3; i++)
    {
        fP = rkFrustum.LBound()*Math<Real>::FAbs(afC[i]);
        fMin = -afNA[i] - fP;
        if ( fMin < 0.0f )
            fMin *= rkFrustum.GetDRatio();
        fMax = -afNA[i] + fP;
        if ( fMax > 0.0f )
            fMax *= rkFrustum.GetDRatio();
        fDdD = afC[i]*afD[0] - afA[i]*afD[2];
        fR = afE[0]*Math<Real>::FAbs(afA[i]*afC[0]-afA[0]*afC[i]) +
             afE[1]*Math<Real>::FAbs(afA[i]*afC[1]-afA[1]*afC[i]) +
             afE[2]*Math<Real>::FAbs(afA[i]*afC[2]-afA[2]*afC[i]);
        if ( fDdD + fR < fMin || fDdD - fR > fMax )
            return false;
    }

    // M = Cross(n*D+l*L+u*U,A[i])
    for (i = 0; i < 3; i++)
    {
        fLB = rkFrustum.LBound()*afB[i];
        fUA = rkFrustum.UBound()*afA[i];
        afLBpUA[i] = fLB + fUA;
        afLBmUA[i] = fLB - fUA;
    }
    for (i = 0; i < 3; i++)
    {
        fP = rkFrustum.LBound()*Math<Real>::FAbs(afNBmUC[i]) +
             rkFrustum.UBound()*Math<Real>::FAbs(afNAmLC[i]);
        fTmp = rkFrustum.DMin()*afLBmUA[i];
        fMin = fTmp - fP;
        if ( fMin < 0.0f )
            fMin *= rkFrustum.GetDRatio();
        fMax = fTmp + fP;
        if ( fMax > 0.0f )
            fMax *= rkFrustum.GetDRatio();
        fDdD = -afD[0]*afNBmUC[i] + afD[1]*afNAmLC[i] + afD[2]*afLBmUA[i];
        fR = 0.0f;
        for (j = 0; j < 3; j++)
        {
            fR += afE[j]*Math<Real>::FAbs(-afA[j]*afNBmUC[i]+ afB[j]*afNAmLC[i]
                + afC[j]*afLBmUA[i]);
        }
        if ( fDdD + fR < fMin || fDdD - fR > fMax )
            return false;
    }

    // M = Cross(n*D+l*L-u*U,A[i])
    for (i = 0; i < 3; i++)
    {
        fP = rkFrustum.LBound()*Math<Real>::FAbs(afNBpUC[i]) +
             rkFrustum.UBound()*Math<Real>::FAbs(afNAmLC[i]);
        fTmp = rkFrustum.DMin()*afLBpUA[i];
        fMin = fTmp - fP;
        if ( fMin < 0.0f )
            fMin *= rkFrustum.GetDRatio();
        fMax = fTmp + fP;
        if ( fMax > 0.0f )
            fMax *= rkFrustum.GetDRatio();
        fDdD = -afD[0]*afNBpUC[i] + afD[1]*afNAmLC[i] + afD[2]*afLBpUA[i];
        fR = 0.0f;
        for (j = 0; j < 3; j++)
        {
            fR += afE[j]*Math<Real>::FAbs(-afA[j]*afNBpUC[i]+ afB[j]*afNAmLC[i]
                + afC[j]*afLBpUA[i]);
        }
        if ( fDdD + fR < fMin || fDdD - fR > fMax )
            return false;
    }

    // M = Cross(n*D-l*L+u*U,A[i])
    for (i = 0; i < 3; i++)
    {
        fP = rkFrustum.LBound()*Math<Real>::FAbs(afNBmUC[i]) +
             rkFrustum.UBound()*Math<Real>::FAbs(afNApLC[i]);
        fTmp = -rkFrustum.DMin()*afLBpUA[i];
        fMin = fTmp - fP;
        if ( fMin < 0.0f )
            fMin *= rkFrustum.GetDRatio();
        fMax = fTmp + fP;
        if ( fMax > 0.0f )
            fMax *= rkFrustum.GetDRatio();
        fDdD = -afD[0]*afNBmUC[i] + afD[1]*afNApLC[i] - afD[2]*afLBpUA[i];
        fR = 0.0f;
        for (j = 0; j < 3; j++)
        {
            fR += afE[j]*Math<Real>::FAbs(-afA[j]*afNBmUC[i]+ afB[j]*afNApLC[i]
                - afC[j]*afLBpUA[i]);
        }
        if ( fDdD + fR < fMin || fDdD - fR > fMax )
            return false;
    }

    // M = Cross(n*D-l*L-u*U,A[i])
    for (i = 0; i < 3; i++)
    {
        fP = rkFrustum.LBound()*Math<Real>::FAbs(afNBpUC[i]) +
             rkFrustum.UBound()*Math<Real>::FAbs(afNApLC[i]);
        fTmp = -rkFrustum.DMin()*afLBmUA[i];
        fMin = fTmp - fP;
        if ( fMin < 0.0f )
            fMin *= rkFrustum.GetDRatio();
        fMax = fTmp + fP;
        if ( fMax > 0.0f )
            fMax *= rkFrustum.GetDRatio();
        fDdD = -afD[0]*afNBpUC[i] + afD[1]*afNApLC[i] - afD[2]*afLBmUA[i];
        fR = 0.0f;
        for (j = 0; j < 3; j++)
        {
            fR += afE[j]*Math<Real>::FAbs(-afA[j]*afNBpUC[i]+ afB[j]*afNApLC[i]
                - afC[j]*afLBmUA[i]);
        }
        if ( fDdD + fR < fMin || fDdD - fR > fMax )
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
template WML_ITEM bool TestIntersection<float> (const Box3<float>&,
    const Frustum3<float>&);

template WML_ITEM bool TestIntersection<double> (const Box3<double>&,
    const Frustum3<double>&);
}
//----------------------------------------------------------------------------
