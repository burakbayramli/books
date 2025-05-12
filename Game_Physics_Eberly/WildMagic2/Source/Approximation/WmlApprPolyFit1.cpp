// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlApprPolyFit1.h"
#include "WmlLinearSystem.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
void Wml::PolyFit1 (int iSamples, const Real* afX, const Real* afW,
    int iXDegree, int& riQuantity, Real*& rafC)
{
    int i0, i1, iS;

    riQuantity = iXDegree + 1;
    rafC = new Real[riQuantity];

    // powers of x
    Real** aafXP;
    Allocate2D(2*riQuantity,iSamples,aafXP);
    for (iS = 0; iS < iSamples; iS++)
    {
        aafXP[iS][0] = (Real)1.0;
        for (i0 = 1; i0 <= 2*iXDegree; i0++)
            aafXP[iS][i0] = afX[iS]*aafXP[iS][i0-1];
    }

    // Vandermonde matrix and right-hand side of linear system
    GMatrix<Real> kA(riQuantity,riQuantity);
    Real* afB = new Real[riQuantity];

    for (i0 = 0; i0 <= iXDegree; i0++)
    {
        Real fSum = (Real)0.0;
        for (iS = 0; iS < iSamples; iS++)
        {
            fSum += afW[iS]*aafXP[iS][i0];
        }
        afB[i0] = fSum;

        for (i1 = 0; i1 <= iXDegree; i1++)
        {
            fSum = (Real)0.0;
            for (iS = 0; iS < iSamples; iS++)
            {
                fSum += aafXP[iS][i0+i1];
            }
            kA(i0,i1) = fSum;
        }
    }

    // solve for the polynomial coefficients
    bool bHasSolution = LinearSystem<Real>::Solve(kA,afB,rafC);
    assert( bHasSolution );

    Deallocate2D(aafXP);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM void PolyFit1<float> (int, const float*, const float*,
    int, int&, float*&);

template WML_ITEM void PolyFit1<double> (int, const double*, const double*,
    int, int&, double*&);
}
//----------------------------------------------------------------------------
