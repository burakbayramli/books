// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "WmlPolyhedralMassProperties.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
void Wml::ComputeMassProperties (const Vector3<Real>* akVertex,
    int iTQuantity, const int* aiConnect, bool bBodyCoords, Real& rfMass,
    Vector3<Real>& rkCenter, Matrix3<Real>& rkInertia)
{
    const Real fOneDiv6 = (Real)(1.0/6.0);
    const Real fOneDiv24 = (Real)(1.0/24.0);
    const Real fOneDiv60 = (Real)(1.0/60.0);
    const Real fOneDiv120 = (Real)(1.0/120.0);

    // order:  1, x, y, z, x^2, y^2, z^2, xy, yz, zx
    Real afIntegral[10] = { (Real)0.0, (Real)0.0, (Real)0.0, (Real)0.0,
        (Real)0.0, (Real)0.0, (Real)0.0, (Real)0.0, (Real)0.0, (Real)0.0 };

    const int* piConnect = aiConnect;
    int i;
    for (i = 0; i < iTQuantity; i++)
    {
        // get vertices of triangle i
        Vector3<Real> kV0 = akVertex[*piConnect++];
        Vector3<Real> kV1 = akVertex[*piConnect++];
        Vector3<Real> kV2 = akVertex[*piConnect++];

        // get cross product of edges
        Vector3<Real> kV1mV0 = kV1 - kV0;
        Vector3<Real> kV2mV0 = kV2 - kV0;
        Vector3<Real> kN = kV1mV0.Cross(kV2mV0);

        // compute integral terms
        Real fTmp0, fTmp1, fTmp2;
        Real fF1x, fF2x, fF3x, fG0x, fG1x, fG2x;
        fTmp0 = kV0.X() + kV1.X();
        fF1x = fTmp0 + kV2.X();
        fTmp1 = kV0.X()*kV0.X();
        fTmp2 = fTmp1 + kV1.X()*fTmp0;
        fF2x = fTmp2 + kV2.X()*fF1x;
        fF3x = kV0.X()*fTmp1 + kV1.X()*fTmp2 + kV2.X()*fF2x;
        fG0x = fF2x + kV0.X()*(fF1x + kV0.X());
        fG1x = fF2x + kV1.X()*(fF1x + kV1.X());
        fG2x = fF2x + kV2.X()*(fF1x + kV2.X());

        Real fF1y, fF2y, fF3y, fG0y, fG1y, fG2y;
        fTmp0 = kV0.Y() + kV1.Y();
        fF1y = fTmp0 + kV2.Y();
        fTmp1 = kV0.Y()*kV0.Y();
        fTmp2 = fTmp1 + kV1.Y()*fTmp0;
        fF2y = fTmp2 + kV2.Y()*fF1y;
        fF3y = kV0.Y()*fTmp1 + kV1.Y()*fTmp2 + kV2.Y()*fF2y;
        fG0y = fF2y + kV0.Y()*(fF1y + kV0.Y());
        fG1y = fF2y + kV1.Y()*(fF1y + kV1.Y());
        fG2y = fF2y + kV2.Y()*(fF1y + kV2.Y());

        Real fF1z, fF2z, fF3z, fG0z, fG1z, fG2z;
        fTmp0 = kV0.Z() + kV1.Z();
        fF1z = fTmp0 + kV2.Z();
        fTmp1 = kV0.Z()*kV0.Z();
        fTmp2 = fTmp1 + kV1.Z()*fTmp0;
        fF2z = fTmp2 + kV2.Z()*fF1z;
        fF3z = kV0.Z()*fTmp1 + kV1.Z()*fTmp2 + kV2.Z()*fF2z;
        fG0z = fF2z + kV0.Z()*(fF1z + kV0.Z());
        fG1z = fF2z + kV1.Z()*(fF1z + kV1.Z());
        fG2z = fF2z + kV2.Z()*(fF1z + kV2.Z());

        // update integrals
        afIntegral[0] += kN.X()*fF1x;
        afIntegral[1] += kN.X()*fF2x;
        afIntegral[2] += kN.Y()*fF2y;
        afIntegral[3] += kN.Z()*fF2z;
        afIntegral[4] += kN.X()*fF3x;
        afIntegral[5] += kN.Y()*fF3y;
        afIntegral[6] += kN.Z()*fF3z;
        afIntegral[7] += kN.X()*(kV0.Y()*fG0x + kV1.Y()*fG1x + kV2.Y()*fG2x);
        afIntegral[8] += kN.Y()*(kV0.Z()*fG0y + kV1.Z()*fG1y + kV2.Z()*fG2y);
        afIntegral[9] += kN.Z()*(kV0.X()*fG0z + kV1.X()*fG1z + kV2.X()*fG2z);
    }

    afIntegral[0] *= fOneDiv6;
    afIntegral[1] *= fOneDiv24;
    afIntegral[2] *= fOneDiv24;
    afIntegral[3] *= fOneDiv24;
    afIntegral[4] *= fOneDiv60;
    afIntegral[5] *= fOneDiv60;
    afIntegral[6] *= fOneDiv60;
    afIntegral[7] *= fOneDiv120;
    afIntegral[8] *= fOneDiv120;
    afIntegral[9] *= fOneDiv120;

    // mass
    rfMass = afIntegral[0];

    // center of mass
    rkCenter =
        Vector3<Real>(afIntegral[1],afIntegral[2],afIntegral[3])/rfMass;

    // inertia relative to world origin
    rkInertia[0][0] = afIntegral[5] + afIntegral[6];
    rkInertia[0][1] = -afIntegral[7];
    rkInertia[0][2] = -afIntegral[9];
    rkInertia[1][0] = rkInertia[0][1];
    rkInertia[1][1] = afIntegral[4] + afIntegral[6];
    rkInertia[1][2] = -afIntegral[8];
    rkInertia[2][0] = rkInertia[0][2];
    rkInertia[2][1] = rkInertia[1][2];
    rkInertia[2][2] = afIntegral[4] + afIntegral[5];

    // inertia relative to center of mass
    if ( bBodyCoords )
    {
        rkInertia[0][0] -= rfMass*(rkCenter.Y()*rkCenter.Y() +
            rkCenter.Z()*rkCenter.Z());
        rkInertia[0][1] += rfMass*rkCenter.X()*rkCenter.Y();
        rkInertia[0][2] += rfMass*rkCenter.Z()*rkCenter.X();
        rkInertia[1][0] = rkInertia[0][1];
        rkInertia[1][1] -= rfMass*(rkCenter.Z()*rkCenter.Z() +
            rkCenter.X()*rkCenter.X());
        rkInertia[1][2] += rfMass*rkCenter.Y()*rkCenter.Z();
        rkInertia[2][0] = rkInertia[0][2];
        rkInertia[2][1] = rkInertia[1][2];
        rkInertia[2][2] -= rfMass*(rkCenter.X()*rkCenter.X() +
            rkCenter.Y()*rkCenter.Y());
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template void WML_ITEM ComputeMassProperties<float>
    (const Vector3<float>*, int, const int*, bool, float&,
    Vector3<float>&, Matrix3<float>&);

template void WML_ITEM ComputeMassProperties<double>
    (const Vector3<double>*, int, const int*, bool, double&,
    Vector3<double>&, Matrix3<double>&);
}
//----------------------------------------------------------------------------
