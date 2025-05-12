// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "WmlMassSpringCurve.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real, class TVector>
MassSpringCurve<Real,TVector>::MassSpringCurve (int iNumParticles, Real fStep)
    :
    ParticlePhysics<Real,TVector>(iNumParticles,fStep)
{
    m_iNumSprings = m_iNumParticles - 1;
    m_afConstant = new Real[m_iNumSprings];
    m_afLength = new Real[m_iNumSprings];
    memset(m_afConstant,0,m_iNumSprings*sizeof(Real));
    memset(m_afLength,0,m_iNumSprings*sizeof(Real));
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
MassSpringCurve<Real,TVector>::~MassSpringCurve ()
{
    delete[] m_afConstant;
    delete[] m_afLength;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
int MassSpringCurve<Real,TVector>::GetNumSprings () const
{
    return m_iNumSprings;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
Real& MassSpringCurve<Real,TVector>::Constant (int i)
{
    assert( 0 <= i && i < m_iNumSprings );
    return m_afConstant[i];
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
Real& MassSpringCurve<Real,TVector>::Length (int i)
{
    assert( 0 <= i && i < m_iNumSprings );
    return m_afLength[i];
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
TVector MassSpringCurve<Real,TVector>::Impulse (int i, Real fTime,
    const TVector* akPosition, const TVector* akVelocity)
{
    // Compute spring forces on position X[i].  The positions are not
    // necessarily m_akPosition since the RK4 solver in ParticlePhysics
    // evaluates the impulse function at intermediate positions.  The end
    // points of the curve of masses must be handled separately since each
    // has only one spring attached to it.

    TVector kImpulse = ExternalImpulse(i,fTime,akPosition,akVelocity);

    TVector kDiff, kForce;
    Real fRatio;

    if ( i > 0 )
    {
        int iM1 = i-1;
        kDiff = akPosition[iM1] - akPosition[i];
        fRatio = m_afLength[iM1]/kDiff.Length();
        kForce = m_afConstant[iM1]*(((Real)1.0)-fRatio)*kDiff;
        kImpulse += m_afInvMass[i]*kForce;
    }

    int iP1 = i+1;
    if ( iP1 < m_iNumParticles )
    {
        kDiff = akPosition[iP1] - akPosition[i];
        fRatio = m_afLength[i]/kDiff.Length();
        kForce = m_afConstant[i]*(((Real)1.0)-fRatio)*kDiff;
        kImpulse += m_afInvMass[i]*kForce;
    }

    return kImpulse;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
TVector MassSpringCurve<Real,TVector>::ExternalImpulse (int, Real,
    const TVector*, const TVector*)
{
    return TVector::ZERO;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM MassSpringCurve<float,Vector2f>;
template class WML_ITEM MassSpringCurve<double,Vector2d>;
template class WML_ITEM MassSpringCurve<float,Vector3f>;
template class WML_ITEM MassSpringCurve<double,Vector3d>;
}
//----------------------------------------------------------------------------
