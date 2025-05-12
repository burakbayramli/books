// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "WmlMassSpringArbitrary.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real, class TVector>
MassSpringArbitrary<Real,TVector>::MassSpringArbitrary (int iNumParticles,
    int iNumSprings, Real fStep)
    :
    ParticlePhysics<Real,TVector>(iNumParticles,fStep)
{
    m_iNumSprings = iNumSprings;
    m_akSpring = new Spring[m_iNumSprings];
    memset(m_akSpring,0,m_iNumSprings*sizeof(Spring));

    m_akAdjacent = new UnorderedSet<int>[m_iNumParticles];
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
MassSpringArbitrary<Real,TVector>::~MassSpringArbitrary ()
{
    delete[] m_akSpring;
    delete[] m_akAdjacent;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
int MassSpringArbitrary<Real,TVector>::GetNumSprings () const
{
    return m_iNumSprings;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
void MassSpringArbitrary<Real,TVector>::SetSpring (int iSpring,
    int iParticle0, int iParticle1, Real fConstant, Real fLength)
{
    assert( 0 <= iSpring && iSpring < m_iNumSprings
        &&  0 <= iParticle0 && iParticle0 < m_iNumParticles
        &&  0 <= iParticle1 && iParticle1 < m_iNumParticles );

    m_akSpring[iSpring].Particle0 = iParticle0;
    m_akSpring[iSpring].Particle1 = iParticle1;
    m_akSpring[iSpring].Constant = fConstant;
    m_akSpring[iSpring].Length = fLength;

    m_akAdjacent[iParticle0].Insert(iSpring);
    m_akAdjacent[iParticle1].Insert(iSpring);
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
void MassSpringArbitrary<Real,TVector>::GetSpring (int iSpring,
    int& riParticle0, int& riParticle1, Real& rfConstant, Real& rfLength)
    const
{
    assert( 0 <= iSpring && iSpring < m_iNumSprings );
    riParticle0 = m_akSpring[iSpring].Particle0;
    riParticle1 = m_akSpring[iSpring].Particle1;
    rfConstant = m_akSpring[iSpring].Constant;
    rfLength = m_akSpring[iSpring].Length;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
Real& MassSpringArbitrary<Real,TVector>::Constant (int iSpring)
{
    assert( 0 <= iSpring && iSpring < m_iNumSprings );
    return m_akSpring[iSpring].Constant;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
Real& MassSpringArbitrary<Real,TVector>::Length (int iSpring)
{
    assert( 0 <= iSpring && iSpring < m_iNumSprings );
    return m_akSpring[iSpring].Length;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
TVector MassSpringArbitrary<Real,TVector>::Impulse (int i, Real fTime,
    const TVector* akPosition, const TVector* akVelocity)
{
    // Compute spring forces on position X[i].  The positions are not
    // necessarily m_akPosition since the RK4 solver in ParticlePhysics
    // evaluates the impulse function at intermediate positions.

    TVector kImpulse = ExternalImpulse(i,fTime,akPosition,akVelocity);

    TVector kDiff, kForce;
    Real fRatio;

    const UnorderedSet<int>& rkAdj = m_akAdjacent[i];
    for (int j = 0; j < rkAdj.GetQuantity(); j++)
    {
        // process a spring connected to particle i
        const Spring& rkSpring = m_akSpring[rkAdj.Get(j)];
        if ( i != rkSpring.Particle0 )
            kDiff = akPosition[rkSpring.Particle0] - akPosition[i];
        else
            kDiff = akPosition[rkSpring.Particle1] - akPosition[i];

        fRatio = rkSpring.Length/kDiff.Length();
        kForce = rkSpring.Constant*(((Real)1.0)-fRatio)*kDiff;
        kImpulse += m_afInvMass[i]*kForce;
    }

    return kImpulse;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
TVector MassSpringArbitrary<Real,TVector>::ExternalImpulse (int, Real,
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
template class WML_ITEM MassSpringArbitrary<float,Vector2f>;
template class WML_ITEM MassSpringArbitrary<double,Vector2d>;
template class WML_ITEM MassSpringArbitrary<float,Vector3f>;
template class WML_ITEM MassSpringArbitrary<double,Vector3d>;
}
//----------------------------------------------------------------------------
