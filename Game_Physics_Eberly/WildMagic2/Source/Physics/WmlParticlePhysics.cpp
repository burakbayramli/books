// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "WmlParticlePhysics.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real, class TVector>
ParticlePhysics<Real,TVector>::ParticlePhysics (int iNumParticles, Real fStep)
{
    m_iNumParticles = iNumParticles;
    SetStep(fStep);

    m_afMass = new Real[m_iNumParticles];
    m_afInvMass = new Real[m_iNumParticles];
    m_akPosition = new TVector[m_iNumParticles];
    m_akVelocity = new TVector[m_iNumParticles];

    memset(m_afMass,0,m_iNumParticles*sizeof(Real));
    memset(m_afInvMass,0,m_iNumParticles*sizeof(Real));
    memset(m_akPosition,0,m_iNumParticles*sizeof(TVector));
    memset(m_akVelocity,0,m_iNumParticles*sizeof(TVector));

    m_akPTmp = new TVector[m_iNumParticles];
    m_akDPTmp1 = new TVector[m_iNumParticles];
    m_akDPTmp2 = new TVector[m_iNumParticles];
    m_akDPTmp3 = new TVector[m_iNumParticles];
    m_akDPTmp4 = new TVector[m_iNumParticles];
    m_akVTmp = new TVector[m_iNumParticles];
    m_akDVTmp1 = new TVector[m_iNumParticles];
    m_akDVTmp2 = new TVector[m_iNumParticles];
    m_akDVTmp3 = new TVector[m_iNumParticles];
    m_akDVTmp4 = new TVector[m_iNumParticles];
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
ParticlePhysics<Real,TVector>::~ParticlePhysics ()
{
    delete[] m_afMass;
    delete[] m_afInvMass;
    delete[] m_akPosition;
    delete[] m_akVelocity;
    delete[] m_akPTmp;
    delete[] m_akDPTmp1;
    delete[] m_akDPTmp2;
    delete[] m_akDPTmp3;
    delete[] m_akDPTmp4;
    delete[] m_akVTmp;
    delete[] m_akDVTmp1;
    delete[] m_akDVTmp2;
    delete[] m_akDVTmp3;
    delete[] m_akDVTmp4;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
int ParticlePhysics<Real,TVector>::GetNumParticles () const
{
    return m_iNumParticles;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
void ParticlePhysics<Real,TVector>::SetMass (int i, Real fMass)
{
    assert( 0 <= i && i < m_iNumParticles && fMass > (Real)0.0 );
    m_afMass[i] = fMass;
    if ( fMass != Math<Real>::MAX_REAL )
        m_afInvMass[i] = ((Real)1.0)/fMass;
    else
        m_afInvMass[i] = (Real)0.0;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
Real ParticlePhysics<Real,TVector>::GetMass (int i) const
{
    assert( 0 <= i && i < m_iNumParticles );
    return m_afMass[i];
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
TVector* ParticlePhysics<Real,TVector>::Positions () const
{
    return m_akPosition;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
TVector& ParticlePhysics<Real,TVector>::Position (int i)
{
    assert( 0 <= i && i < m_iNumParticles );
    return m_akPosition[i];
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
TVector* ParticlePhysics<Real,TVector>::Velocities () const
{
    return m_akVelocity;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
TVector& ParticlePhysics<Real,TVector>::Velocity (int i)
{
    assert( 0 <= i && i < m_iNumParticles );
    return m_akVelocity[i];
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
void ParticlePhysics<Real,TVector>::SetStep (Real fStep)
{
    m_fStep = fStep;
    m_fHalfStep = ((Real)0.5)*m_fStep;
    m_fSixthStep = m_fStep/(Real)6.0;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
Real ParticlePhysics<Real,TVector>::GetStep () const
{
    return m_fStep;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
void ParticlePhysics<Real,TVector>::Update (Real fTime)
{
    // Runge-Kutta fourth-order solver
    Real fHalfTime = fTime + m_fHalfStep;
    Real fFullTime = fTime + m_fStep;

    // first step
    int i;
    for (i = 0; i < m_iNumParticles; i++)
    {
        if ( m_afInvMass[i] > (Real)0.0 )
        {
            m_akDPTmp1[i] = m_akVelocity[i];
            m_akDVTmp1[i] = Impulse(i,fTime,m_akPosition,m_akVelocity);
        }
    }
    for (i = 0; i < m_iNumParticles; i++)
    {
        if ( m_afInvMass[i] > (Real)0.0 )
        {
            m_akPTmp[i] = m_akPosition[i] + m_fHalfStep*m_akDPTmp1[i];
            m_akVTmp[i] = m_akVelocity[i] + m_fHalfStep*m_akDVTmp1[i];
        }
        else
        {
            m_akPTmp[i] = m_akPosition[i];
            m_akVTmp[i] = TVector::ZERO;
        }
    }

    // second step
    for (i = 0; i < m_iNumParticles; i++)
    {
        if ( m_afInvMass[i] > (Real)0.0 )
        {
            m_akDPTmp2[i] = m_akVTmp[i];
            m_akDVTmp2[i] = Impulse(i,fHalfTime,m_akPTmp,m_akVTmp);
        }
    }
    for (i = 0; i < m_iNumParticles; i++)
    {
        if ( m_afInvMass[i] > (Real)0.0 )
        {
            m_akPTmp[i] = m_akPosition[i] + m_fHalfStep*m_akDPTmp2[i];
            m_akVTmp[i] = m_akVelocity[i] + m_fHalfStep*m_akDVTmp2[i];
        }
        else
        {
            m_akPTmp[i] = m_akPosition[i];
            m_akVTmp[i] = TVector::ZERO;
        }
    }

    // third step
    for (i = 0; i < m_iNumParticles; i++)
    {
        if ( m_afInvMass[i] > (Real)0.0 )
        {
            m_akDPTmp3[i] = m_akVTmp[i];
            m_akDVTmp3[i] = Impulse(i,fHalfTime,m_akPTmp,m_akVTmp);
        }
    }
    for (i = 0; i < m_iNumParticles; i++)
    {
        if ( m_afInvMass[i] > (Real)0.0 )
        {
            m_akPTmp[i] = m_akPosition[i] + m_fStep*m_akDPTmp3[i];
            m_akVTmp[i] = m_akVelocity[i] + m_fStep*m_akDVTmp3[i];
        }
        else
        {
            m_akPTmp[i] = m_akPosition[i];
            m_akVTmp[i] = TVector::ZERO;
        }
    }

    // fourth step
    for (i = 0; i < m_iNumParticles; i++)
    {
        if ( m_afInvMass[i] > (Real)0.0 )
        {
            m_akDPTmp4[i] = m_akVTmp[i];
            m_akDVTmp4[i] = Impulse(i,fFullTime,m_akPTmp,m_akVTmp);
        }
    }
    for (i = 0; i < m_iNumParticles; i++)
    {
        if ( m_afInvMass[i] > (Real)0.0 )
        {
            m_akPosition[i] += m_fSixthStep*(m_akDPTmp1[i] +
                ((Real)2.0)*(m_akDPTmp2[i] + m_akDPTmp3[i]) + m_akDPTmp4[i]);
            m_akVelocity[i] += m_fSixthStep*(m_akDVTmp1[i] +
                ((Real)2.0)*(m_akDVTmp2[i] + m_akDVTmp3[i]) + m_akDVTmp4[i]);
        }
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM ParticlePhysics<float,Vector2f>;
template class WML_ITEM ParticlePhysics<double,Vector2d>;
template class WML_ITEM ParticlePhysics<float,Vector3f>;
template class WML_ITEM ParticlePhysics<double,Vector3d>;
}
//----------------------------------------------------------------------------
