// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef WMLPARTICLEPHYSICS_H
#define WMLPARTICLEPHYSICS_H

#include "WmlVector2.h"
#include "WmlVector3.h"

namespace Wml
{

template <class Real, class TVector>
class WML_ITEM ParticlePhysics
{
public:
    // Construction and destruction.  If a particle is to be immovable, set
    // its mass to Math<Real>::MAX_REAL.
    ParticlePhysics (int iNumParticles, Real fStep);
    virtual ~ParticlePhysics ();

    int GetNumParticles () const;
    void SetMass (int i, Real fMass);
    Real GetMass (int i) const;
    TVector* Positions () const;
    TVector& Position (int i);
    TVector* Velocities () const;
    TVector& Velocity (int i);
    void SetStep (Real fStep);
    Real GetStep () const;

    // Callback for physical impulses (ODE solver uses x" = F/m) applied to
    // particle i.  The positions and velocities are not necessarily
    // m_akPosition and m_akVelocity since the ODE solver evaluates the
    // impulse function at intermediate positions.
    virtual TVector Impulse (int i, Real fTime, const TVector* akPosition,
        const TVector* akVelocity) = 0;

    // Update the particle positions based on current time and particle state.
    // The Impulse(...) function is called in this update for each particle.
    // This function is virtual so that derived classes can perform pre-update
    // and/or post-update semantics.
    virtual void Update (Real fTime);

protected:
    int m_iNumParticles;
    Real* m_afMass;
    Real* m_afInvMass;
    TVector* m_akPosition;
    TVector* m_akVelocity;
    Real m_fStep, m_fHalfStep, m_fSixthStep;

    // temporary storage for solver
    typedef TVector* TVectorPtr;
    TVectorPtr m_akPTmp, m_akDPTmp1, m_akDPTmp2, m_akDPTmp3, m_akDPTmp4;
    TVectorPtr m_akVTmp, m_akDVTmp1, m_akDVTmp2, m_akDVTmp3, m_akDVTmp4;
};

typedef ParticlePhysics<float,Vector2f> ParticlePhysics2f;
typedef ParticlePhysics<double,Vector2d> ParticlePhysics2d;
typedef ParticlePhysics<float,Vector3f> ParticlePhysics3f;
typedef ParticlePhysics<double,Vector3d> ParticlePhysics3d;

}

#endif
