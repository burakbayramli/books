// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef DEFORMABLEBALL_H
#define DEFORMABLEBALL_H

#include "WmlMeshSmoother.h"
#include "WmlTriMesh.h"
using namespace Wml;

class DeformableBall : public MeshSmoother<float>
{
public:
    // construction and destruction
    DeformableBall (float fDuration, float fPeriod);
    virtual ~DeformableBall ();

    // member access
    TriMeshPtr& Mesh ();
    void Set (float fDuration, float fPeriod);
    float GetDuration () const;
    float GetPeriod () const;
    float GetMinActive () const;
    float GetMaxActive () const;
    float GetAmplitude (float fTime);
    bool& DoAffine ();

    // deform the ball
    bool DoSimulationStep (float fRealTime);

private:
    void CreateBall ();

    // Influence function:  parameters (X,t)
    //   input:  X = point on surface
    //           t = current time
    // The return value is 'true' if and only if the point is within the
    // region of influence of the deformation *and* if the deformation
    // function is active at time t.  This allows MeshSmoother to avoid
    // evolving the surface in regions where no deformation is occuring.
    virtual bool VertexInfluenced (int i, float fTime);

    virtual float GetTangentWeight (int i, float fTime);
    virtual float GetNormalWeight (int i, float fTime);

    // Level surface function:  parameters (X,t,F,Grad(F))
    //   input:  X = point in space
    //           t = time of deformation
    //   output: F(X,t) = scalar function at position and time
    //           Grad(F)(X,t) = gradient at level surface through X at time
    void ComputeFunction (const Vector3f& rkPos, float fTime, float& rfFunc,
        Vector3f& rkGrad);

    TriMeshPtr m_spkMesh;
    float m_fDuration, m_fPeriod, m_fDeformMult;
    float m_fMinActive, m_fMaxActive, m_fInvActiveRange;
    bool m_bDeforming, m_bDoAffine;

    // parameters for Newton's method in ComputeFunction
    int m_iMaxIterations;
    float m_fErrorTolerance;
};

#include "DeformableBall.inl"

#endif
