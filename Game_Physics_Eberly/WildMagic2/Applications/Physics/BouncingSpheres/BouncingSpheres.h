// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef BOUNCINGSPHERES_H
#define BOUNCINGSPHERES_H

#include "WmlApplication.h"
#include "RigidBall.h"
#include <vector>

class BouncingSpheres : public Application
{
public:
    BouncingSpheres ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:

    class Contact
    {
    public:
        RigidBodyf* A;  // ball containing face
        RigidBodyf* B;  // ball containing vertex
        Vector3f P;     // contact point
        Vector3f N;     // outward unit-length normal to face
    };

    void CreateBalls ();
    void CreateFloor ();
    void CreateBackWall ();
    void CreateSideWall1 ();
    void CreateSideWall2 ();
    void DoPhysical ();
    void DoVisual ();
    void DoCollisionDetection ();
    void DoCollisionResponse ();
    void ComputePreimpulseVelocity (float* afPreRelVel);
    void ComputeImpulseMagnitude (float* afPreRelVel, float* afImpulseMag);
    void DoImpulse (float* fImpulseMag);
    void DoMotion ();
    void SetBoundaryContact (int i, int iBIndex, const Vector3f& rkPos, 
        float fRadius, Contact& rkContact);

    // external forces and torques for this application
    static Vector3f Force (float fTime, float fMass, const Vector3f& rkPos,
        const Quaternionf& rkQOrient, const Vector3f& rkLinMom,
        const Vector3f& rkAngMom, const Matrix3f& rkOrient,
        const Vector3f& rkLinVel, const Vector3f& rkAngVel);

    static Vector3f Torque (float fTime, float fMass, const Vector3f& rkPos,
        const Quaternionf& rkQOrient, const Vector3f& rkLinMom,
        const Vector3f& rkAngMom, const Matrix3f& rkOrient,
        const Vector3f& rkLinVel, const Vector3f& rkAngVel);

    // rigid spheres are the rigid bodies
    enum { NUM_BALLS = 16 };
    RigidBall* m_apkBall[NUM_BALLS];
 
    // representation of boundaries (floor, ceiling, walls)
    RigidBodyf m_akBoundary[6];
    Vector3f m_akBLocation[6];
    Vector3f m_akBNormal[6];

    // contact points during one pass of the physical simulation
    int m_iNumContacts;
    std::vector<Contact> m_kBContact;

    // blocked directions
    std::vector<Vector3f> m_akBlocked[NUM_BALLS];

    // simulated clock
    float m_fSimTime, m_fSimDelta;

    // the scene graph
    NodePtr m_spkScene;
    WireframeStatePtr m_spkWireframeState;
    ZBufferStatePtr m_spkZBufferState;
    NodePtr m_aspkBall[NUM_BALLS];
    TriMeshPtr m_spkFloor, m_spkBackWall, m_spkSideWall1, m_spkSideWall2;
    PolylinePtr m_spkLine;
};

#endif
