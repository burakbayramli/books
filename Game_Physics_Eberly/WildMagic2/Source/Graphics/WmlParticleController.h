// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLPARTICLECONTROLLER_H
#define WMLPARTICLECONTROLLER_H

#include "WmlController.h"

namespace Wml
{

class WML_ITEM ParticleController : public Controller
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    virtual ~ParticleController ();

    // system motion (velocity vectors are unit length)
    float& SystemLinearSpeed ();
    float& SystemAngularSpeed ();
    Vector3f& SystemLinearAxis ();
    Vector3f& SystemAngularAxis ();

    // point motion (velocity vectors are unit length)
    float* PointLinearSpeed ();
    float* PointAngularSpeed ();
    Vector3f* PointLinearAxis ();
    Vector3f* PointAngularAxis ();

    // size change parameters
    float& SystemSizeChange ();
    float* PointSizeChange ();

    // If the controlled object ever reallocates its points, the motion
    // parameters must be reallocated.
    virtual void Reallocate (int iVertexQuantity);

    virtual void SetObject (Object* pkObject);
    virtual bool Update (float fAppTime);

protected:
    // streaming support
    ParticleController ();

    // update the motion parameters
    virtual void UpdateSystemMotion (float fCtrlTime);
    virtual void UpdatePointMotion (float fCtrlTime);

    // system motion (in local coordinates)
    float m_fSystemLinearSpeed;
    float m_fSystemAngularSpeed;
    Vector3f m_kSystemLinearAxis;
    Vector3f m_kSystemAngularAxis;

    // point motion (in model space of system)
    float* m_afPointLinearSpeed;
    float* m_afPointAngularSpeed;
    Vector3f* m_akPointLinearAxis;
    Vector3f* m_akPointAngularAxis;

    // size change parameters
    float m_fSystemSizeChange;
    float* m_afPointSizeChange;
};

WmlSmartPointer(ParticleController);
WmlRegisterStream(ParticleController);
#include "WmlParticleController.inl"

}

#endif
