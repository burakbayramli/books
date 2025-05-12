// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLPOINTCONTROLLER_H
#define WMLPOINTCONTROLLER_H

#include "WmlController.h"
#include "WmlVector3.h"

namespace Wml
{

class WML_ITEM PointController : public Controller
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    virtual ~PointController ();

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

    // If the controlled object ever reallocates its points, the motion
    // parameters must be reallocated.
    virtual void Reallocate (int iVertexQuantity);

    virtual void SetObject (Object* pkObject);
    virtual bool Update (float fAppTime);

protected:
    // streaming support
    PointController ();

    // This class computes the new positions and orientations from the motion
    // parameters.  Derived classes should update the motion parameters and
    // then either call the base class update methods or provide its own
    // update methods for position and orientation.
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
};

WmlSmartPointer(PointController);
WmlRegisterStream(PointController);
#include "WmlPointController.inl"

}

#endif
