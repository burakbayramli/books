// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLIKGOAL_H
#define WMLIKGOAL_H

#include "WmlObject.h"
#include "WmlVector3.h"

namespace Wml
{

class Spatial;

class WML_ITEM IKGoal : public Object
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    IKGoal (Spatial* pkTarget, Spatial* pkEffector, float fWeight);

    Spatial*& Target ();
    Spatial*& Effector ();
    float& Weight ();

    Vector3f GetPosition () const;
    Vector3f GetEffectorPosition () const;
    float GetNorm () const;

protected:
    IKGoal ();

    Spatial* m_pkTarget;
    Spatial* m_pkEffector;
    float m_fWeight;
};

WmlSmartPointer(IKGoal);
WmlRegisterStream(IKGoal);
#include "WmlIKGoal.inl"

}

#endif
