// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLIKCONTROLLER_H
#define WMLIKCONTROLLER_H

#include "WmlController.h"
#include "WmlVector3.h"

namespace Wml
{

class IKJoint;
class IKGoal;
class Spatial;

class WML_ITEM IKController : public Controller
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    enum ProcessOrder
    {
        PO_END_TO_ROOT,
        PO_ROOT_TO_END
    };

    IKController (int iJointQuantity, IKJoint** apkJoint, int iGoalQuantity,
        IKGoal** apkGoal, int iIterations = 128,
        ProcessOrder eOrder = PO_END_TO_ROOT);

    virtual ~IKController ();

    virtual bool Update (float fAppTime);

    int& JointQuantity ();
    IKJoint**& Joints ();

    int& GoalQuantity ();
    IKGoal**& Goals ();

    int& Iterations ();  // default = 16

    ProcessOrder& Order ();

protected:
    IKController ();

    int m_iJointQuantity;
    IKJoint** m_apkJoint;

    int m_iGoalQuantity;
    IKGoal** m_apkGoal;

    int m_iIterations;
    ProcessOrder m_eOrder;
};

WmlSmartPointer(IKController);
WmlRegisterStream(IKController);
#include "WmlIKController.inl"

}

#endif
