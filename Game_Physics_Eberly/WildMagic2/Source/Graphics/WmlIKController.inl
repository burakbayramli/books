// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

//----------------------------------------------------------------------------
inline int& IKController::JointQuantity ()
{
    return m_iJointQuantity;
}
//----------------------------------------------------------------------------
inline IKJoint**& IKController::Joints ()
{
    return m_apkJoint;
}
//----------------------------------------------------------------------------
inline int& IKController::GoalQuantity ()
{
    return m_iGoalQuantity;
}
//----------------------------------------------------------------------------
inline IKGoal**& IKController::Goals ()
{
    return m_apkGoal;
}
//----------------------------------------------------------------------------
inline int& IKController::Iterations ()
{
    return m_iIterations;
}
//----------------------------------------------------------------------------
inline IKController::ProcessOrder& IKController::Order ()
{
    return m_eOrder;
}
//----------------------------------------------------------------------------
