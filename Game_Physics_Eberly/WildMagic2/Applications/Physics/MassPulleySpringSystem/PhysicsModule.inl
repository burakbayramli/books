// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

//----------------------------------------------------------------------------
inline double PhysicsModule::GetInitialY1 () const
{
    return m_dY1;
}
//----------------------------------------------------------------------------
inline double PhysicsModule::GetCurrentY1 () const
{
    return m_dY1Curr;
}
//----------------------------------------------------------------------------
inline double PhysicsModule::GetCurrentY2 () const
{
    return m_dY2Curr;
}
//----------------------------------------------------------------------------
inline double PhysicsModule::GetCurrentY3 () const
{
    return m_dY3Curr;
}
//----------------------------------------------------------------------------
inline double PhysicsModule::GetAngle () const
{
    return (m_dY1 - m_dY1Curr)/Radius;
}
//----------------------------------------------------------------------------
inline double PhysicsModule::GetCableFraction1 () const
{
    return (m_dY1Curr - m_dY3Curr)/WireLength;
}
//----------------------------------------------------------------------------
inline double PhysicsModule::GetCableFraction2 () const
{
    return (m_dY2Curr - m_dY3Curr)/WireLength;
}
//----------------------------------------------------------------------------
