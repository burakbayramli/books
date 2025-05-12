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
inline Plane3f& BspNode::ModelPlane ()
{
    return m_kModelPlane;
}
//----------------------------------------------------------------------------
inline const Plane3f& BspNode::ModelPlane () const
{
    return m_kModelPlane;
}
//----------------------------------------------------------------------------
inline const Plane3f& BspNode::GetWorldPlane () const
{
    return m_kWorldPlane;
}
//----------------------------------------------------------------------------
inline void*& BspNode::Data ()
{
    return m_pvData;
}
//----------------------------------------------------------------------------
inline BspNode::CallbackFunction& BspNode::Callback ()
{
    return m_oCallback;
}
//----------------------------------------------------------------------------
