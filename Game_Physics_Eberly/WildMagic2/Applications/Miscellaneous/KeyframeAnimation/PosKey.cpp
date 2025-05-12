// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "PosKey.h"

//----------------------------------------------------------------------------
PosKey::PosKey ()
    :
    m_kP(Vector3f::ZERO)
{
    m_fTime = 0.0f;
    m_fTension = 0.0f;
    m_fContinuity = 0.0f;
    m_fBias = 0.0f;
}
//----------------------------------------------------------------------------
