// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef ROTKEY_H
#define ROTKEY_H

#include "WmlQuaternion.h"
using namespace Wml;

class RotKey
{
public:
    RotKey ();

    float& Time ();
    Quaternionf& Q ();
    float& Tension ();
    float& Continuity ();
    float& Bias ();

private:
    float m_fTime;
    Quaternionf m_kQ;

    // parameters are each in [-1,1]
    float m_fTension, m_fContinuity, m_fBias;
};

#include "RotKey.inl"

#endif
