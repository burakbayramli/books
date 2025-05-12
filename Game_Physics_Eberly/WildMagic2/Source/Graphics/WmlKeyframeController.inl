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
inline void KeyframeController::SetTranslationQuantity (int iTQuantity)
{
    m_iTQuantity = iTQuantity;
}
//----------------------------------------------------------------------------
inline void KeyframeController::SetTranslationTimes (float* afTTime)
{
    m_afTTime = afTTime;
}
//----------------------------------------------------------------------------
inline void KeyframeController::SetTranslations (Vector3f* akTData)
{
    m_akTData = akTData;
}
//----------------------------------------------------------------------------
inline int KeyframeController::GetTranslationQuantity () const
{
    return m_iTQuantity;
}
//----------------------------------------------------------------------------
inline float* KeyframeController::GetTranslationTimes () const
{
    return (float*) m_afTTime;
}
//----------------------------------------------------------------------------
inline Vector3f* KeyframeController::GetTranslations () const
{
    return (Vector3f*) m_akTData;
}
//----------------------------------------------------------------------------
inline void KeyframeController::SetRotationQuantity (int iRQuantity)
{
    m_iRQuantity = iRQuantity;
}
//----------------------------------------------------------------------------
inline void KeyframeController::SetRotationTimes (float* afRTime)
{
    m_afRTime = afRTime;
}
//----------------------------------------------------------------------------
inline void KeyframeController::SetRotations (Quaternionf* akRData)
{
    m_akRData = akRData;
}
//----------------------------------------------------------------------------
inline int KeyframeController::GetRotationQuantity () const
{
    return m_iRQuantity;
}
//----------------------------------------------------------------------------
inline float* KeyframeController::GetRotationTimes () const
{
    return (float*) m_afRTime;
}
//----------------------------------------------------------------------------
inline Quaternionf* KeyframeController::GetRotations () const
{
    return (Quaternionf*) m_akRData;
}
//----------------------------------------------------------------------------
inline void KeyframeController::SetScaleQuantity (int iSQuantity)
{
    m_iSQuantity = iSQuantity;
}
//----------------------------------------------------------------------------
inline void KeyframeController::SetScaleTimes (float* afSTime)
{
    m_afSTime = afSTime;
}
//----------------------------------------------------------------------------
inline void KeyframeController::SetScales (float* afSData)
{
    m_afSData = afSData;
}
//----------------------------------------------------------------------------
inline int KeyframeController::GetScaleQuantity () const
{
    return m_iSQuantity;
}
//----------------------------------------------------------------------------
inline float* KeyframeController::GetScaleTimes () const
{
    return (float*) m_afSTime;
}
//----------------------------------------------------------------------------
inline float* KeyframeController::GetScales () const
{
    return (float*) m_afSData;
}
//----------------------------------------------------------------------------
inline int KeyframeController::GetSharedQuantity () const
{
    return m_iSharedQuantity;
}
//----------------------------------------------------------------------------
inline float* KeyframeController::GetSharedTimes () const
{
    return (float*) m_afSharedTime;
}
//----------------------------------------------------------------------------
