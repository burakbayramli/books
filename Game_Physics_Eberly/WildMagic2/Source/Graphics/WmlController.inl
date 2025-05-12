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
inline Controller::RepeatType& Controller::Repeat ()
{
    return m_eRepeat;
}
//----------------------------------------------------------------------------
inline float& Controller::MinTime ()
{
    return m_fMinTime;
}
//----------------------------------------------------------------------------
inline float& Controller::MaxTime ()
{
    return m_fMaxTime;
}
//----------------------------------------------------------------------------
inline float& Controller::Phase ()
{
    return m_fPhase;
}
//----------------------------------------------------------------------------
inline float& Controller::Frequency ()
{
    return m_fFrequency;
}
//----------------------------------------------------------------------------
inline bool& Controller::Active ()
{
    return m_bActive;
}
//----------------------------------------------------------------------------
inline bool Controller::Active () const
{
    return m_bActive;
}
//----------------------------------------------------------------------------
inline Object* Controller::GetObject () const
{
    return m_pkObject;
}
//----------------------------------------------------------------------------
inline void Controller::SetNext (Controller* pkNext)
{
    m_spkNext = pkNext;
}
//----------------------------------------------------------------------------
inline Controller* Controller::GetNext () const
{
    return m_spkNext;
}
//----------------------------------------------------------------------------


