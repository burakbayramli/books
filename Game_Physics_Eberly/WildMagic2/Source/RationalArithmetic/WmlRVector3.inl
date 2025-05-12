// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.wild-magic.com/License/WildMagic.pdf and
// may not be copied or disclosed except in accordance with the terms of that
// agreement.

//----------------------------------------------------------------------------
template <int ISIZE>
RVector3<ISIZE>::RVector3 ()
{
    // the vector is uninitialized
}
//----------------------------------------------------------------------------
template <int ISIZE>
RVector3<ISIZE>::RVector3 (const RVector3& rkV)
{
    memcpy(m_akTuple,rkV.m_akTuple,3*sizeof(TRational<ISIZE>));
}
//----------------------------------------------------------------------------
template <int ISIZE>
RVector3<ISIZE>::RVector3 (const TRational<ISIZE>& rkX,
    const TRational<ISIZE>& rkY, const TRational<ISIZE>& rkZ)
{
    m_akTuple[0] = rkX;
    m_akTuple[1] = rkY;
    m_akTuple[2] = rkZ;
}
//----------------------------------------------------------------------------
template <int ISIZE>
RVector3<ISIZE>& RVector3<ISIZE>::operator= (const RVector3& rkV)
{
    memcpy(m_akTuple,rkV.m_akTuple,3*sizeof(TRational<ISIZE>));
    return *this;
}
//----------------------------------------------------------------------------
template <int ISIZE>
TRational<ISIZE> RVector3<ISIZE>::X () const
{
    return m_akTuple[0];
}
//----------------------------------------------------------------------------
template <int ISIZE>
TRational<ISIZE>& RVector3<ISIZE>::X ()
{
    return m_akTuple[0];
}
//----------------------------------------------------------------------------
template <int ISIZE>
TRational<ISIZE> RVector3<ISIZE>::Y () const
{
    return m_akTuple[1];
}
//----------------------------------------------------------------------------
template <int ISIZE>
TRational<ISIZE>& RVector3<ISIZE>::Y ()
{
    return m_akTuple[1];
}
//----------------------------------------------------------------------------
template <int ISIZE>
TRational<ISIZE> RVector3<ISIZE>::Z () const
{
    return m_akTuple[2];
}
//----------------------------------------------------------------------------
template <int ISIZE>
TRational<ISIZE>& RVector3<ISIZE>::Z ()
{
    return m_akTuple[2];
}
//----------------------------------------------------------------------------
template <int ISIZE>
RVector3<ISIZE> RVector3<ISIZE>::Cross (const RVector3& rkV) const
{
    return RVector3(
        m_akTuple[1]*rkV.m_akTuple[2] - m_akTuple[2]*rkV.m_akTuple[1],
        m_akTuple[2]*rkV.m_akTuple[0] - m_akTuple[0]*rkV.m_akTuple[2],
        m_akTuple[0]*rkV.m_akTuple[1] - m_akTuple[1]*rkV.m_akTuple[0]);
}
//----------------------------------------------------------------------------
template <int ISIZE>
TRational<ISIZE> RVector3<ISIZE>::TripleScalar (const RVector3& rkU,
    const RVector3& rkV) const
{
    return Dot(rkU.Cross(rkV));
}
//----------------------------------------------------------------------------
