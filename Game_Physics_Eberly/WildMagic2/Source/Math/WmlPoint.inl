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
template <int N, class Real>
Point<N,Real>::Point ()
{
    // For efficiency in construction of large arrays of points, the
    // default constructor does not initialize the point.
}
//----------------------------------------------------------------------------
template <int N, class Real>
Point<N,Real>::Point (const Real* afTuple)
{
    memcpy(m_afTuple,afTuple,N*sizeof(Real));
}
//----------------------------------------------------------------------------
template <int N, class Real>
Point<N,Real>::Point (const Point& rkP)
{
    memcpy(m_afTuple,rkP.m_afTuple,N*sizeof(Real));
}
//----------------------------------------------------------------------------
template <int N, class Real>
Point<N,Real>::operator const Real* () const
{
    return m_afTuple;
}
//----------------------------------------------------------------------------
template <int N, class Real>
Point<N,Real>::operator Real* ()
{
    return m_afTuple;
}
//----------------------------------------------------------------------------
template <int N, class Real>
Real Point<N,Real>::operator[] (int i) const
{
    assert( 0 <= i && i < N );
    return m_afTuple[i];
}
//----------------------------------------------------------------------------
template <int N, class Real>
Real& Point<N,Real>::operator[] (int i)
{
    assert( 0 <= i && i < N );
    return m_afTuple[i];
}
//----------------------------------------------------------------------------
template <int N, class Real>
Point<N,Real>& Point<N,Real>::operator= (const Point& rkP)
{
    memcpy(m_afTuple,rkP.m_afTuple,N*sizeof(Real));
    return *this;
}
//----------------------------------------------------------------------------
template <int N, class Real>
bool Point<N,Real>::operator== (const Point& rkP) const
{
    for (int i = 0; i < N; i++)
    {
        if ( m_afTuple[i] != rkP.m_afTuple[i] )
            return false;
    }
    return true;
}
//----------------------------------------------------------------------------
template <int N, class Real>
bool Point<N,Real>::operator!= (const Point& rkP) const
{
    return !operator==(rkP);
}
//----------------------------------------------------------------------------
template <int N, class Real>
int Point<N,Real>::CompareArrays (const Point& rkP) const
{
    for (int i = 0; i < N; i++)
    {
        unsigned int uiTest0 = *(unsigned int*)&m_afTuple[i];
        unsigned int uiTest1 = *(unsigned int*)&rkP.m_afTuple[i];
        if ( uiTest0 < uiTest1 )
            return -1;
        if ( uiTest0 > uiTest1 )
            return +1;
    }
    return 0;
}
//----------------------------------------------------------------------------
template <int N, class Real>
bool Point<N,Real>::operator<  (const Point& rkP) const
{
    return CompareArrays(rkP) < 0;
}
//----------------------------------------------------------------------------
template <int N, class Real>
bool Point<N,Real>::operator<= (const Point& rkP) const
{
    return CompareArrays(rkP) <= 0;
}
//----------------------------------------------------------------------------
template <int N, class Real>
bool Point<N,Real>::operator>  (const Point& rkP) const
{
    return CompareArrays(rkP) > 0;
}
//----------------------------------------------------------------------------
template <int N, class Real>
bool Point<N,Real>::operator>= (const Point& rkP) const
{
    return CompareArrays(rkP) >= 0;
}
//----------------------------------------------------------------------------
template <int N, class Real>
Point<N,Real> Point<N,Real>::operator+ (const Vector<N,Real>& rkV) const
{
    Point<N,Real> kSum;
    for (int i = 0; i < N; i++)
        kSum.m_afTuple[i] = m_afTuple[i] + rkV[i];
    return kSum;
}
//----------------------------------------------------------------------------
template <int N, class Real>
Vector<N,Real> Point<N,Real>::operator- (const Point& rkP) const
{
    Vector<N,Real> kDiff;
    for (int i = 0; i < N; i++)
        kDiff[i] = m_afTuple[i] - rkP.m_afTuple[i];
    return kDiff;
}
//----------------------------------------------------------------------------
template <int N, class Real>
Point<N,Real> Point<N,Real>::AffineSum (Real fT, const Point& rkP) const
{
    Point<N,Real> kSum;
    for (int i = 0; i < N; i++)
        kSum.m_afTuple[i] = m_afTuple[i]+fT*(rkP.m_afTuple[i]-m_afTuple[i]);
    return kSum;
}
//----------------------------------------------------------------------------
