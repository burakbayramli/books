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
template <int N>
Tuple<N>::Tuple ()
{
    // default constructor does not initialize the tuple
}
//----------------------------------------------------------------------------
template <int N>
Tuple<N>::Tuple (const Tuple& rkT)
{
    memcpy(m_aiTuple,rkT.m_aiTuple,N*sizeof(int));
}
//----------------------------------------------------------------------------
template <int N>
Tuple<N>::operator const int* () const
{
    return m_aiTuple;
}
//----------------------------------------------------------------------------
template <int N>
Tuple<N>::operator int* ()
{
    return m_aiTuple;
}
//----------------------------------------------------------------------------
template <int N>
int Tuple<N>::operator[] (int i) const
{
    assert( 0 <= i && i < N );
    return m_aiTuple[i];
}
//----------------------------------------------------------------------------
template <int N>
int& Tuple<N>::operator[] (int i)
{
    assert( 0 <= i && i < N );
    return m_aiTuple[i];
}
//----------------------------------------------------------------------------
template <int N>
Tuple<N>& Tuple<N>::operator= (const Tuple& rkT)
{
    memcpy(m_aiTuple,rkT.m_aiTuple,N*sizeof(int));
    return *this;
}
//----------------------------------------------------------------------------
