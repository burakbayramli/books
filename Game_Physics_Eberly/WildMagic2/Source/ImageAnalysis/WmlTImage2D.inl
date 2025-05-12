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
template <class T>
TImage2D<T>::TImage2D (int iXBound, int iYBound, T* atData)
    :
    TImage<T>(2)
{
    int* aiBound = new int[2];
    aiBound[0] = iXBound;
    aiBound[1] = iYBound;
    SetBounds(aiBound);
    SetData(atData);
}
//----------------------------------------------------------------------------
template <class T>
TImage2D<T>::TImage2D (const TImage2D& rkImage)
    :
    TImage<T>(rkImage)
{
}
//----------------------------------------------------------------------------
template <class T>
TImage2D<T>::TImage2D (const char* acFilename)
    :
    TImage<T>(acFilename)
{
}
//----------------------------------------------------------------------------
template <class T>
T& TImage2D<T>::operator() (int iX, int iY) const
{
    // assert:  x < bound[0] && y < bound[1]
    return m_atData[iX + m_aiBound[0]*iY];
}
//----------------------------------------------------------------------------
template <class T>
int TImage2D<T>::GetIndex (int iX, int iY) const
{
    // assert:  x < bound[0] && y < bound[1]
    return iX + m_aiBound[0]*iY;
}
//----------------------------------------------------------------------------
template <class T>
void TImage2D<T>::GetCoordinates (int iIndex, int& riX, int& riY) const
{
    // assert:  iIndex < m_iQuantity
    riX = iIndex % m_aiBound[0];
    riY = iIndex / m_aiBound[0];
}
//----------------------------------------------------------------------------
template <class T>
TImage2D<T>& TImage2D<T>::operator= (const TImage2D& rkImage)
{
    return (TImage2D<T>&) TImage<T>::operator=(rkImage);
}
//----------------------------------------------------------------------------
template <class T>
TImage2D<T>& TImage2D<T>::operator= (T tValue)
{
    return (TImage2D<T>&) TImage<T>::operator=(tValue);
}
//----------------------------------------------------------------------------
