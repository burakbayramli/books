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
inline Version Stream::GetVersion () const
{
    return m_kVersion;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// buffer for streaming
//----------------------------------------------------------------------------
inline int Stream::BufferSize () const
{
    return m_iBufferSize;
}
//----------------------------------------------------------------------------
inline int& Stream::BufferNext ()
{
    return m_iBufferNext;
}
//----------------------------------------------------------------------------
inline char* Stream::Buffer ()
{
    return m_acBuffer;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// bytes used for disk storage
//----------------------------------------------------------------------------
template <class T>
int StreamBytesEnum (T)
{
    // number of bytes written by 'StreamWriteEnum (T)'
    return 4;
}
//----------------------------------------------------------------------------
inline int StreamBytesBool (bool)
{
    // number of bytes written by 'StreamWriteBool (bool)'
    return 1;
}
//----------------------------------------------------------------------------
inline int StreamBytesString (const char* acString)
{
    // number of bytes written by 'Stream::Write'
    return sizeof(int) + ( acString ? (int)strlen(acString) : 0 );
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// Byte swapping templates with specialization for Bound, ColorRGB, Matrix2f,
// Matrix3f, Plane, Quaternionf, Vector2f, and Vector3f.
//----------------------------------------------------------------------------
template <class T>
inline void StreamSwapBytes (T* ptData)
{
    char* acBytes = (char*) ptData;
    int iSize = (int)(sizeof(T));
    int iSizeM1 = iSize - 1;
    int iHSize = iSize/2;
    for (int i0 = 0, i1 = iSizeM1; i0 < iHSize; i0++, i1--)
    {
        char cSave = acBytes[i0];
        acBytes[i0] = acBytes[i1];
        acBytes[i1] = cSave;
    }
}
//----------------------------------------------------------------------------
template <class T>
inline void StreamSwapBytes (int iQuantity, T* atData)
{
    char* acBytes = (char*) atData;
    int iSize = (int)(sizeof(T));
    int iSizeM1 = iSize - 1;
    int iHSize = iSize/2;
    for (int i = 0; i < iQuantity; i++, acBytes += iSize)
    {
        for (int i0 = 0, i1 = iSizeM1; i0 < iHSize; i0++, i1--)
        {
            char cSave = acBytes[i0];
            acBytes[i0] = acBytes[i1];
            acBytes[i1] = cSave;
        }
    }
}
//----------------------------------------------------------------------------
template <>
inline void StreamSwapBytes<Bound> (Bound* pkBound)
{
    StreamSwapBytes(4,(float*)pkBound);
}
//----------------------------------------------------------------------------
template <>
inline void StreamSwapBytes<Bound> (int iQuantity, Bound* akBound)
{
    StreamSwapBytes(4*iQuantity,(float*)akBound);
}
//----------------------------------------------------------------------------
template <>
inline void StreamSwapBytes<ColorRGB> (ColorRGB* pkColor)
{
    StreamSwapBytes(3,(float*)pkColor);
}
//----------------------------------------------------------------------------
template <>
inline void StreamSwapBytes<ColorRGB> (int iQuantity, ColorRGB* pkColor)
{
    StreamSwapBytes(3*iQuantity,(float*)pkColor);
}
//----------------------------------------------------------------------------
template <>
inline void StreamSwapBytes<Matrix2f> (Matrix2f* pkMat)
{
    StreamSwapBytes(4,(float*)pkMat);
}
//----------------------------------------------------------------------------
template <>
inline void StreamSwapBytes<Matrix2f> (int iQuantity, Matrix2f* akMat)
{
    StreamSwapBytes(4*iQuantity,(float*)akMat);
}
//----------------------------------------------------------------------------
template <>
inline void StreamSwapBytes<Matrix3f> (Matrix3f* pkMat)
{
    StreamSwapBytes(9,(float*)pkMat);
}
//----------------------------------------------------------------------------
template <>
inline void StreamSwapBytes<Matrix3f> (int iQuantity, Matrix3f* akMat)
{
    StreamSwapBytes(9*iQuantity,(float*)akMat);
}
//----------------------------------------------------------------------------
template <>
inline void StreamSwapBytes<Plane3f> (Plane3f* pkPlane)
{
    StreamSwapBytes(4,(float*)pkPlane);
}
//----------------------------------------------------------------------------
template <>
inline void StreamSwapBytes<Plane3f> (int iQuantity, Plane3f* pkPlane)
{
    StreamSwapBytes(4*iQuantity,(float*)pkPlane);
}
//----------------------------------------------------------------------------
template <>
inline void StreamSwapBytes<Vector2f> (Vector2f* pkVec)
{
    StreamSwapBytes(2,(float*)pkVec);
}
//----------------------------------------------------------------------------
template <>
inline void StreamSwapBytes<Vector2f> (int iQuantity, Vector2f* pkVec)
{
    StreamSwapBytes(2*iQuantity,(float*)pkVec);
}
//----------------------------------------------------------------------------
template <>
inline void StreamSwapBytes<Vector3f> (Vector3f* pkVec)
{
    StreamSwapBytes(3,(float*)pkVec);
}
//----------------------------------------------------------------------------
template <>
inline void StreamSwapBytes<Vector3f> (int iQuantity, Vector3f* pkVec)
{
    StreamSwapBytes(3*iQuantity,(float*)pkVec);
}
//----------------------------------------------------------------------------
template <>
inline void StreamSwapBytes<Quaternionf> (Quaternionf* pkQuat)
{
    StreamSwapBytes(4,(float*)pkQuat);
}
//----------------------------------------------------------------------------
template <>
inline void StreamSwapBytes<Quaternionf> (int iQuantity, Quaternionf* pkQuat)
{
    StreamSwapBytes(4*iQuantity,(float*)pkQuat);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// read/write templates
//----------------------------------------------------------------------------
template <class T>
void StreamRead (Stream& rkStream, T& rtValue)
{
    int iRead = (int)(sizeof(T));
    assert( rkStream.BufferNext() + iRead <= rkStream.BufferSize() );

    T* ptSrc = (T*)(rkStream.Buffer() + rkStream.BufferNext());

#ifdef WML_BIG_ENDIAN
    StreamSwapBytes(ptSrc);
#endif

    memcpy(&rtValue,ptSrc,iRead);
    rkStream.BufferNext() += iRead;
}
//----------------------------------------------------------------------------
template <class T>
void StreamRead (Stream& rkStream, T* atValue, int iQuantity)
{
    int iRead = sizeof(T)*iQuantity;
    assert( rkStream.BufferNext() + iRead <= rkStream.BufferSize() );
    T* atSrc = (T*)(rkStream.Buffer() + rkStream.BufferNext());

#ifdef WML_BIG_ENDIAN
    StreamSwapBytes(iQuantity,atSrc);
#endif

    memcpy(atValue,atSrc,iRead);
    rkStream.BufferNext() += iRead;
}
//----------------------------------------------------------------------------
template <class T>
void StreamWrite (Stream& rkStream, const T& rtValue)
{
    int iWrite = (int)(sizeof(T));
    assert( rkStream.BufferNext() + iWrite <= rkStream.BufferSize() );

    T* ptDest = (T*)(rkStream.Buffer() + rkStream.BufferNext());
    memcpy(ptDest,&rtValue,iWrite);
    rkStream.BufferNext() += iWrite;

#ifdef WML_BIG_ENDIAN
    StreamSwapBytes(ptDest);
#endif
}
//----------------------------------------------------------------------------
template <class T>
void StreamWrite (Stream& rkStream, const T* atValue, int iQuantity)
{
    int iWrite = sizeof(T)*iQuantity;
    assert( rkStream.BufferNext() + iWrite <= rkStream.BufferSize() );

    T* atDest = (T*)(rkStream.Buffer() + rkStream.BufferNext());
    memcpy(atDest,atValue,iWrite);
    rkStream.BufferNext() += iWrite;

#ifdef WML_BIG_ENDIAN
    StreamSwapBytes(iQuantity,atDest);
#endif
}
//----------------------------------------------------------------------------
template <class T>
void StreamReadEnum (Stream& rkStream, T& rtValue)
{
    // enums are packed into 4-byte quantities (portability issue)
    int iSize = (int)(sizeof(T));
    assert( iSize == 1 || iSize == 2 || iSize == 4 );
    assert( rkStream.BufferNext() + 4 <= rkStream.BufferSize() );

    T* ptSrc = (T*)(rkStream.Buffer() + rkStream.BufferNext());

#ifdef WML_BIG_ENDIAN
    StreamSwapBytes(ptSrc);
#endif

    // copy and discard 4-byte padding
    memcpy(&rtValue,ptSrc,iSize);
    rkStream.BufferNext() += 4;
}
//----------------------------------------------------------------------------
template <class T>
void StreamWriteEnum (Stream& rkStream, T tValue)
{
    // enums are packed into 4-byte quantities (portability issue)
    int iSize = (int)(sizeof(T));
    assert( iSize == 1 || iSize == 2 || iSize == 4 );
    assert( rkStream.BufferNext() + 4 <= rkStream.BufferSize() );

    T* ptDest = (T*)(rkStream.Buffer() + rkStream.BufferNext());
    memcpy(ptDest,&tValue,iSize);
    rkStream.BufferNext() += iSize;

#ifdef WML_BIG_ENDIAN
    StreamSwapBytes(ptDest);
#endif

    // pad to a 4-byte boundary
    int iWrite = 4 - iSize;
    if ( iWrite > 0 )
    {
        memset(rkStream.Buffer()+rkStream.BufferNext(),0,iWrite);
        rkStream.BufferNext() += iWrite;
    }
}
//----------------------------------------------------------------------------
inline void StreamReadBool (Stream& rkStream, bool& rbValue)
{
    assert( rkStream.BufferNext() + 1 <= rkStream.BufferSize() );

    unsigned char ucValue;
    char* pcSrc = rkStream.Buffer() + rkStream.BufferNext();
    memcpy(&ucValue,pcSrc,1);
    rkStream.BufferNext()++;
    rbValue = ( ucValue > 0 ? true : false );
}
//----------------------------------------------------------------------------
inline void StreamReadBool (Stream& rkStream, bool* abValue, int iQuantity)
{
    assert( rkStream.BufferNext() + iQuantity <= rkStream.BufferSize() );

    char* pcSrc = rkStream.Buffer() + rkStream.BufferNext();
    for (int i = 0; i < iQuantity; i++)
    {
        unsigned char ucValue;
        memcpy(&ucValue,pcSrc,1);
        rkStream.BufferNext()++;
        pcSrc++;
        abValue[i] = ( ucValue > 0 ? true : false );
    }
}
//----------------------------------------------------------------------------
inline void StreamWriteBool (Stream& rkStream, bool bValue)
{
    assert( rkStream.BufferNext() + 1 <= rkStream.BufferSize() );

    unsigned char ucValue = ( bValue ? 1 : 0 );
    char* pcDest = rkStream.Buffer() + rkStream.BufferNext();
    memcpy(pcDest,&ucValue,1);
    rkStream.BufferNext()++;
}
//----------------------------------------------------------------------------
inline void StreamWriteBool (Stream& rkStream, bool* abValue, int iQuantity)
{
    assert( rkStream.BufferNext() + iQuantity <= rkStream.BufferSize() );

    char* pcSrc = rkStream.Buffer() + rkStream.BufferNext();
    for (int i = 0; i < iQuantity; i++)
    {
        unsigned char ucValue = ( abValue[i] ? 1 : 0 );
        memcpy(pcSrc,&ucValue,1);
        rkStream.BufferNext()++;
        pcSrc++;
    }
}
//----------------------------------------------------------------------------


