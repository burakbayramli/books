// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlLattice.h"
using namespace Wml;
using namespace std;

const char* Lattice::ms_acHeader = "Magic Image";

//----------------------------------------------------------------------------
Lattice::Lattice (int iDimensions, int* aiBound)
{
#ifdef _DEBUG
    assert( iDimensions > 0 && aiBound );
    for (int i = 0; i < iDimensions; i++)
    {
        assert( aiBound[i] > 0 );
    }
#endif

    m_iDimensions = iDimensions;
    m_aiBound = aiBound;
    m_aiOffset = new int[iDimensions];

    ComputeQuantityAndOffsets();
}
//----------------------------------------------------------------------------
Lattice::Lattice (const Lattice& rkLattice)
{
    m_iDimensions = rkLattice.m_iDimensions;
    m_iQuantity = rkLattice.m_iQuantity;
    m_aiBound = new int[m_iDimensions];
    m_aiOffset = new int[m_iDimensions];

    int iCopySize = m_iDimensions*sizeof(int);
    memcpy(m_aiBound,rkLattice.m_aiBound,iCopySize);
    memcpy(m_aiOffset,rkLattice.m_aiOffset,iCopySize);
}
//----------------------------------------------------------------------------
Lattice::Lattice (int iDimensions)
{
    assert( iDimensions > 0 );

    m_iDimensions = iDimensions;
    m_aiBound = NULL;
    m_aiOffset = new int[iDimensions];
    memset(m_aiOffset,0,iDimensions*sizeof(int));
    m_iQuantity = 0;
}
//----------------------------------------------------------------------------
Lattice::Lattice ()
{
    m_iDimensions = 0;
    m_aiBound = NULL;
    m_aiOffset = NULL;
    m_iQuantity = 0;
}
//----------------------------------------------------------------------------
Lattice::~Lattice ()
{
    delete[] m_aiBound;
    delete[] m_aiOffset;
}
//----------------------------------------------------------------------------
void Lattice::SetBounds (int* aiBound)
{
#ifdef _DEBUG
    assert( aiBound );
    for (int i = 0; i < m_iDimensions; i++)
    {
        assert( aiBound[i] > 0 );
    }
#endif

    m_aiBound = aiBound;
    ComputeQuantityAndOffsets();
}
//----------------------------------------------------------------------------
void Lattice::ComputeQuantityAndOffsets ()
{
    int i;

    // calculate number of lattice points
    m_iQuantity = 1;
    for (i = 0; i < m_iDimensions; i++)
        m_iQuantity *= m_aiBound[i];

    // calculate offset indices of neighboring lattice points
    m_aiOffset[0] = 1;
    for (i = 1; i < m_iDimensions; i++)
        m_aiOffset[i] = m_aiBound[i-1]*m_aiOffset[i-1];
}
//----------------------------------------------------------------------------
Lattice& Lattice::operator= (const Lattice& rkLattice)
{
    if ( m_iDimensions != rkLattice.m_iDimensions )
    {
        delete[] m_aiBound;
        delete[] m_aiOffset;
        m_iDimensions = rkLattice.m_iDimensions;
        m_iQuantity = rkLattice.m_iQuantity;
        m_aiBound = new int[m_iDimensions];
        m_aiOffset = new int[m_iDimensions];
    }

    int iCopySize = m_iDimensions*sizeof(int);
    memcpy(m_aiBound,rkLattice.m_aiBound,iCopySize);
    memcpy(m_aiOffset,rkLattice.m_aiOffset,iCopySize);

    return *this;
}
//----------------------------------------------------------------------------
bool Lattice::operator== (const Lattice& rkLattice) const
{
    if ( m_iDimensions != rkLattice.m_iDimensions )
        return false;

    int iCompareSize = m_iDimensions*sizeof(int);
    return memcmp(m_aiBound,rkLattice.m_aiBound,iCompareSize) == 0;
}
//----------------------------------------------------------------------------
bool Lattice::operator!= (const Lattice& rkLattice) const
{
    return !operator==(rkLattice);
}
//----------------------------------------------------------------------------
int Lattice::GetIndex (const int* aiCoord) const
{
    // assert:  auiCoord is array of m_iDimensions elements
    int iIndex = aiCoord[0];
    for (int i = 1; i < m_iDimensions; i++)
        iIndex += m_aiOffset[i]*aiCoord[i];
    return iIndex;
}
//----------------------------------------------------------------------------
void Lattice::GetCoordinates (int iIndex, int* aiCoord) const
{
    // assert:  aiCoord is array of m_iDimensions elements
    for (int i = 0; i < m_iDimensions; i++)
    {
        aiCoord[i] = iIndex % m_aiBound[i];
        iIndex /= m_aiBound[i];
    }
}
//----------------------------------------------------------------------------
bool Lattice::Load (ifstream& rkIStr)
{
    // Header is an ASCII string of 'char'.  No need to swap bytes on big
    // endian platforms.
    int iSize = (int)strlen(ms_acHeader) + 1;
    char* acBuffer = new char[iSize];
    rkIStr.read(acBuffer,iSize);
    acBuffer[iSize-1] = 0;
    if ( strncmp(acBuffer,ms_acHeader,iSize) != 0 )
    {
        delete[] acBuffer;
        m_iDimensions = 0;
        m_iQuantity = 0;
        m_aiBound = NULL;
        m_aiOffset = NULL;
        return false;
    }
    delete[] acBuffer;

    rkIStr.read((char*)&m_iDimensions,sizeof(int));
#ifdef WML_BIG_ENDIAN
    System::SwapBytes(sizeof(int),&m_iDimensions);
#endif

    delete[] m_aiBound;
    m_aiBound = new int[m_iDimensions];
    rkIStr.read((char*)m_aiBound,m_iDimensions*sizeof(int));
#ifdef WML_BIG_ENDIAN
    System::SwapBytes(sizeof(int),m_iDimensions,m_aiBound);
#endif

    delete[] m_aiOffset;
    m_aiOffset = new int[m_iDimensions];

    ComputeQuantityAndOffsets();

    return true;
}
//----------------------------------------------------------------------------
bool Lattice::Save (ofstream& rkOStr) const
{
    // Header is an ASCII string of 'char'.  No need to swap bytes on big
    // endian platforms.
    rkOStr.write(ms_acHeader,(int)strlen(ms_acHeader)+1);

#ifdef WML_BIG_ENDIAN
    int iDummy = m_iDimensions;
    System::SwapBytes(sizeof(int),&iDummy);
    rkOStr.write((const char*)&iDummy,sizeof(int));
#else
    rkOStr.write((const char*)&m_iDimensions,sizeof(int));
#endif

#ifdef WML_BIG_ENDIAN
    for (int i = 0; i < m_iDimensions; i++)
    {
        iDummy = m_aiBound[i];
        System::SwapBytes(sizeof(int),&iDummy);
        rkOStr.write((const char*)&iDummy,sizeof(int));
    }
#else
    rkOStr.write((const char*)m_aiBound,m_iDimensions*sizeof(int));
#endif

    return true;
}
//----------------------------------------------------------------------------
bool Lattice::LoadRaw (const char* acFilename, int& riDimensions,
    int*& raiBound, int& riQuantity, int& riRTTI, int& riSizeOf,
    char*& racData)
{
    ifstream kIStr(acFilename,ios::in|ios::binary);
    if ( !kIStr )
        return false;

    // Header is an ASCII string of 'char'.  No need to swap bytes on big
    // endian platforms.
    int iSize = (int)strlen(ms_acHeader) + 1;
    char* acBuffer = new char[iSize];
    kIStr.read(acBuffer,iSize);
    acBuffer[iSize-1] = 0;
    if ( strncmp(acBuffer,ms_acHeader,iSize) != 0 )
    {
        delete[] acBuffer;
        return false;
    }
    delete[] acBuffer;

    kIStr.read((char*)&riDimensions,sizeof(int));
#ifdef WML_BIG_ENDIAN
    System::SwapBytes(sizeof(int),&riDimensions);
#endif

    raiBound = new int[riDimensions];
    kIStr.read((char*)raiBound,riDimensions*sizeof(int));
#ifdef WML_BIG_ENDIAN
    System::SwapBytes(sizeof(int),riDimensions,raiBound);
#endif

    riQuantity = 1;
    for (int i = 0; i < riDimensions; i++)
        riQuantity *= raiBound[i];

    kIStr.read((char*)&riRTTI,sizeof(int));
#ifdef WML_BIG_ENDIAN
    System::SwapBytes(sizeof(int),&riRTTI);
#endif

    kIStr.read((char*)&riSizeOf,sizeof(int));
#ifdef WML_BIG_ENDIAN
    System::SwapBytes(sizeof(int),&riSizeOf);
#endif

    racData = new char[riQuantity*riSizeOf];
    kIStr.read(racData,riQuantity*riSizeOf);
#ifdef WML_BIG_ENDIAN
    System::SwapBytes(riSizeOf,riQuantity,racData);
#endif

    return true;
}
//----------------------------------------------------------------------------
