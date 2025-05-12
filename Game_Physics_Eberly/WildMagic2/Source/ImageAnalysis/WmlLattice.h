// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLLATTICE_H
#define WMLLATTICE_H

#include "WmlSystem.h"
#include <fstream>

namespace Wml
{

class WML_ITEM Lattice
{
public:
    // abstract base class for TImage
    virtual ~Lattice ();

    // data access
    int GetDimensions () const;
    const int* GetBounds () const;
    int GetBound (int i) const;
    int GetQuantity () const;
    const int* GetOffsets () const;
    int GetOffset (int i) const;

    // assignment
    Lattice& operator= (const Lattice& rkLattice);

    // comparisons
    bool operator== (const Lattice& rkLattice) const;
    bool operator!= (const Lattice& rkLattice) const;

    // Conversions between n-dim and 1-dim structures.  The coordinate arrays
    // must have the same number of elements as the dimensions of the lattice.
    int GetIndex (const int* aiCoord) const;
    void GetCoordinates (int iIndex, int* aiCoord) const;

    // streaming
    bool Load (std::ifstream& rkIStr);
    bool Save (std::ofstream& rkOStr) const;

    static bool LoadRaw (const char* acFilename, int& riDimensions,
        int*& raiBound, int& riQuantity, int& riRTTI, int& riSizeOf,
        char*& racData);

protected:
    // Construction.  Lattice accepts responsibility for deleting the
    // bound array.
    Lattice (int iDimensions, int* aiBound);
    Lattice (const Lattice& rkLattice);
    Lattice ();

    // For deferred creation of bounds.  Lattice accepts responsibility
    // for deleting the bound array.
    Lattice (int iDimensions);
    void SetBounds (int* aiBound);
    void ComputeQuantityAndOffsets ();

    int m_iDimensions;
    int* m_aiBound;
    int m_iQuantity;
    int* m_aiOffset;

    // streaming
    static const char* ms_acHeader;
};

#include "WmlLattice.inl"

}

#endif
