// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLUNORDEREDSET_H
#define WMLUNORDEREDSET_H

// An unordered set of objects stored in contiguous memory.  The type T must
// have the following member functions:
//   T::T();
//   T::~T();
//   T& operator= (const T&);
//   bool operator== (const T&) const;

#include "WmlSystem.h"

namespace Wml
{

template <class T>
class UnorderedSet
{
public:
    UnorderedSet (int iMaxQuantity = 0, int iGrow = 0);
    UnorderedSet (const UnorderedSet& rkSet);
    ~UnorderedSet ();

    void Reset (int iMaxQuantity = 0, int iGrow = 0);
    void Clear ();
    UnorderedSet& operator= (const UnorderedSet& rkSet);

    int GetMaxQuantity () const;
    int GetGrow () const;

    int GetQuantity () const;
    const T& Get (int i) const;
    T& operator[] (int i);

    bool Exists (const T& rtElement) const;
    bool Insert (const T& rtElement);
    int Append (const T& rtElement);
    bool Remove (const T& rtElement, int* piOld = NULL, int* piNew = NULL);
    bool RemoveAt (int i, int* piOld = NULL, int* piNew = NULL);
    void Compactify ();

    enum { DEFAULT_GROW = 8 };

protected:
    int m_iQuantity, m_iMaxQuantity, m_iGrow;
    T* m_atElement;
};

#include "WmlUnorderedSet.inl"

}

#endif
