// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLSMARTPOINTER_H
#define WMLSMARTPOINTER_H

#include "WmlSmartPointer.mcr"

namespace Wml
{

template <class T>
class Pointer
{
public:
    // construction and destruction
    Pointer (T* pkObject = 0);
    Pointer (const Pointer& rkPointer);
    ~Pointer ();

    // implicit conversions
    operator T* () const;
    T& operator* () const;
    T* operator-> () const;

    // assignment
    Pointer& operator= (const Pointer& rkReference);
    Pointer& operator= (T* pkObject);

    // comparisons
    bool operator== (T* pkObject) const;
    bool operator!= (T* pkObject) const;
    bool operator== (const Pointer& rkReference) const;
    bool operator!= (const Pointer& rkReference) const;

protected:
    // the shared object
    T* m_pkObject;
};

#include "WmlSmartPointer.inl"

}

#endif



