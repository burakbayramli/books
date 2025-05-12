// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLTRVECTOR_H
#define WMLTRVECTOR_H

#include "WmlTRational.h"

namespace Wml
{

template <int VSIZE, int ISIZE>
class WML_ITEM TRVector
{
public:
    // construction
    TRVector ();
    TRVector (const TRVector& rkV);

    // coordinate access
    operator const TRational<ISIZE>* () const;
    operator TRational<ISIZE>* ();
    TRational<ISIZE> operator[] (int i) const;
    TRational<ISIZE>& operator[] (int i);

    // assignment
    TRVector& operator= (const TRVector& rkV);

    // comparison
    bool operator== (const TRVector& rkV) const;
    bool operator!= (const TRVector& rkV) const;
    bool operator<  (const TRVector& rkV) const;
    bool operator<= (const TRVector& rkV) const;
    bool operator>  (const TRVector& rkV) const;
    bool operator>= (const TRVector& rkV) const;

    // arithmetic operations
    TRVector operator+ (const TRVector& rkV) const;
    TRVector operator- (const TRVector& rkV) const;
    TRVector operator* (const TRational<ISIZE>& rkR) const;
    TRVector operator/ (const TRational<ISIZE>& rkR) const;
    TRVector operator- () const;

    // TO DO:  The body is inline here because when placed in the *.inl
    // file, the linker complains it cannot find the function
    // "Wml::operator*".  This same problem does not occur with the TVector
    // class, but that has "template <int N, class Real>" whereas this class
    // has "template <int VSIZE, int ISIZE>".  Maybe the presence of the class
    // parameter is the difference.
    WML_ITEM friend TRVector operator* (const TRational<ISIZE>& rkR,
        const TRVector& rkV)
    {
        TRVector<VSIZE,ISIZE> kProd;
        for (int i = 0; i < VSIZE; i++)
            kProd.m_akTuple[i] = rkR*rkV.m_akTuple[i];
        return kProd;
    }

    // arithmetic updates
    TRVector& operator+= (const TRVector& rkV);
    TRVector& operator-= (const TRVector& rkV);
    TRVector& operator*= (const TRational<ISIZE>& rkR);
    TRVector& operator/= (const TRational<ISIZE>& rkR);

    // vector operations
    TRational<ISIZE> SquaredLength () const;
    TRational<ISIZE> Dot (const TRVector& rkV) const;

protected:
    // support for comparisons
    int CompareArrays (const TRVector& rkV) const;

    TRational<ISIZE> m_akTuple[VSIZE];
};

}

#endif
