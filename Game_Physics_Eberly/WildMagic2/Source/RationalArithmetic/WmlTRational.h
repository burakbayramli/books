// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLTRATIONAL_H
#define WMLTRATIONAL_H

#include "WmlTInteger.h"

namespace Wml
{

// N is the number of 32-bit words per TInteger numerator/denominator
template <int N>
class WML_ITEM TRational
{
public:
    // construction
    TRational ();  // default rational is 0/1
    TRational (const TRational& rkR);
    TRational (const TInteger<N>& rkNumer);
    TRational (const TInteger<N>& rkNumer, const TInteger<N>& rkDenom);

    // construction converters
    TRational (int iNumer);
    TRational (int iNumer, int iDenom);
    TRational (float fValue);
    TRational (double dValue);

    // member access
    TInteger<N>& Numer ();
    TInteger<N>& Denom ();
    const TInteger<N>& Numer () const;
    const TInteger<N>& Denom () const;

    // assignment
    TRational& operator= (const TRational& rkR);

    // comparison
    bool operator== (const TRational& rkR) const;
    bool operator!= (const TRational& rkR) const;
    bool operator<= (const TRational& rkR) const;
    bool operator<  (const TRational& rkR) const;
    bool operator>= (const TRational& rkR) const;
    bool operator>  (const TRational& rkR) const;

    // arithmetic operations
    TRational operator+ (const TRational& rkR) const;
    TRational operator- (const TRational& rkR) const;
    TRational operator* (const TRational& rkR) const;
    TRational operator/ (const TRational& rkR) const;
    TRational operator- () const;

    // arithmetic updates
    TRational& operator+= (const TRational& rkR);
    TRational& operator-= (const TRational& rkR);
    TRational& operator*= (const TRational& rkR);
    TRational& operator/= (const TRational& rkR);

    // conversions to float and double (approximately)
    float ToFloat () const;
    double ToDouble () const;

    // compute the absolute value of the rational number
    TRational Abs () const;

private:
    // miscellaneous utilities
    void EliminatePowersOfTwo ();

    static void GetPositiveFloat (const TInteger<N>& rkDenom,
        TInteger<N>& rkQuo, TInteger<N>& rkRem, int iBlock,
        unsigned int& ruiExponent, unsigned int& ruiMantissa);

    static void GetPositiveDouble (const TInteger<N>& rkDenom,
        TInteger<N>& rkQuo, TInteger<N>& rkRem, int iBlock,
        unsigned int& ruiExponent, unsigned int& ruiMantissaHi,
        unsigned int& ruiMantissaLo);

    TInteger<N> m_kNumer, m_kDenom;
};

template <int N>
WML_ITEM TRational<N> operator+ (const TInteger<N>& rkI,
    const TRational<N>& rkR);

template <int N>
WML_ITEM TRational<N> operator- (const TInteger<N>& rkI,
    const TRational<N>& rkR);

template <int N>
WML_ITEM TRational<N> operator* (const TInteger<N>& rkI,
    const TRational<N>& rkR);

template <int N>
WML_ITEM TRational<N> operator/ (const TInteger<N>& rkI,
    const TRational<N>& rkR);

}

#endif
