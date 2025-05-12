// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLTINTEGER_H
#define WMLTINTEGER_H

#include "WmlSystem.h"

namespace Wml
{

template <int N> class TRational;

// N is the number of 32-bit words you want per TInteger.
template <int N>
class WML_ITEM TInteger
{
public:
    // construction and destruction
    TInteger (int i = 0);
    TInteger (const TInteger& rkI);
    ~TInteger ();

    // assignment
    TInteger& operator= (const TInteger& rkI);

    // comparison
    bool operator== (const TInteger& rkI) const;
    bool operator!= (const TInteger& rkI) const;
    bool operator<  (const TInteger& rkI) const;
    bool operator<= (const TInteger& rkI) const;
    bool operator>  (const TInteger& rkI) const;
    bool operator>= (const TInteger& rkI) const;

    // arithmetic operations
    TInteger operator- () const;
    TInteger operator+ (const TInteger& rkI) const;
    TInteger operator- (const TInteger& rkI) const;
    TInteger operator* (const TInteger& rkI) const;
    TInteger operator/ (const TInteger& rkI) const;
    TInteger operator% (const TInteger& rkI) const;

    // arithmetic updates
    TInteger& operator+= (const TInteger& rkI);
    TInteger& operator-= (const TInteger& rkI);
    TInteger& operator*= (const TInteger& rkI);
    TInteger& operator/= (const TInteger& rkI);

    // shift operations
    TInteger operator<< (int iShift) const;
    TInteger operator>> (int iShift) const;

    // shift updates
    TInteger& operator<<= (int iShift);
    TInteger& operator>>= (int iShift);

private:
    // Support for comparisons.  The return value of Compare is -1 if I0 < I1,
    // is 0 if I0 == I1, or is +1 if I0 > I1.
    static int Compare (const TInteger& rkI0, const TInteger& rkI1);
    int GetSign () const;

    // support for division and modulo
    static bool GetDivMod (const TInteger& rkNumer, const TInteger& rkDenom,
        TInteger& rkQuotient, TInteger& rkRemainder);

    static void DivSingle (const TInteger& rkNumer, short usDenom,
        TInteger& rkQuo, TInteger& rkRem);

    static void DivMultiple (const TInteger& rkNumer, const TInteger& rkDenom,
        TInteger& rkQuo, TInteger& rkRem);

    // miscellaneous utilities
    int GetLeadingBlock () const;
    int GetTrailingBlock () const;
    int GetLeadingBit (int i) const;  // of m_asBuffer[i]
    int GetTrailingBit (int i) const;  // of m_asBuffer[i]
    int GetLeadingBit () const;  // of entire number
    int GetTrailingBit () const;  // of entire number
    void SetBit (int i, bool bOn);
    bool GetBit (int i) const;
    unsigned int ToUnsignedInt (int i) const;
    void FromUnsignedInt (int i, unsigned int uiValue);
    unsigned int ToUnsignedInt (int iLo, int iHi) const;
    int ToInt (int i) const;

    enum
    {
        TINT_SIZE = 2*N,
        TINT_BYTES = TINT_SIZE*sizeof(short),
        TINT_LAST = TINT_SIZE-1
    };

    short m_asBuffer[TINT_SIZE];

    // TRational needs access to private members of TInteger.
    friend class TRational<N>;
};

template <int N>
WML_ITEM TInteger<N> operator* (int i, const TInteger<N>& rkI);

}

#endif
