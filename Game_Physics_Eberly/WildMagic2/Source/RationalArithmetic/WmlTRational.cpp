// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlTRational.h"
namespace Wml
{
#include "WmlTRational.inl"
}
using namespace Wml;

//----------------------------------------------------------------------------
// explicit instantiation (N = 2, 4, 8, 16, 32, 64, 128, 256)
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM TRational<2>;
#ifdef WML_USING_VC6
template WML_ITEM TRational<2> operator+ (const TInteger<2>&,
    const TRational<2>&);
template WML_ITEM TRational<2> operator- (const TInteger<2>&,
    const TRational<2>&);
template WML_ITEM TRational<2> operator* (const TInteger<2>&,
    const TRational<2>&);
template WML_ITEM TRational<2> operator/ (const TInteger<2>&,
    const TRational<2>&);
#else
template WML_ITEM TRational<2> operator+<2> (const TInteger<2>&,
    const TRational<2>&);
template WML_ITEM TRational<2> operator-<2> (const TInteger<2>&,
    const TRational<2>&);
template WML_ITEM TRational<2> operator*<2> (const TInteger<2>&,
    const TRational<2>&);
template WML_ITEM TRational<2> operator/<2> (const TInteger<2>&,
    const TRational<2>&);
#endif

template class WML_ITEM TRational<4>;
#ifdef WML_USING_VC6
template WML_ITEM TRational<4> operator+ (const TInteger<4>&,
    const TRational<4>&);
template WML_ITEM TRational<4> operator- (const TInteger<4>&,
    const TRational<4>&);
template WML_ITEM TRational<4> operator* (const TInteger<4>&,
    const TRational<4>&);
template WML_ITEM TRational<4> operator/ (const TInteger<4>&,
    const TRational<4>&);
#else
template WML_ITEM TRational<4> operator+<4> (const TInteger<4>&,
    const TRational<4>&);
template WML_ITEM TRational<4> operator-<4> (const TInteger<4>&,
    const TRational<4>&);
template WML_ITEM TRational<4> operator*<4> (const TInteger<4>&,
    const TRational<4>&);
template WML_ITEM TRational<4> operator/<4> (const TInteger<4>&,
    const TRational<4>&);
#endif

template class WML_ITEM TRational<8>;
#ifdef WML_USING_VC6
template WML_ITEM TRational<8> operator+ (const TInteger<8>&,
    const TRational<8>&);
template WML_ITEM TRational<8> operator- (const TInteger<8>&,
    const TRational<8>&);
template WML_ITEM TRational<8> operator* (const TInteger<8>&,
    const TRational<8>&);
template WML_ITEM TRational<8> operator/ (const TInteger<8>&,
    const TRational<8>&);
#else
template WML_ITEM TRational<8> operator+<8> (const TInteger<8>&,
    const TRational<8>&);
template WML_ITEM TRational<8> operator-<8> (const TInteger<8>&,
    const TRational<8>&);
template WML_ITEM TRational<8> operator*<8> (const TInteger<8>&,
    const TRational<8>&);
template WML_ITEM TRational<8> operator/<8> (const TInteger<8>&,
    const TRational<8>&);
#endif

template class WML_ITEM TRational<16>;
#ifdef WML_USING_VC6
template WML_ITEM TRational<16> operator+ (const TInteger<16>&,
    const TRational<16>&);
template WML_ITEM TRational<16> operator- (const TInteger<16>&,
    const TRational<16>&);
template WML_ITEM TRational<16> operator* (const TInteger<16>&,
    const TRational<16>&);
template WML_ITEM TRational<16> operator/ (const TInteger<16>&,
    const TRational<16>&);
#else
template WML_ITEM TRational<16> operator+<16> (const TInteger<16>&,
    const TRational<16>&);
template WML_ITEM TRational<16> operator-<16> (const TInteger<16>&,
    const TRational<16>&);
template WML_ITEM TRational<16> operator*<16> (const TInteger<16>&,
    const TRational<16>&);
template WML_ITEM TRational<16> operator/<16> (const TInteger<16>&,
    const TRational<16>&);
#endif

template class WML_ITEM TRational<32>;
#ifdef WML_USING_VC6
template WML_ITEM TRational<32> operator+ (const TInteger<32>&,
    const TRational<32>&);
template WML_ITEM TRational<32> operator- (const TInteger<32>&,
    const TRational<32>&);
template WML_ITEM TRational<32> operator* (const TInteger<32>&,
    const TRational<32>&);
template WML_ITEM TRational<32> operator/ (const TInteger<32>&,
    const TRational<32>&);
#else
template WML_ITEM TRational<32> operator+<32> (const TInteger<32>&,
    const TRational<32>&);
template WML_ITEM TRational<32> operator-<32> (const TInteger<32>&,
    const TRational<32>&);
template WML_ITEM TRational<32> operator*<32> (const TInteger<32>&,
    const TRational<32>&);
template WML_ITEM TRational<32> operator/<32> (const TInteger<32>&,
    const TRational<32>&);
#endif

template class WML_ITEM TRational<64>;
#ifdef WML_USING_VC6
template WML_ITEM TRational<64> operator+ (const TInteger<64>&,
    const TRational<64>&);
template WML_ITEM TRational<64> operator- (const TInteger<64>&,
    const TRational<64>&);
template WML_ITEM TRational<64> operator* (const TInteger<64>&,
    const TRational<64>&);
template WML_ITEM TRational<64> operator/ (const TInteger<64>&,
    const TRational<64>&);
#else
template WML_ITEM TRational<64> operator+<64> (const TInteger<64>&,
    const TRational<64>&);
template WML_ITEM TRational<64> operator-<64> (const TInteger<64>&,
    const TRational<64>&);
template WML_ITEM TRational<64> operator*<64> (const TInteger<64>&,
    const TRational<64>&);
template WML_ITEM TRational<64> operator/<64> (const TInteger<64>&,
    const TRational<64>&);
#endif

template class WML_ITEM TRational<128>;
#ifdef WML_USING_VC6
template WML_ITEM TRational<128> operator+ (const TInteger<128>&,
    const TRational<128>&);
template WML_ITEM TRational<128> operator- (const TInteger<128>&,
    const TRational<128>&);
template WML_ITEM TRational<128> operator* (const TInteger<128>&,
    const TRational<128>&);
template WML_ITEM TRational<128> operator/ (const TInteger<128>&,
    const TRational<128>&);
#else
template WML_ITEM TRational<128> operator+<128> (const TInteger<128>&,
    const TRational<128>&);
template WML_ITEM TRational<128> operator-<128> (const TInteger<128>&,
    const TRational<128>&);
template WML_ITEM TRational<128> operator*<128> (const TInteger<128>&,
    const TRational<128>&);
template WML_ITEM TRational<128> operator/<128> (const TInteger<128>&,
    const TRational<128>&);
#endif

template class WML_ITEM TRational<256>;
#ifdef WML_USING_VC6
template WML_ITEM TRational<256> operator+ (const TInteger<256>&,
    const TRational<256>&);
template WML_ITEM TRational<256> operator- (const TInteger<256>&,
    const TRational<256>&);
template WML_ITEM TRational<256> operator* (const TInteger<256>&,
    const TRational<256>&);
template WML_ITEM TRational<256> operator/ (const TInteger<256>&,
    const TRational<256>&);
#else
template WML_ITEM TRational<256> operator+<256> (const TInteger<256>&,
    const TRational<256>&);
template WML_ITEM TRational<256> operator-<256> (const TInteger<256>&,
    const TRational<256>&);
template WML_ITEM TRational<256> operator*<256> (const TInteger<256>&,
    const TRational<256>&);
template WML_ITEM TRational<256> operator/<256> (const TInteger<256>&,
    const TRational<256>&);
#endif
}
//----------------------------------------------------------------------------
