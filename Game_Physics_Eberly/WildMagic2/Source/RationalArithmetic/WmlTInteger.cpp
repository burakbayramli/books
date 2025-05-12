// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlTInteger.h"
namespace Wml
{
#include "WmlTInteger.inl"
}
using namespace Wml;

//----------------------------------------------------------------------------
// explicit instantiation (N = 2, 4, 8, 16, 32, 64, 128, 256)
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM TInteger<2>;
#ifdef WML_USING_VC6
template WML_ITEM TInteger<2> operator* (int, const TInteger<2>&);
#else
template WML_ITEM TInteger<2> operator*<2> (int, const TInteger<2>&);
#endif

template class WML_ITEM TInteger<4>;
#ifdef WML_USING_VC6
template WML_ITEM TInteger<4> operator* (int, const TInteger<4>&);
#else
template WML_ITEM TInteger<4> operator*<4> (int, const TInteger<4>&);
#endif

template class WML_ITEM TInteger<8>;
#ifdef WML_USING_VC6
template WML_ITEM TInteger<8> operator* (int, const TInteger<8>&);
#else
template WML_ITEM TInteger<8> operator*<8> (int, const TInteger<8>&);
#endif

template class WML_ITEM TInteger<16>;
#ifdef WML_USING_VC6
template WML_ITEM TInteger<16> operator* (int, const TInteger<16>&);
#else
template WML_ITEM TInteger<16> operator*<16> (int, const TInteger<16>&);
#endif

template class WML_ITEM TInteger<32>;
#ifdef WML_USING_VC6
template WML_ITEM TInteger<32> operator* (int, const TInteger<32>&);
#else
template WML_ITEM TInteger<32> operator*<32> (int, const TInteger<32>&);
#endif

template class WML_ITEM TInteger<64>;
#ifdef WML_USING_VC6
template WML_ITEM TInteger<64> operator* (int, const TInteger<64>&);
#else
template WML_ITEM TInteger<64> operator*<64> (int, const TInteger<64>&);
#endif

template class WML_ITEM TInteger<128>;
#ifdef WML_USING_VC6
template WML_ITEM TInteger<128> operator* (int, const TInteger<128>&);
#else
template WML_ITEM TInteger<128> operator*<128> (int, const TInteger<128>&);
#endif

template class WML_ITEM TInteger<256>;
#ifdef WML_USING_VC6
template WML_ITEM TInteger<256> operator* (int, const TInteger<256>&);
#else
template WML_ITEM TInteger<256> operator*<256> (int, const TInteger<256>&);
#endif
}
//----------------------------------------------------------------------------
