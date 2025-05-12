// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlRVector2.h"
namespace Wml
{
#include "WmlTRVector.inl"
#include "WmlRVector2.inl"
}
using namespace Wml;

//----------------------------------------------------------------------------
// explicit instantiation (N = 2, 4, 8, 16, 32, 64, 128, 256)
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM TRVector<2,2>;
template class WML_ITEM TRVector<2,4>;
template class WML_ITEM TRVector<2,8>;
template class WML_ITEM TRVector<2,16>;
template class WML_ITEM TRVector<2,32>;
template class WML_ITEM TRVector<2,64>;
template class WML_ITEM TRVector<2,128>;
template class WML_ITEM TRVector<2,256>;

template class WML_ITEM RVector2<2>;
template class WML_ITEM RVector2<4>;
template class WML_ITEM RVector2<8>;
template class WML_ITEM RVector2<16>;
template class WML_ITEM RVector2<32>;
template class WML_ITEM RVector2<64>;
template class WML_ITEM RVector2<128>;
template class WML_ITEM RVector2<256>;
}
//----------------------------------------------------------------------------
