// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlRVector3.h"
namespace Wml
{
#include "WmlTRVector.inl"
#include "WmlRVector3.inl"
}
using namespace Wml;

//----------------------------------------------------------------------------
// explicit instantiation (N = 2, 4, 8, 16, 32, 64, 128, 256)
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM TRVector<3,2>;
template class WML_ITEM TRVector<3,4>;
template class WML_ITEM TRVector<3,8>;
template class WML_ITEM TRVector<3,16>;
template class WML_ITEM TRVector<3,32>;
template class WML_ITEM TRVector<3,64>;
template class WML_ITEM TRVector<3,128>;
template class WML_ITEM TRVector<3,256>;

template class WML_ITEM RVector3<2>;
template class WML_ITEM RVector3<4>;
template class WML_ITEM RVector3<8>;
template class WML_ITEM RVector3<16>;
template class WML_ITEM RVector3<32>;
template class WML_ITEM RVector3<64>;
template class WML_ITEM RVector3<128>;
template class WML_ITEM RVector3<256>;
}
//----------------------------------------------------------------------------
