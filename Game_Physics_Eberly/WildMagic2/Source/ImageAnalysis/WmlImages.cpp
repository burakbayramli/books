// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlImages.h"
using namespace Wml;

// Instantiations to support dynamic link libraries.  This appears to be
// necessary on the SGI running IRIX and using the MIPSPRO CC compiler.

namespace Wml
{

template class WML_ITEM TImage<Echar>;
template class WML_ITEM TImage<Euchar>;
template class WML_ITEM TImage<Eshort>;
template class WML_ITEM TImage<Eushort>;
template class WML_ITEM TImage<Eint>;
template class WML_ITEM TImage<Euint>;
template class WML_ITEM TImage<Elong>;
template class WML_ITEM TImage<Eulong>;
template class WML_ITEM TImage<Efloat>;
template class WML_ITEM TImage<Edouble>;
template class WML_ITEM TImage<Ergb5>;
template class WML_ITEM TImage<Ergb8>;

template class WML_ITEM TImage2D<Echar>;
template class WML_ITEM TImage2D<Euchar>;
template class WML_ITEM TImage2D<Eshort>;
template class WML_ITEM TImage2D<Eushort>;
template class WML_ITEM TImage2D<Eint>;
template class WML_ITEM TImage2D<Euint>;
template class WML_ITEM TImage2D<Elong>;
template class WML_ITEM TImage2D<Eulong>;
template class WML_ITEM TImage2D<Efloat>;
template class WML_ITEM TImage2D<Edouble>;
template class WML_ITEM TImage2D<Ergb5>;
template class WML_ITEM TImage2D<Ergb8>;

template class WML_ITEM TImage3D<Echar>;
template class WML_ITEM TImage3D<Euchar>;
template class WML_ITEM TImage3D<Eshort>;
template class WML_ITEM TImage3D<Eushort>;
template class WML_ITEM TImage3D<Eint>;
template class WML_ITEM TImage3D<Euint>;
template class WML_ITEM TImage3D<Elong>;
template class WML_ITEM TImage3D<Eulong>;
template class WML_ITEM TImage3D<Efloat>;
template class WML_ITEM TImage3D<Edouble>;
template class WML_ITEM TImage3D<Ergb5>;
template class WML_ITEM TImage3D<Ergb8>;

}
