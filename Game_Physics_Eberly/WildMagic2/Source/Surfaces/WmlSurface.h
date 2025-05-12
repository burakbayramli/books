// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLSURFACE_H
#define WMLSURFACE_H

#include "WmlSystem.h"

namespace Wml
{

template <class Real>
class WML_ITEM Surface
{
public:
    virtual ~Surface ();

protected:
    // abstract base class
    Surface ();
};

typedef Surface<float> Surfacef;
typedef Surface<double> Surfaced;

}

#endif
