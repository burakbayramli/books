// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTEGRATE1_H
#define WMLINTEGRATE1_H

#include "WmlSystem.h"

namespace Wml
{

template <class Real> class Integrate1InitTerm;

template <class Real>
class WML_ITEM Integrate1
{
public:
    // last parameter is for user-defined data
    typedef Real (*Function)(Real,void*);

    // Romberg integration
    static void SetOrder (int iOrder);
    static int GetOrder ();
    static Real RombergIntegral (Real fA, Real fB, Function oF,
        void* pvUserData = 0);

    // Gaussian quadrature
    static Real GaussianQuadrature (Real fA, Real fB, Function oF,
        void* pvUserData = 0);

protected:
    // parameters for Romberg integration
    static int ms_iOrder;
    static Real* ms_apfRom[2];

    // pre-main initialization and post-main termination
    static void Initialize ();
    static void Terminate ();

    friend class Integrate1InitTerm<Real>;
};

typedef Integrate1<float> Integrate1f;
typedef Integrate1<double> Integrate1d;

}

#endif
