// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLRVECTOR2_H
#define WMLRVECTOR2_H

#include "WmlTRVector.h"

namespace Wml
{

template <int ISIZE>
class WML_ITEM RVector2 : public TRVector<2,ISIZE>
{
public:
    // construction
    RVector2 ();
    RVector2 (const RVector2& rkV);
    RVector2 (const TRVector<2,ISIZE>& rkV)
    {
        // TO DO:  Inline body written here because of an apparent MSVC++ .NET
        // compiler bug.  If placed in the *.inl file, the compiler complains:
        //
        //   error C2244: 'Wml::RVector2<>::__ctor' : unable to match function
        //       definition to an existing declaration
        //   definition
        //       'Wml::RVector2<>::RVector2(const Wml::TRVector<2,> &)'
        //   existing declarations
        //       'Wml::RVector2<>::RVector2(const Wml::TRational<> &,
        //                                  const Wml::TRational<> &)'
        //       'Wml::RVector2<>::RVector2(const Wml::TRVector<2,> &)'
        //       'Wml::RVector2<>::RVector2(const Wml::RVector2<> &)'
        //       'Wml::RVector2<>::RVector2(void)'
        //
        // The "definition" is in the "existing declarations" list, so I do
        // not know what the compiler is complaining about.  When I find out
        // how to handle this in the *.inl file, I will remove the body from
        // this location.

        m_akTuple[0] = rkV[0];
        m_akTuple[1] = rkV[1];
    }

    RVector2 (const TRational<ISIZE>& rkX, const TRational<ISIZE>& rkY);

    // member access
    TRational<ISIZE> X () const;
    TRational<ISIZE>& X ();
    TRational<ISIZE> Y () const;
    TRational<ISIZE>& Y ();

    // assignment
    RVector2& operator= (const RVector2& rkV);
    RVector2& operator= (const TRVector<2,ISIZE>& rkV)
    {
        // TO DO:  Inline body written here because of an apparent MSVC++ .NET
        // compiler bug.  If placed in the *.inl file, the compiler complains:
        //
        //   error C2244: 'Wml::RVector2<>::operator`='' : unable to match
        //       function definition to an existing declaration
        //   definition
        //       'Wml::RVector2<> &Wml::RVector2<>::operator =(
        //            const Wml::TRVector<2,> &)'
        //   existing declarations
        //       'Wml::RVector2<> &Wml::RVector2<>::operator =(
        //            const Wml::TRVector<2,> &)'
        //       'Wml::RVector2<> &Wml::RVector2<>::operator =(
        //            const Wml::RVector2<> &)'
        //
        // The "definition" is in the "existing declarations" list, so I do
        // not know what the compiler is complaining about.  When I find out
        // how to handle this in the *.inl file, I will remove the body from
        // this location.

        m_akTuple[0] = rkV[0];
        m_akTuple[1] = rkV[1];
        return *this;
    }

    // returns (y,-x)
    RVector2 Perp () const;

    // returns Cross((x,y,0),(V.x,V.y,0)) = x*V.y - y*V.x
    TRational<ISIZE> Kross (const RVector2& rkV) const;
};

}

#endif
