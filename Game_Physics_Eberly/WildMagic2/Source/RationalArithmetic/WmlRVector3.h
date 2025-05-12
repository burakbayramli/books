// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLRVECTOR3_H
#define WMLRVECTOR3_H

#include "WmlTRVector.h"

namespace Wml
{

template <int ISIZE>
class WML_ITEM RVector3 : public TRVector<3,ISIZE>
{
public:
    // construction
    RVector3 ();
    RVector3 (const RVector3& rkV);
    RVector3 (const TRVector<3,ISIZE>& rkV)
    {
        // TO DO:  Inline body written here because of an apparent MSVC++ .NET
        // compiler bug.  If placed in the *.inl file, the compiler complains:
        //
        //   error C2244: 'Wml::RVector3<>::__ctor' : unable to match function
        //       definition to an existing declaration
        //   definition
        //       'Wml::RVector3<>::RVector3(const Wml::TRVector<3,> &)'
        //   existing declarations
        //       'Wml::RVector3<>::RVector3(const Wml::TRational<> &,
        //                                  const Wml::TRational<> &)'
        //       'Wml::RVector3<>::RVector3(const Wml::TRVector<3,> &)'
        //       'Wml::RVector3<>::RVector3(const Wml::RVector3<> &)'
        //       'Wml::RVector3<>::RVector3(void)'
        //
        // The "definition" is in the "existing declarations" list, so I do
        // not know what the compiler is complaining about.  When I find out
        // how to handle this in the *.inl file, I will remove the body from
        // this location.

        m_akTuple[0] = rkV[0];
        m_akTuple[1] = rkV[1];
        m_akTuple[2] = rkV[2];
    }

    RVector3 (const TRational<ISIZE>& rkX, const TRational<ISIZE>& rkY,
        const TRational<ISIZE>& rkZ);

    // member access
    TRational<ISIZE> X () const;
    TRational<ISIZE>& X ();
    TRational<ISIZE> Y () const;
    TRational<ISIZE>& Y ();
    TRational<ISIZE> Z () const;
    TRational<ISIZE>& Z ();

    // assignment
    RVector3& operator= (const RVector3& rkV);
    RVector3& operator= (const TRVector<3,ISIZE>& rkV)
    {
        // TO DO:  Inline body written here because of an apparent MSVC++ .NET
        // compiler bug.  If placed in the *.inl file, the compiler complains:
        //
        //   error C2244: 'Wml::RVector3<>::operator`='' : unable to match
        //       function definition to an existing declaration
        //   definition
        //       'Wml::RVector3<> &Wml::RVector3<>::operator =(
        //            const Wml::TRVector<3,> &)'
        //   existing declarations
        //       'Wml::RVector3<> &Wml::RVector3<>::operator =(
        //            const Wml::TRVector<3,> &)'
        //       'Wml::RVector3<> &Wml::RVector3<>::operator =(
        //            const Wml::RVector3<> &)'
        //
        // The "definition" is in the "existing declarations" list, so I do
        // not know what the compiler is complaining about.  When I find out
        // how to handle this in the *.inl file, I will remove the body from
        // this location.

        m_akTuple[0] = rkV[0];
        m_akTuple[1] = rkV[1];
        m_akTuple[2] = rkV[2];
        return *this;
    }

    // returns Cross(this,V)
    RVector3 Cross (const RVector3& rkV) const;

    // returns Dot(this,Cross(U,V))
    TRational<ISIZE> TripleScalar (const RVector3& rkU, const RVector3& rkV)
        const;
};

}

#endif
