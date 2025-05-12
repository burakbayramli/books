// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrInterval.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
int Wml::FindIntersection (Real fU0, Real fU1, Real fV0, Real fV1,
    Real& rfW0, Real& rfW1)
{
    assert( fU0 < fU1 && fV0 < fV1 );

    if ( fU1 < fV0 || fU0 > fV1 )
    {
        return 0;
    }
    else if ( fU1 > fV0 )
    {
        if ( fU0 < fV1 )
        {
            rfW0 = ( fU0 < fV0 ? fV0 : fU0 );
            rfW1 = ( fU1 > fV1 ? fV1 : fU1 );
            return 2;
        }
        else  // fU0 == fV1
        {
            rfW0 = fU0;
            return 1;
        }
    }
    else  // fU1 == fV0
    {
        rfW0 = fU1;
        return 1;
    }
}
//----------------------------------------------------------------------------
template <class Real>
int Wml::FindIntersection (Real fU0, Real fU1, Real& rfV0, Real& rfV1)
{
    assert( fU0 < fU1 && rfV0 < rfV1 );

    if ( fU1 < rfV0 || fU0 > rfV1 )
    {
        return 0;
    }
    else if ( fU1 > rfV0 )
    {
        if ( fU0 < rfV1 )
        {
            if ( fU0 > rfV0 )
                rfV0 = fU0;
            if ( fU1 < rfV1 )
                rfV1 = fU1;
            return 2;
        }
        else  // fU0 == fV1
        {
            rfV0 = fU0;
            return 1;
        }
    }
    else  // fU1 == fV0
    {
        rfV0 = fU1;
        return 1;
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM int FindIntersection<float> (float, float, float,
    float, float&, float&);
template WML_ITEM int FindIntersection<float> (float, float, float&,
    float&);

template WML_ITEM int FindIntersection<double> (double, double, double,
    double, double&, double&);
template WML_ITEM int FindIntersection<double> (double, double, double&,
    double&);
}
//----------------------------------------------------------------------------
