// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrUtilityBox3.h"
#include "WmlIntrUtilityTri3.h"
#include "WmlIntrUtilityLin3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
void Wml::BoxProjection (const Vector3<Real>& rkAxis,
    const Box3<Real>& rkBox, Real& rfMin, Real& rfMax)
{
    Real fOrigin = rkAxis.Dot(rkBox.Center());
    Real fMaximumExtent =
        Math<Real>::FAbs(rkBox.Extent(0)*rkAxis.Dot(rkBox.Axis(0))) +
        Math<Real>::FAbs(rkBox.Extent(1)*rkAxis.Dot(rkBox.Axis(1))) +
        Math<Real>::FAbs(rkBox.Extent(2)*rkAxis.Dot(rkBox.Axis(2)));

    rfMin = fOrigin - fMaximumExtent;
    rfMax = fOrigin + fMaximumExtent;
}
//----------------------------------------------------------------------------
template <class Real>
void Wml::GetBoxConfiguration (const Vector3<Real>& rkAxis, 
    const Box3<Real>& rkBox, ContactConfig<Real>& rkConfig)
{
    // Description of coordinate ordering scheme for ContactConfig.m_aiIndex.
    //
    // Vertex number (up/down) vs. sign of extent (only matters in mapping
    // back)
    //   012
    // 0 ---
    // 1 +--
    // 2 -+-
    // 3 ++-
    // 4 --+
    // 5 +-+
    // 6 -++
    // 7 +++
    //
    // When it returns an ordering in the ContactConfig, it is also
    // guarenteed to be in-order (if 4 vertices, then they are guarunteed in
    // an order that will create a box, e.g. 0,1,3,2).
    //
    // assert: akAxis is an array containing unit length vectors

    Real afAxis[3] =
    {
        rkAxis.Dot(rkBox.Axis(0)),
        rkAxis.Dot(rkBox.Axis(1)),
        rkAxis.Dot(rkBox.Axis(2))
    };

    Real afAAxis[3] =
    {
        Math<Real>::FAbs(afAxis[0]),
        Math<Real>::FAbs(afAxis[1]),
        Math<Real>::FAbs(afAxis[2])
    };

    Real fMaxProjectedExtent;

    if ( afAAxis[0] < Math<Real>::EPSILON )
    {
        if ( afAAxis[1] < Math<Real>::EPSILON )
        {
            // face-face
            rkConfig.m_kMap = m44;

            fMaxProjectedExtent = afAAxis[2]*rkBox.Extent(2);

            // faces have normals along axis[2]
            if ( afAxis[2] > (Real)0.0 )
            {       
                rkConfig.m_aiIndex[0] = 0;
                rkConfig.m_aiIndex[1] = 1;
                rkConfig.m_aiIndex[2] = 3;
                rkConfig.m_aiIndex[3] = 2;

                rkConfig.m_aiIndex[4] = 6;
                rkConfig.m_aiIndex[5] = 7;
                rkConfig.m_aiIndex[6] = 5;
                rkConfig.m_aiIndex[7] = 4;
            }
            else
            {
                rkConfig.m_aiIndex[0] = 6;
                rkConfig.m_aiIndex[1] = 7;
                rkConfig.m_aiIndex[2] = 5;
                rkConfig.m_aiIndex[3] = 4;

                rkConfig.m_aiIndex[4] = 0;
                rkConfig.m_aiIndex[5] = 1;
                rkConfig.m_aiIndex[6] = 3;
                rkConfig.m_aiIndex[7] = 2;
            }
        }
        else if ( afAAxis[2] < Math<Real>::EPSILON )
        {
            // face-face
            rkConfig.m_kMap = m44;

            fMaxProjectedExtent = afAAxis[1]*rkBox.Extent(1);

            // faces have normals along axis[1]
            if ( afAxis[1] > (Real)0.0 ) 
            {
                rkConfig.m_aiIndex[0] = 4;
                rkConfig.m_aiIndex[1] = 5;
                rkConfig.m_aiIndex[2] = 1;
                rkConfig.m_aiIndex[3] = 0;

                rkConfig.m_aiIndex[4] = 2;
                rkConfig.m_aiIndex[5] = 3;
                rkConfig.m_aiIndex[6] = 7;
                rkConfig.m_aiIndex[7] = 6;
            }
            else
            {
                rkConfig.m_aiIndex[0] = 2;
                rkConfig.m_aiIndex[1] = 3;
                rkConfig.m_aiIndex[2] = 7;
                rkConfig.m_aiIndex[3] = 6;

                rkConfig.m_aiIndex[4] = 4;
                rkConfig.m_aiIndex[5] = 5;
                rkConfig.m_aiIndex[6] = 1;
                rkConfig.m_aiIndex[7] = 0;
            }
        }
        else // only afAxis[0] is equal to 0
        {
            // seg-seg
            rkConfig.m_kMap = m2_2;

            fMaxProjectedExtent = afAAxis[1]*rkBox.Extent(1) +
                afAAxis[2]*rkBox.Extent(2);

            // axis 0 is perpendicular to rkAxis
            if ( afAxis[1] > (Real)0.0 )
            {
                if ( afAxis[2] > (Real)0.0 ) 
                {
                    rkConfig.m_aiIndex[0] = 0;
                    rkConfig.m_aiIndex[1] = 1;

                    rkConfig.m_aiIndex[6] = 6;
                    rkConfig.m_aiIndex[7] = 7;
                }
                else 
                {
                    rkConfig.m_aiIndex[0] = 4;
                    rkConfig.m_aiIndex[1] = 5;

                    rkConfig.m_aiIndex[6] = 2;
                    rkConfig.m_aiIndex[7] = 3;
                }
            }
            else // afAxis[1] < 0
            {
                if ( afAxis[2] > (Real)0.0 )
                {
                    rkConfig.m_aiIndex[0] = 2;
                    rkConfig.m_aiIndex[1] = 3;

                    rkConfig.m_aiIndex[6] = 4;
                    rkConfig.m_aiIndex[7] = 5;
                }
                else
                {
                    rkConfig.m_aiIndex[0] = 6;
                    rkConfig.m_aiIndex[1] = 7;

                    rkConfig.m_aiIndex[6] = 0;
                    rkConfig.m_aiIndex[7] = 1;
                }
            }
        }
    }
    else if ( afAAxis[1] < Math<Real>::EPSILON )
    {
        if ( afAAxis[2] < Math<Real>::EPSILON )
        {
            // face-face
            rkConfig.m_kMap = m44;

            fMaxProjectedExtent = afAAxis[0]*rkBox.Extent(0);

            // faces have normals along axis[0]
            if ( afAxis[0] > (Real)0.0 )
            {
                rkConfig.m_aiIndex[0] = 0;
                rkConfig.m_aiIndex[1] = 2;
                rkConfig.m_aiIndex[2] = 6;
                rkConfig.m_aiIndex[3] = 4;

                rkConfig.m_aiIndex[4] = 5;
                rkConfig.m_aiIndex[5] = 7;
                rkConfig.m_aiIndex[6] = 3;
                rkConfig.m_aiIndex[7] = 1;
            }
            else
            {
                rkConfig.m_aiIndex[4] = 0;
                rkConfig.m_aiIndex[5] = 2;
                rkConfig.m_aiIndex[6] = 6;
                rkConfig.m_aiIndex[7] = 4;

                rkConfig.m_aiIndex[0] = 5;
                rkConfig.m_aiIndex[1] = 7;
                rkConfig.m_aiIndex[2] = 3;
                rkConfig.m_aiIndex[3] = 1;
            }

        }
        else // only afAxis[1] is equal to 0
        {
            // seg-seg
            rkConfig.m_kMap = m2_2;

            fMaxProjectedExtent = afAAxis[0]*rkBox.Extent(0) + 
                afAAxis[2]*rkBox.Extent(2);

            // axis 1 is perpendicular to rkAxis
            if ( afAxis[0] > (Real)0.0 )
            {
                if ( afAxis[2] > (Real)0.0 ) 
                {
                    rkConfig.m_aiIndex[0] = 0;
                    rkConfig.m_aiIndex[1] = 2;

                    rkConfig.m_aiIndex[6] = 5;
                    rkConfig.m_aiIndex[7] = 7;
                }
                else 
                {
                    rkConfig.m_aiIndex[0] = 4;
                    rkConfig.m_aiIndex[1] = 6;

                    rkConfig.m_aiIndex[6] = 1;
                    rkConfig.m_aiIndex[7] = 3;
                }
            }
            else // afAxis[0] < 0
            {
                if ( afAxis[2] > (Real)0.0 )
                {
                    rkConfig.m_aiIndex[0] = 1;
                    rkConfig.m_aiIndex[1] = 3;

                    rkConfig.m_aiIndex[6] = 4;
                    rkConfig.m_aiIndex[7] = 6;
                }
                else
                {
                    rkConfig.m_aiIndex[0] = 5;
                    rkConfig.m_aiIndex[1] = 7;

                    rkConfig.m_aiIndex[6] = 0;
                    rkConfig.m_aiIndex[7] = 2;
                }
            }
        }
    }
    
    else if ( afAAxis[2] < Math<Real>::EPSILON ) // only axis2 less than zero
    {
        // seg-seg
        rkConfig.m_kMap = m2_2;

        fMaxProjectedExtent = afAAxis[0]*rkBox.Extent(0) +
            afAAxis[1]*rkBox.Extent(1);

        // axis 2 is perpendicular to rkAxis
        if ( afAxis[0] > (Real)0.0 )
        {
            if ( afAxis[1] > (Real)0.0 ) 
            {
                rkConfig.m_aiIndex[0] = 0;
                rkConfig.m_aiIndex[1] = 4;

                rkConfig.m_aiIndex[6] = 3;
                rkConfig.m_aiIndex[7] = 7;
            }
            else 
            {
                rkConfig.m_aiIndex[0] = 2;
                rkConfig.m_aiIndex[1] = 6;

                rkConfig.m_aiIndex[6] = 1;
                rkConfig.m_aiIndex[7] = 5;
            }
        }
        else // afAxis[0] < 0
        {
            if ( afAxis[1] > (Real)0.0 )
            {
                rkConfig.m_aiIndex[0] = 1;
                rkConfig.m_aiIndex[1] = 5;

                rkConfig.m_aiIndex[6] = 2;
                rkConfig.m_aiIndex[7] = 6;
            }
            else
            {
                rkConfig.m_aiIndex[0] = 3;
                rkConfig.m_aiIndex[1] = 7;

                rkConfig.m_aiIndex[6] = 0;
                rkConfig.m_aiIndex[7] = 4;
            }
        }
    }
  
    else // no axis is equal to zero
    {
        // point-point (unique maximal and minimal vertex)
        rkConfig.m_kMap = m1_1;

        fMaxProjectedExtent = afAAxis[0]*rkBox.Extent(0) +
            afAAxis[1]*rkBox.Extent(1) + afAAxis[2]*rkBox.Extent(2);

        // only these two vertices matter, the rest are irrelevant
        rkConfig.m_aiIndex[0] = 
            ( afAxis[0] > (Real)0.0 ? 0 : 1 ) + 
            ( afAxis[1] > (Real)0.0 ? 0 : 2 ) + 
            ( afAxis[2] > (Real)0.0 ? 0 : 4 );
        // by ordering the vertices this way, opposite corners add up to 7
        rkConfig.m_aiIndex[7] = 7 - rkConfig.m_aiIndex[0];
    }

    // Find projections onto line
    Real fOrigin = rkAxis.Dot(rkBox.Center());
    rkConfig.m_fMin = fOrigin - fMaxProjectedExtent;
    rkConfig.m_fMax = fOrigin + fMaxProjectedExtent;
}
//----------------------------------------------------------------------------
template <class Real>
void Wml::FindContactSetCoplanarLineRect (const Vector3<Real> akSeg[2],
    const Vector3<Real> akFace[4], int& riQuantity, Vector3<Real>* akP)
{
    // The potential intersection is initialized to the line segment and then
    // clipped against the four sides of the rect

    riQuantity = 2;
    memcpy(akP,akSeg,2*sizeof(Vector3<Real>));

    for (int i0 = 3, i1 = 0; i1 < 4; i0 = i1++)
    {
        Vector3<Real> kNormal = akFace[i1] - akFace[i0];
        Real fConstant = kNormal.Dot(akFace[i0]);
        ClipConvexPolygonAgainstPlane(kNormal,fConstant,riQuantity,akP);
    }
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> Wml::GetPoint (int iIndex, const Box3<Real>& rkBox)
{
    Vector3<Real> kPoint = rkBox.Center();

    if ( iIndex & 4 )
        kPoint += rkBox.Extent(2)*rkBox.Axis(2);
    else
        kPoint -= rkBox.Extent(2)*rkBox.Axis(2);

    if ( iIndex & 2 )
        kPoint += rkBox.Extent(1)*rkBox.Axis(1);
    else
        kPoint -= rkBox.Extent(1)*rkBox.Axis(1);

    if ( iIndex & 1 )
        kPoint += rkBox.Extent(0)*rkBox.Axis(0);
    else
        kPoint -= rkBox.Extent(0)*rkBox.Axis(0);

    return kPoint;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM void BoxProjection<float> (const Vector3<float>&,
    const Box3<float>&, float&, float&);
template WML_ITEM void GetBoxConfiguration<float> (
    const Vector3<float>&, const Box3<float>&, ContactConfig<float>&);
template WML_ITEM void FindContactSetCoplanarLineRect<float> (
    const Vector3<float>[2], const Vector3<float>[4], int&,
    Vector3<float>*);
template WML_ITEM Vector3<float> GetPoint<float> (int,
    const Box3<float>&);

template WML_ITEM void BoxProjection<double> (const Vector3<double>&,
    const Box3<double>&, double&, double&);
template WML_ITEM void GetBoxConfiguration<double> (
    const Vector3<double>&, const Box3<double>&, ContactConfig<double>&);
template WML_ITEM void FindContactSetCoplanarLineRect<double> (
    const Vector3<double>[2], const Vector3<double>[4], int&,
    Vector3<double>*);
template WML_ITEM Vector3<double> GetPoint<double> (int,
    const Box3<double>&);
}
//----------------------------------------------------------------------------
