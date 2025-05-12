// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlContSeparatePoints3.h"
#include "WmlConvexHull3.h"
using namespace Wml;
using namespace std;

//----------------------------------------------------------------------------
template <class Real>
static int OnSameSide (const Plane3<Real>& rkPlane, int iTriangleQuantity,
    const int* aiConnect, const Vector3<Real>* akPoint)
{
    // test if all points on same side of plane (nx,ny,nz)*(x,y,z) = c
    int iPosSide = 0, iNegSide = 0;

    for (int iT = 0; iT < iTriangleQuantity; iT++)
    {
        for (int i = 0; i < 3; i++)
        {
            int iV = aiConnect[3*iT+i];;
            Real fC0 = rkPlane.GetNormal().Dot(akPoint[iV]);
            if ( fC0 > rkPlane.GetConstant() + Math<Real>::EPSILON )
                iPosSide++;
            else if ( fC0 < rkPlane.GetConstant() - Math<Real>::EPSILON )
                iNegSide++;
            
            if ( iPosSide && iNegSide )
            {
                // plane splits point set
                return 0;
            }
        }
    }

    return iPosSide ? +1 : -1;
}
//----------------------------------------------------------------------------
template <class Real>
static int WhichSide (const Plane3<Real>& rkPlane, int iTriangleQuantity,
    const int* aiConnect, const Vector3<Real>* akPoint)
{
    // establish which side of plane hull is on
    for (int iT = 0; iT < iTriangleQuantity; iT++)
    {
        for (int i = 0; i < 3; i++)
        {
            int iV = aiConnect[3*iT+i];
            Real fC0 = rkPlane.GetNormal().Dot(akPoint[iV]);
            if ( fC0 > rkPlane.GetConstant() + Math<Real>::EPSILON )
            {
                // positive side
                return +1;
            }
            if ( fC0 < rkPlane.GetConstant() - Math<Real>::EPSILON )
            {
                // negative side
                return -1;
            }
        }
    }

    // hull is effectively collinear
    return 0;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::SeparatePoints3 (int iQuantity0, const Vector3<Real>* akVertex0,
    int iQuantity1, const Vector3<Real>* akVertex1, Plane3<Real>& rkSeprPlane)
{
    // construct convex hull of point set 0
    ConvexHull3<Real> kHull0(iQuantity0,akVertex0);
    assert( kHull0.GetType() == ConvexHull3<Real>::HULL_SPATIAL );
    const vector<int>& rkConnect0 = kHull0.GetConnectivity();
    int iTriangleQuantity0 = (int)rkConnect0.size();
    const int* aiConnect0 = &rkConnect0.front();

    // construct convex hull of point set 1
    ConvexHull3<Real> kHull1(iQuantity1,akVertex1);
    assert( kHull1.GetType() == ConvexHull3<Real>::HULL_SPATIAL );
    const vector<int>& rkConnect1 = kHull1.GetConnectivity();
    int iTriangleQuantity1 = (int)rkConnect1.size();
    const int* aiConnect1 = &rkConnect1.front();

    // test faces of hull 0 for possible separation of points
    int i, iI0, iI1, iI2, iSide0, iSide1;
    Vector3<Real> kDiff0, kDiff1;
    for (i = 0; i < iTriangleQuantity0; i++)
    {
        // lookup face (assert: iI0 != iI1 && iI0 != iI2 && iI1 != iI2)
        iI0 = aiConnect0[3*i  ];
        iI1 = aiConnect0[3*i+1];
        iI2 = aiConnect0[3*i+2];

        // compute potential separating plane (assert: normal != (0,0,0))
        kDiff0 = akVertex0[iI1] - akVertex0[iI0];
        kDiff1 = akVertex0[iI2] - akVertex0[iI0];
        rkSeprPlane.SetNormal(kDiff0.Cross(kDiff1));
        rkSeprPlane.SetConstant(rkSeprPlane.GetNormal().Dot(akVertex0[iI0]));

        // determine if hull 1 is on same side of plane
        iSide1 = OnSameSide(rkSeprPlane,iTriangleQuantity1,aiConnect1,
            akVertex1);
        if ( iSide1 )
        {
            // determine which side of plane hull 0 lies
            iSide0 = WhichSide(rkSeprPlane,iTriangleQuantity0,aiConnect0,
                akVertex0);
            if ( iSide0*iSide1 <= 0 )  // plane separates hulls
                return true;
        }
    }

    // test faces of hull 1 for possible separation of points
    for (i = 0; i < iTriangleQuantity1; i++)
    {
        // lookup edge (assert: iI0 != iI1 && iI0 != iI2 && iI1 != iI2)
        iI0 = aiConnect1[3*i  ];
        iI1 = aiConnect1[3*i+1];
        iI2 = aiConnect1[3*i+2];

        // compute perpendicular to face (assert: normal != (0,0,0))
        kDiff0 = akVertex1[iI1] - akVertex1[iI0];
        kDiff1 = akVertex1[iI2] - akVertex1[iI0];
        rkSeprPlane.SetNormal(kDiff0.Cross(kDiff1));
        rkSeprPlane.SetConstant(rkSeprPlane.GetNormal().Dot(akVertex1[iI0]));

        // determine if hull 0 is on same side of plane
        iSide0 = OnSameSide(rkSeprPlane,iTriangleQuantity0,aiConnect0,
            akVertex0);
        if ( iSide0 )
        {
            // determine which side of plane hull 1 lies
            iSide1 = WhichSide(rkSeprPlane,iTriangleQuantity1,aiConnect1,
                akVertex1);
            if ( iSide0*iSide1 <= 0 )  // plane separates hulls
                return true;
        }
    }

    // build edge set for hull 0
    set<pair<int,int> > kESet0;
    for (i = 0; i < iTriangleQuantity0; i++)
    {
        // lookup face (assert: iI0 != iI1 && iI0 != iI2 && iI1 != iI2)
        iI0 = aiConnect0[3*i  ];
        iI1 = aiConnect0[3*i+1];
        iI2 = aiConnect0[3*i+2];
        kESet0.insert(make_pair(iI0,iI1));
        kESet0.insert(make_pair(iI0,iI2));
        kESet0.insert(make_pair(iI1,iI2));
    }

    // build edge list for hull 1
    set<pair<int,int> > kESet1;
    for (i = 0; i < iTriangleQuantity1; i++)
    {
        // lookup face (assert: iI0 != iI1 && iI0 != iI2 && iI1 != iI2)
        iI0 = aiConnect1[3*i  ];
        iI1 = aiConnect1[3*i+1];
        iI2 = aiConnect1[3*i+2];
        kESet1.insert(make_pair(iI0,iI1));
        kESet1.insert(make_pair(iI0,iI2));
        kESet1.insert(make_pair(iI1,iI2));
    }

    // Test planes whose normals are cross products of two edges, one from
    // each hull.
    set<pair<int,int> >::iterator pkEI0, pkEI1;
    for (pkEI0 = kESet0.begin(); pkEI0 != kESet0.end(); pkEI0++)
    {
        // get edge
        kDiff0 = akVertex0[pkEI0->second] - akVertex0[pkEI0->first];

        for (pkEI1 = kESet1.begin(); pkEI1 != kESet1.end(); pkEI1++)
        {
            kDiff1 = akVertex1[pkEI1->second] - akVertex1[pkEI1->first];

            // compute potential separating plane
            rkSeprPlane.SetNormal(kDiff0.Cross(kDiff1));
            rkSeprPlane.SetConstant(rkSeprPlane.GetNormal().Dot(
                akVertex0[pkEI0->first]));

            // determine if hull 0 is on same side of plane
            iSide0 = OnSameSide(rkSeprPlane,iTriangleQuantity0,aiConnect0,
                akVertex0);
            iSide1 = OnSameSide(rkSeprPlane,iTriangleQuantity1,aiConnect1,
                akVertex1);
            if ( iSide0*iSide1 < 0 )  // plane separates hulls
                return true;
        }
    }

    return false;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool SeparatePoints3<float> (int,
    const Vector3<float>*, int, const Vector3<float>*, Plane3<float>&);

template WML_ITEM bool SeparatePoints3<double> (int,
    const Vector3<double>*, int, const Vector3<double>*, Plane3<double>&);
}
//----------------------------------------------------------------------------
