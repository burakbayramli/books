// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrTri3Sph3.h"
#include "WmlDistVec3Tri3.h"
#include "WmlIntrLin3Sph3.h"
using namespace Wml;

//----------------------------------------------------------------------------
// stationary objects
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Triangle3<Real>& rkTri,
    const Sphere3<Real>& rkSphere)
{
    Real fSqrDist = SqrDistance(rkSphere.Center(),rkTri);
    Real fRSqr = rkSphere.Radius()*rkSphere.Radius();
    return fSqrDist < fRSqr;
}
//----------------------------------------------------------------------------
// moving objects
//----------------------------------------------------------------------------
template <class Real>
static bool FindTriSphrCoplanarIntersection (int iVertex, 
    const Vector3<Real> akV[3], const Vector3<Real>& /* rkNormal */,
    const Vector3<Real>& rkSideNorm, const Vector3<Real>& rkSide,
    const Sphere3<Real>& rkSphere, const Vector3<Real>& rkTriVelocity,
    const Vector3<Real>& rkSphVelocity, Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real> akP[6])
{
    // TO DO.  The parameter rkNormal is not used here.  Is this an error?
    // Or does the caller make some adjustments to the other inputs to
    // account for the normal?

    // iVertex is the "hinge" vertex that the two potential edges that can
    // be intersected by the sphere connect to, and it indexes into akV.

    // rkSideNorm is the normal of the plane formed by (iVertex,iVertex+1)
    // and the tri norm, passed so as not to recalculate

    // check for intersections at time 0
    Vector3<Real> kDist = akV[iVertex] - rkSphere.Center();
    if ( kDist.SquaredLength() < rkSphere.Radius()*rkSphere.Radius() )
    {
        // already intersecting that vertex
        rfTFirst = (Real)0.0;
        return false;
    }

    // Tri stationary, sphere moving
    Vector3<Real> kVel = rkSphVelocity - rkTriVelocity;

    // check for easy out
    if ( kVel.Dot(kDist) <= (Real)0.0 )
    {
        // moving away
        return false;
    }

    // find intersection of velocity ray and side normal

    // project ray and plane onto the plane normal
    Real fPlane = rkSideNorm.Dot(akV[iVertex]);
    Real fCenter = rkSideNorm.Dot(rkSphere.Center());
    Real fVel = rkSideNorm.Dot(kVel);
    Real fFactor = (fPlane - fCenter)/fVel;

    Vector3<Real> kPoint = rkSphere.Center() + fFactor*kVel;

    // now, find which side of the hinge vertex this lies by projecting
    // both the vertex and this new point onto the triangle edge (the same
    // edge whose "normal" was used to find this point)

    Real fVertex = rkSide.Dot(akV[iVertex]);
    Real fPoint = rkSide.Dot(kPoint);

    Segment3<Real> kSeg;

    if ( fPoint >= fVertex )
    {
        // intersection with edge (iVertex,iVertex+1)
        kSeg.Origin() = akV[iVertex];
        kSeg.Direction() = akV[(iVertex+1)%3] - akV[iVertex];
    }
    else
    {
        // intersection with edge (iVertex-1,iVertex)
        if ( iVertex != 0 )
            kSeg.Origin() = akV[iVertex-1];
        else
            kSeg.Origin() = akV[2];
        kSeg.Direction() = akV[iVertex] - kSeg.Origin();
    }

    // This could be either an sphere-edge or a sphere-vertex intersection
    // (this test isn't enough to differentiate), so use the full-on
    // line-sphere test.
    return FindIntersection(kSeg,rkTriVelocity,rkSphere,rkSphVelocity,
        rfTFirst,fTMax,riQuantity,akP);
}
//----------------------------------------------------------------------------
template <class Real>
static bool FindSphereVertexIntersection (const Vector3<Real>& rkVertex, 
    const Sphere3<Real>& rkSphere, const Vector3<Real>& rkSphVelocity,
    const Vector3<Real>& rkTriVelocity, Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real> akP[6])
{
    // Finds the time and place (and possible occurance, it may miss) of an
    // intersection between a sphere of fRadius at rkOrigin moving in rkDir
    // towards a vertex at rkVertex.

    Vector3<Real> kVel = rkSphVelocity - rkTriVelocity;
    Vector3<Real> kD = rkSphere.Center() - rkVertex;
    Vector3<Real> kCross = kD.Cross(kVel);
    Real fRSqr = rkSphere.Radius()*rkSphere.Radius();
    Real fVSqr = kVel.SquaredLength();

    if ( kCross.SquaredLength() > fRSqr*fVSqr )
    {
        // ray overshoots the sphere
        return false;
    }

    // find time of intersection
    Real fDot = kD.Dot(kVel);
    Real fDiff = kD.SquaredLength() - fRSqr;
    Real fInv = Math<Real>::InvSqrt(Math<Real>::FAbs(fDot*fDot-fVSqr*fDiff));

    rfTFirst = fDiff*fInv/((Real)1.0-fDot*fInv);
    if ( rfTFirst > fTMax )
    {
        // intersection after max time
        return false;
    }

    // place of intersection is triangle vertex
    riQuantity = 1;
    akP[0] = rkVertex + rfTFirst*rkTriVelocity;
    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Triangle3<Real>& rkTri,
    const Vector3<Real>& rkTriVelocity, const Sphere3<Real>& rkSphere,
    const Vector3<Real>& rkSphVelocity, Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real> akP[6])
{
    // triangle vertices
    Vector3<Real> akV[3] =
    {
        rkTri.Origin(),
        rkTri.Origin() + rkTri.Edge0(),
        rkTri.Origin() + rkTri.Edge1()
    };

    // triangle edges
    Vector3<Real> akE[3] =
    {
        akV[1] - akV[0],
        akV[2] - akV[1], 
        akV[0] - akV[2]
    };

    // triangle normal
    Vector3<Real> kN = akE[1].Cross(akE[0]);

    // sphere center projection on triangle normal
    Real fNdC = kN.Dot(rkSphere.Center());

    // Radius projected length in normal direction.  This defers the square
    // root to normalize kN until absolutely needed.
    Real fNormRadiusSqr =
        kN.SquaredLength()*rkSphere.Radius()*rkSphere.Radius();

    // triangle projection on triangle normal
    Real fNdT = kN.Dot(akV[0]);

    // Distance from sphere to triangle along the normal
    Real fDist = fNdC - fNdT;
    
    // normals for the plane formed by edge i and the triangle normal
    Vector3<Real> akExN[3] =
    {
        akE[0].Cross(kN),
        akE[1].Cross(kN),
        akE[2].Cross(kN)
    };

    Segment3<Real> kSeg;

    if ( fDist*fDist <= fNormRadiusSqr )
    {
        // sphere currently intersects the plane of the triangle

        // see which edges the sphere center is inside/outside of
        bool bInside[3];
        for (int i = 0; i < 3; i++ )
        {
            bInside[i] = ( akExN[i].Dot(rkSphere.Center()) >=
                akExN[i].Dot(akV[i]) );
        }

        if ( bInside[0] )
        {
            if ( bInside[1] )
            {
                if ( bInside[2] )
                {
                    // triangle inside sphere
                    return false;
                }
                else // !bInside[2]
                {
                    // potential intersection with edge 2
                    kSeg.Origin() = akV[2];
                    kSeg.Direction() = akE[2];
                    return FindIntersection(kSeg,rkTriVelocity,rkSphere,
                        rkSphVelocity,rfTFirst,fTMax,riQuantity,akP);
                }
            }
            else // !bInside[1]
            {
                if ( bInside[2] )
                {
                    // potential intersection with edge 1
                    kSeg.Origin() = akV[1];
                    kSeg.Direction() = akE[1];
                    return FindIntersection(kSeg,rkTriVelocity,rkSphere,
                        rkSphVelocity,rfTFirst,fTMax,riQuantity,akP);
                }
                else // !bInside[2]
                {
                    // potential intersection with edges 1,2
                    return FindTriSphrCoplanarIntersection(2,akV,kN,akExN[2],
                        akE[2],rkSphere,rkTriVelocity,rkSphVelocity,rfTFirst,
                        fTMax,riQuantity,akP);
                }            
            }
        } 
        else // !bInside[0]
        {
            if ( bInside[1] )
            {
                if ( bInside[2] )
                {
                    // potential intersection with edge 0
                    kSeg.Origin() = akV[0];
                    kSeg.Direction() = akE[0];
                    return FindIntersection(kSeg,rkTriVelocity,rkSphere,
                        rkSphVelocity,rfTFirst,fTMax,riQuantity,akP);
                }
                else // !bInside[2]
                {
                    // potential intersection with edges 2,0
                    return FindTriSphrCoplanarIntersection(0,akV,kN,akExN[0],
                        akE[0],rkSphere,rkTriVelocity,rkSphVelocity,rfTFirst,
                        fTMax,riQuantity,akP);
                }
            }
            else // !bInside[1]
            {
                if ( bInside[2] )
                {
                    // potential intersection with edges 0,1
                    return FindTriSphrCoplanarIntersection(1,akV,kN,akExN[1],
                        akE[1],rkSphere,rkTriVelocity,rkSphVelocity,rfTFirst,
                        fTMax,riQuantity,akP);
                }
                else // !bInside[2]
                {
                    // we should not get here
                    assert( false );
                    return false;
                }            
            }
        }
    }
    else
    {
        // sphere does not currently intersect the plane of the triangle

        // sphere moving, triangle stationary
        Vector3<Real> kVel = rkSphVelocity - rkTriVelocity;

        // Find point of intersection of the sphere and the triangle
        // plane.  Where this point occurs on the plane relative to the
        // triangle determines the potential kind of intersection.

        kN.Normalize();

        // Point on sphere we care about intersecting the triangle plane
        Vector3<Real> kSpherePoint;

        // Which side of the triangle is the sphere on?
        if ( fNdC > fNdT )
        {
            // positive side
            if ( kVel.Dot(kN) >= (Real)0.0 )
            {
                // moving away, easy out
                return false;
            }

            kSpherePoint = rkSphere.Center() - rkSphere.Radius()*kN;
        }
        else
        {
            // negative side
            if ( kVel.Dot(kN) <= (Real)0.0 )
            {
                // moving away, easy out
                return false;
            }

            kSpherePoint = rkSphere.Center() + rkSphere.Radius()*kN;
        }

        // find intersection of velocity ray and triangle plane

        // project ray and plane onto the plane normal
        Real fPlane = kN.Dot(akV[0]);
        Real fPoint = kN.Dot(kSpherePoint);
        Real fVel = kN.Dot(kVel);
        Real fTime = (fPlane - fPoint)/fVel;

        // where this intersects
        Vector3<Real> kIntrPoint = kSpherePoint + fTime*kVel;

        // see which edges this intersection point is inside/outside of
        bool bInside[3];
        for (int i = 0; i < 3; i++ )
            bInside[i] = (akExN[i].Dot(kIntrPoint) >=  akExN[i].Dot(akV[i]));

        if ( bInside[0] )
        {
            if ( bInside[1] )
            {
                if ( bInside[2] )
                {
                    // intersects face at time fTime

                    if ( fTime > fTMax )
                    {
                        // intersection after tMax
                        return false;
                    }
                    else
                    {
                        rfTFirst = fTime;
                        riQuantity = 1;

                        // kIntrPoint is the point in space, assuming that 
                        // TriVel is 0.  Re-adjust the point to where it 
                        // should be, were it not.
                        akP[0] = kIntrPoint + fTime*rkTriVelocity;
                        return true;
                    }
                }
                else // !bInside[2]
                {
                    // potential intersection with edge 2
                    kSeg.Origin() = akV[2];
                    kSeg.Direction() = akE[2];
                    return FindIntersection(kSeg,rkTriVelocity,rkSphere,
                        rkSphVelocity,rfTFirst,fTMax,riQuantity,akP);
                }
            }
            else // !bInside[1]
            {
                if ( bInside[2] )
                {
                    // potential intersection with edge 1
                    kSeg.Origin() = akV[1];
                    kSeg.Direction() = akE[1];
                    return FindIntersection(kSeg,rkTriVelocity,rkSphere,
                        rkSphVelocity,rfTFirst,fTMax,riQuantity,akP);
                }
                else // !bInside[2]
                {
                    // potential intersection with vertex 2
                    return FindSphereVertexIntersection(akV[2],rkSphere,
                        rkSphVelocity,rkTriVelocity,rfTFirst,fTMax,
                        riQuantity,akP);
                }            
            }
        } 
        else // !bInside[0]
        {
            if ( bInside[1] )
            {
                if ( bInside[2] )
                {
                    // potential intersection with edge 0
                    kSeg.Origin() = akV[0];
                    kSeg.Direction() = akE[0];
                    return FindIntersection(kSeg,rkTriVelocity,rkSphere,
                        rkSphVelocity,rfTFirst,fTMax,riQuantity,akP);
                }
                else // !bInside[2]
                {
                    // potential intersection with vertex 0
                    return FindSphereVertexIntersection(akV[0],rkSphere,
                        rkSphVelocity,rkTriVelocity,rfTFirst,fTMax,
                        riQuantity,akP);
                }
            }
            else // !bInside[1]
            {
                if ( bInside[2] )
                {
                    // potential intersection with vertex 1
                    return FindSphereVertexIntersection(akV[1],rkSphere,
                        rkSphVelocity,rkTriVelocity,rfTFirst,fTMax,
                        riQuantity,akP);
                }
                else // !bInside[2]
                {
                    // we should not get here
                    assert( false );
                    return false;
                }            
            }
        }
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (
    const Triangle3<float>&, const Sphere3<float>&);
template WML_ITEM bool FindIntersection<float> (
    const Triangle3<float>&, const Vector3<float>&,
    const Sphere3<float>&, const Vector3<float>&, float&, float, int&,
    Vector3<float>[6]);

template WML_ITEM bool TestIntersection<double> (
    const Triangle3<double>&, const Sphere3<double>&);
template WML_ITEM bool FindIntersection<double> (
    const Triangle3<double>&, const Vector3<double>&,
    const Sphere3<double>&, const Vector3<double>&, double&, double, int&,
    Vector3<double>[6]);
}
//----------------------------------------------------------------------------
