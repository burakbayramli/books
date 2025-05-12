// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrUtility3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::AxisTest (const Vector3<Real>& rkVelocity,
    const Vector3<Real>& rkTestAxis, Real fUMin, Real fUMax, Real fVMin,
    Real fVMax, Real& rfTFirst, Real& rfTLast, Real fTMax)
{
    // Static separating axis test.  Returns false iff U and V don't intersect
    // in the interval [0, TMax] on any separating axis ( TFirst > TLast ||
    // TFirst > TMax ) during the time interval, that is, a quick out.
    // Returns true otherwise.
    //
    // fUmin, fUmax, fVMin, and fVMax are the minimal and maximal points of
    // whatever object U and V are projected onto the test axis.
    //
    // rkVelocity is VelocityV - VelocityU

    Real fT;
    Real fSpeed = rkTestAxis.Dot(rkVelocity);
    
    if ( fVMax < fUMin ) // V on left of U
    {
        if ( fSpeed <= (Real)0.0 ) // V moving away from U
            return false;

        // find first time of contact on this axis
        fT = (fUMin - fVMax)/fSpeed;
        if ( fT > rfTFirst )
            rfTFirst = fT;

        // quick out: intersection after desired interval
        if ( rfTFirst > fTMax )
            return false;   

        // find last time of contact on this axis
        fT = (fUMax - fVMin)/fSpeed;
        if ( fT < fTMax )
            rfTLast = fT;

        // quick out: intersection before desired interval
        if ( rfTFirst > rfTLast )
            return false; 
    }
    else if ( fUMax < fVMin )   // V on right of U
    {
        if ( fSpeed >= (Real)0.0 ) // V moving away from U
            return false;

        // find first time of contact on this axis
        fT = (fUMax - fVMin)/fSpeed;
        if ( fT > rfTFirst )
            rfTFirst = fT;

        // quick out: intersection after desired interval
        if ( rfTFirst > fTMax )
            return false;   

        // find last time of contact on this axis
        fT = (fUMin - fVMax)/fSpeed;
        if ( fT < fTMax )
            rfTLast = fT;

        // quick out: intersection before desired interval
        if ( rfTFirst > rfTLast )
            return false; 

    }
    else // V and U on overlapping interval
    {
        if ( fSpeed > (Real)0.0 )
        {
            // find last time of contact on this axis
            fT = (fUMax - fVMin)/fSpeed;
            if ( fT < rfTLast )
                rfTLast = fT;

            // quick out: intersection before desired interval
            if ( rfTFirst > rfTLast )
                return false; 
        }
        else if ( fSpeed < (Real)0.0 )
        {
            // find last time of contact on this axis
            fT = (fUMin - fVMax)/fSpeed;
            if ( fT < rfTLast )
                rfTLast = fT;

            // quick out: intersection before desired interval
            if ( rfTFirst > rfTLast )
                return false;
        }
    }
    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::AxisFind (const Vector3<Real>& rkVelocity, 
    const Vector3<Real>& rkTestAxis, const ContactConfig<Real>& rkUC, 
    const ContactConfig<Real>& rkVC, ContactSide& rkSide,
    ContactConfig<Real>& rkTUC, ContactConfig<Real>& rkTVC, Real& rfTFirst,
    Real& rfTLast, Real fTMax)
{
    // Constant velocity separating axis test.  UC and VC are the "new"
    // potential configurations, and TUC and TVC are the best known
    // configurations

    Real fT;
    Real fSpeed = rkTestAxis.Dot(rkVelocity);

    if ( rkVC.m_fMax < rkUC.m_fMin ) // V on left of U
    {
        if ( fSpeed <= (Real)0.0 ) // V moving away from U
            return false;

        // find first time of contact on this axis
        fT = (rkUC.m_fMin - rkVC.m_fMax)/fSpeed;

        // If this is the new maximum first time of contact, set side and
        // configuration.
        if ( fT > rfTFirst )
        {
            rfTFirst = fT;
            rkSide = LEFT;
            rkTUC = rkUC;
            rkTVC = rkVC;
        }

        // quick out: intersection after desired interval
        if ( rfTFirst > fTMax )
            return false;

        // find last time of contact on this axis
        fT = (rkUC.m_fMax - rkVC.m_fMin)/fSpeed;
        if ( fT < fTMax )
            rfTLast = fT;

        // quick out: intersection before desired interval
        if ( rfTFirst > rfTLast )
            return false;
    }
    else if ( rkUC.m_fMax < rkVC.m_fMin )   // V on right of U
    {
        if ( fSpeed >= (Real)0.0 ) // V moving away from U
            return false;

        // find first time of contact on this axis
        fT = (rkUC.m_fMax - rkVC.m_fMin)/fSpeed;

        // If this is the new maximum first time of contact,  set side and
        // configuration.
        if ( fT > rfTFirst )
        {
            rfTFirst = fT;
            rkSide = RIGHT;
            rkTUC = rkUC;
            rkTVC = rkVC;
        }

        // quick out: intersection after desired interval
        if ( rfTFirst > fTMax )
            return false;   

        // find last time of contact on this axis
        fT = (rkUC.m_fMin - rkVC.m_fMax)/fSpeed;
        if ( fT < fTMax )
            rfTLast = fT;

        // quick out: intersection before desired interval
        if ( rfTFirst > rfTLast )
            return false;
    }
    else // V and U on overlapping interval
    {
        if ( fSpeed > (Real)0.0 )
        {
            // find last time of contact on this axis
            fT = (rkUC.m_fMax - rkVC.m_fMin)/fSpeed;
            if ( fT < rfTLast )
                rfTLast = fT;

            // quick out: intersection before desired interval
            if ( rfTFirst > rfTLast )
                return false; 
        }
        else if ( fSpeed < (Real)0.0 )
        {
            // find last time of contact on this axis
            fT = (rkUC.m_fMin - rkVC.m_fMax)/fSpeed;
            if ( fT < rfTLast )
                rfTLast = fT;

            // quick out: intersection before desired interval
            if ( rfTFirst > rfTLast )
                return false; 
        }
    }
    return true;
}
//----------------------------------------------------------------------------
template <class Real>
void Wml::ClipConvexPolygonAgainstPlane (const Vector3<Real>& rkNormal,
    Real fConstant, int& riQuantity, Vector3<Real>* akP)
{
    // The input vertices are assumed to be in counterclockwise order.  The
    // ordering is an invariant of this function.  The size of array akP is
    // assumed to be large enough to store the clipped polygon vertices.

    // test on which side of line are the vertices
    int iPositive = 0, iNegative = 0, iPIndex = -1;
    int iQuantity = riQuantity;

    Real afTest[8];
    int i;
    for (i = 0; i < riQuantity; i++)
    {

        // An epsilon is used here because it is possible for the dot product
        // and fC to be exactly equal to each other (in theory), but differ
        // slightly because of floating point problems.  Thus, add a little
        // to the test number to push actually equal numbers over the edge
        // towards the positive.

        // TO DO: This should probably be somehow a relative tolerance, and
        // I do not think multiplying by the constant is somehow the best
        // way to do this.
        afTest[i] = rkNormal.Dot(akP[i]) - fConstant +
            Math<Real>::FAbs(fConstant)*Math<Real>::EPSILON;

        if ( afTest[i] >= (Real)0.0 )
        {
            iPositive++;
            if ( iPIndex < 0 )
                iPIndex = i;
        }
        else
        {
            iNegative++;
        }
    }

    if ( riQuantity == 2 ) 
    {
        // Lines are a little different, in that clipping the segment
        // cannot create a new segment, as clipping a polygon would
        if ( iPositive > 0 )
        {
            if ( iNegative > 0 ) 
            {
                int iClip;

                if ( iPIndex == 0 )
                {
                    // vertex0 positive, vertex1 is clipped
                    iClip = 1;
                }
                else // iPIndex == 1
                {
                    // vertex1 positive, vertex0 clipped
                    iClip = 0;
                }

                Real fT = afTest[iPIndex]/(afTest[iPIndex] - afTest[iClip]);
                akP[iClip] = akP[iPIndex] + fT*(akP[iClip] - akP[iPIndex]);                
            }
            // otherwise both positive, no clipping
        }
        else
        {
            //assert( !"Entire line got clipped.  Shouldn't happen." );
            riQuantity = 0;
        }
    }
    else
    {
        if ( iPositive > 0 )
        {
            if ( iNegative > 0 )
            {
                // plane transversely intersects polygon
                Vector3<Real> akCV[8];
                int iCQuantity = 0, iCur, iPrv;
                Real fT;

                if ( iPIndex > 0 )
                {
                    // first clip vertex on line
                    iCur = iPIndex;
                    iPrv = iCur-1;
                    fT = afTest[iCur]/(afTest[iCur]-afTest[iPrv]);
                    akCV[iCQuantity++] = akP[iCur]+fT*(akP[iPrv]-akP[iCur]);

                    // vertices on positive side of line
                    while ( iCur < iQuantity && afTest[iCur] >= (Real)0.0 )
                        akCV[iCQuantity++] = akP[iCur++];

                    // last clip vertex on line
                    if ( iCur < iQuantity )
                    {
                        iPrv = iCur-1;
                    }
                    else
                    {
                        iCur = 0;
                        iPrv = iQuantity - 1;
                    }
                    fT = afTest[iCur]/(afTest[iCur]-afTest[iPrv]);
                    akCV[iCQuantity++] = akP[iCur]+fT*(akP[iPrv]-akP[iCur]);
                }
                else  // iPIndex is 0
                {
                    // vertices on positive side of line
                    iCur = 0;
                    while ( iCur < iQuantity && afTest[iCur] >= (Real)0.0 )
                        akCV[iCQuantity++] = akP[iCur++];

                    // last clip vertex on line
                    iPrv = iCur-1;
                    fT = afTest[iCur]/(afTest[iCur]-afTest[iPrv]);
                    akCV[iCQuantity++] = akP[iCur]+fT*(akP[iPrv]-akP[iCur]);

                    // skip vertices on negative side
                    while ( iCur < iQuantity && afTest[iCur] < (Real)0.0 )
                        iCur++;

                    // first clip vertex on line
                    if ( iCur < iQuantity )
                    {
                        iPrv = iCur-1;
                        fT = afTest[iCur]/(afTest[iCur] - afTest[iPrv]);
                        akCV[iCQuantity++] = akP[iCur]+fT*(akP[iPrv]-
                            akP[iCur]);

                        // vertices on positive side of line
                        while ( iCur < iQuantity
                        &&      afTest[iCur] >= (Real)0.0 )
                        {
                            akCV[iCQuantity++] = akP[iCur++];
                        }
                    }
                    else
                    {
                        // iCur = 0
                        iPrv = iQuantity - 1;
                        fT = afTest[0]/(afTest[0]-afTest[iPrv]);
                        akCV[iCQuantity++] = akP[0]+fT*(akP[iPrv]-akP[0]);
                    }
                }

                iQuantity = iCQuantity;
                memcpy(akP,akCV,iCQuantity*sizeof(Vector3<Real>));
            }
            // else polygon fully on positive side of plane, nothing to do

            riQuantity = iQuantity;
        }
        else
        {
            // Polygon does not intersect positive side of plane, clip all.
            // This should not ever happen if called by the findintersect
            // routines after an intersection has been determined.
            riQuantity = 0;
        }    
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM ContactConfig<float>;
template WML_ITEM bool AxisTest<float> (const Vector3<float>&,
    const Vector3<float>&, float, float, float, float, float&, float&,
    float);
template WML_ITEM bool AxisFind<float> (const Vector3<float>&, 
    const Vector3<float>&, const ContactConfig<float>&, 
    const ContactConfig<float>&, ContactSide&, ContactConfig<float>&,
    ContactConfig<float>&, float&, float&, float);
template WML_ITEM void ClipConvexPolygonAgainstPlane<float> (
    const Vector3<float>&, float, int&, Vector3<float>*);

template class WML_ITEM ContactConfig<double>;
template WML_ITEM bool AxisTest<double> (const Vector3<double>&,
    const Vector3<double>&, double, double, double, double, double&, double&,
    double);
template WML_ITEM bool AxisFind<double> (const Vector3<double>&, 
    const Vector3<double>&, const ContactConfig<double>&, 
    const ContactConfig<double>&, ContactSide&, ContactConfig<double>&,
    ContactConfig<double>&, double&, double&, double);
template WML_ITEM void ClipConvexPolygonAgainstPlane<double> (
    const Vector3<double>&, double, int&, Vector3<double>*);
}
//----------------------------------------------------------------------------
