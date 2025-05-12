// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlContMinEllipseCR2.h"
using namespace Wml;

#include <algorithm>
#include <vector>
using namespace std;

//----------------------------------------------------------------------------
template <class Real>
static void MaxProduct (int iQuantity, vector<Vector2<Real> >& rkA, Real& rfX,
    Real& rfY)
{
    // Keep track of which constraint lines have already been used in the
    // search.
    bool* abUsed = new bool[iQuantity];
    memset(abUsed,0,iQuantity*sizeof(bool));

    // Find the constraint line whose y-intercept (0,ymin) is closest to the
    // origin.  This line contributes to the convex hull of the constraints
    // and the search for the maximum starts here.  Also find the constraint
    // line whose x-intercept (xmin,0) is closest to the origin.  This line
    // contributes to the convex hull of the constraints and the search for
    // the maximum terminates before or at this line.
    int i, iYMin = -1;
#ifdef _DEBUG
    int iXMin = -1;
#endif
    Real fAXMax = (Real)0.0, fAYMax = (Real)0.0;  // A[i] >= (0,0) by design
    for (i = 0; i < iQuantity; i++)
    {
        // The minimum x-intercept is 1/A[iXMin].X() for A[iXMin].X() the
        // maximum of the A[i].X().
        if ( rkA[i].X() > fAXMax )
        {
            fAXMax = rkA[i].X();
#ifdef _DEBUG
            iXMin = i;
#endif
        }

        // The minimum y-intercept is 1/A[iYMin].Y() for A[iYMin].Y() the
        // maximum of the A[i].Y().
        if ( rkA[i].Y() > fAYMax )
        {
            fAYMax = rkA[i].Y();
            iYMin = i;
        }
    }
    assert( iXMin != -1 && iYMin != -1 );
    abUsed[iYMin] = true;

    // The convex hull is searched in a clockwise manner starting with the
    // constraint line constructed above.  The next vertex of the hull occurs
    // as the closest point to the first vertex on the current constraint
    // line.  The following loop finds each consecutive vertex.
    Real fX0 = (Real)0.0, fXMax = ((Real)1.0)/fAXMax;
    int j;
    for (j = 0; j < iQuantity; j++)
    {
        // Find the line whose intersection with the current line is closest
        // to the last hull vertex.  The last vertex is at (x0,y0) on the
        // current line.
        Real fX1 = fXMax;
        int iLine = -1;
        for (i = 0; i < iQuantity; i++)
        {
            if ( !abUsed[i] )
            {
                // This line not yet visited, process it.  Given current
                // constraint line a0*x+b0*y =1 and candidate line
                // a1*x+b1*y = 1, find the point of intersection.  The
                // determinant of the system is d = a0*b1-a1*b0.  We only
                // care about lines that have more negative slope than the
                // previous one, that is, -a1/b1 < -a0/b0, in which case we
                // process only lines for which d < 0.
                Real fDet = rkA[iYMin].Kross(rkA[i]);
                if ( fDet < (Real)0.0 )  // TO DO.  Need epsilon test here?
                {
                    // Compute the x-value for the point of intersection,
                    // (x1,y1).  There may be floating point error issues in
                    // the comparision 'rfX <= fX1'.  Consider modifying to
                    // 'rfX <= fX1+epsilon'.
                    rfX = (rkA[i].Y()-rkA[iYMin].Y())/fDet;
                    if ( fX0 < rfX && rfX <= fX1)
                    {
                        iLine = i;
                        fX1 = rfX;
                    }
                }
            }
        }

        // Next vertex is at (x1,y1) whose x-value was computed above.  First
        // check for the maximum of x*y on the current line for x in [x0,x1].
        // On this interval the function is f(x) = x*(1-a0*x)/b0.  The
        // derivative is f'(x) = (1-2*a0*x)/b0 and f'(r) = 0 when
        // r = 1/(2*a0).  The three candidates for the maximum are f(x0),
        // f(r), and f(x1).  Comparisons are made between r and the end points
        // x0 and x1.  Since a0 = 0 is possible (constraint line is horizontal
        // and f is increasing on line), the division in r is not performed
        // and the comparisons are made between 1/2 = a0*r and a0*x0 or a0*x1.

        // compare r < x0
        if ( (Real)0.5 < rkA[iYMin].X()*fX0 )
        {
            // The maximum is f(x0) since the quadratic f decreases for
            // x > r.
            rfX = fX0;
            rfY = ((Real)1.0-rkA[iYMin].X()*rfX)/rkA[iYMin].Y();  // = f(x0)
            break;
        }

        // compare r < x1
        if ( (Real)0.5 < rkA[iYMin].X()*fX1 )
        {
            // The maximum is f(r).  The search ends here because the
            // current line is tangent to the level curve of f(x)=f(r)
            // and x*y can therefore only decrease as we traverse further
            // around the hull in the clockwise direction.
            rfX = ((Real)0.5)/rkA[iYMin].X();
            rfY = ((Real)0.5)/rkA[iYMin].Y();  // = f(r)
            break;
        }

        // The maximum is f(x1).  The function x*y is potentially larger
        // on the next line, so continue the search.
        assert( iLine != -1 );
        fX0 = fX1;
        fX1 = fXMax;
        abUsed[iLine] = true;
        iYMin = iLine;
    }

    assert( j < iQuantity );

    delete[] abUsed;
}
//----------------------------------------------------------------------------
template <class Real>
void Wml::MinEllipseCR2 (int iQuantity, const Vector2<Real>* akPoint,
    const Vector2<Real>& rkC, const Matrix2<Real>& rkR, Real afD[2])
{
    // Given center C and orientation R, finds minimum volume ellipsoid
    // (X-C)^t R^t D R (X-C) = 1 where D is a diagonal matrix whose diagonal
    // entries are positive.  The problem is equivalent to maximizing the
    // product D[0]*D[1] given C and R and subject to the constraints
    // (P[i]-C)^t R^t D R (P[i]-C) <= 1 for all input points P[i] with
    // 0 <= i < N.  Each constraint has form A.X()*D[0]+A.Y()*D[1] <= 1
    // where A.X() >= 0 and A.Y() >= 0.

    vector<Vector2<Real> > kA(iQuantity);
    for (int i = 0; i < iQuantity; i++)
    {
        Vector2<Real> kDiff = akPoint[i] - rkC;
        Vector2<Real> kProd = rkR*kDiff;
        kA[i].X() = kProd.X()*kProd.X();
        kA[i].Y() = kProd.Y()*kProd.Y();
    }

    // Lexicographical sort, (x0,y0) > (x1,y1) if x0 > x1 or if x0 = x1 and
    // y0 > y1.  Remove all but first entry in blocks with x0 = x1 since the
    // corresponding constraint lines for the first entry "hides" all the
    // others from the origin.
    sort(kA.begin(),kA.end());
    typename vector<Vector2<Real> >::iterator pkEnd =
        unique(kA.begin(),kA.end());
    kA.erase(pkEnd,kA.end());

    // Lexicographical sort, (x0,y0) > (x1,y1) if y0 > y1 or if y0 = y1 and
    // x0 > x1.  Remove all but first entry in blocks with y0 = y1 since the
    // corresponding constraint lines for the first entry "hides" all the
    // others from the origin.
    sort(kA.begin(),kA.end());
    pkEnd = unique(kA.begin(),kA.end());
    kA.erase(pkEnd,kA.end());

    MaxProduct((int)kA.size(),kA,afD[0],afD[1]);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM void MinEllipseCR2<float> (int,
    const Vector2<float>*, const Vector2<float>&, const Matrix2<float>&,
    float afD[2]);

template WML_ITEM void MinEllipseCR2<double> (int,
    const Vector2<double>*, const Vector2<double>&, const Matrix2<double>&,
    double afD[2]);
}
//----------------------------------------------------------------------------
