// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlConvexPolygon2.h"
using namespace Wml;

#include <fstream>
using namespace std;

//----------------------------------------------------------------------------
template <class Real>
ConvexPolygon2<Real>::ConvexPolygon2 ()
{
}
//----------------------------------------------------------------------------
template <class Real>
ConvexPolygon2<Real>::ConvexPolygon2 (const VArray& rakPoint)
{
    Create(rakPoint);
}
//----------------------------------------------------------------------------
template <class Real>
ConvexPolygon2<Real>::ConvexPolygon2 (const VArray& rakPoint,
    const LArray& rakLine)
{
    Create(rakPoint,rakLine);
}
//----------------------------------------------------------------------------
template <class Real>
ConvexPolygon2<Real>::ConvexPolygon2 (const ConvexPolygon2& rkPoly)
    :
    m_akPoint(rkPoly.m_akPoint),
    m_akLine(rkPoly.m_akLine)
{
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexPolygon2<Real>::Create (const VArray& rakPoint)
{
    assert( rakPoint.size() >= 3 );
    m_akPoint = rakPoint;
    UpdateLines();
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexPolygon2<Real>::Create (const VArray& rakPoint,
    const LArray& rakLine)
{
    assert( rakPoint.size() >= 3 );
    m_akPoint = rakPoint;
    m_akLine = rakLine;
    ComputeCentroid();
}
//----------------------------------------------------------------------------
template <class Real>
const typename ConvexPolygon2<Real>::VArray&
ConvexPolygon2<Real>::GetPoints () const
{
    return m_akPoint;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector2<Real>& ConvexPolygon2<Real>::GetPoint (int iV) const
{
    assert( 0 <= iV && iV < (int)m_akPoint.size() );
    return m_akPoint[iV];
}
//----------------------------------------------------------------------------
template <class Real>
int ConvexPolygon2<Real>::AddPoint (const Vector2<Real>& rkPoint)
{
    int iLocation = (int)m_akPoint.size();
    m_akPoint.push_back(rkPoint);
    return iLocation;
}
//----------------------------------------------------------------------------
template <class Real>
typename ConvexPolygon2<Real>::VArray& ConvexPolygon2<Real>::Points ()
{
    return m_akPoint;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>& ConvexPolygon2<Real>::Point (int iV)
{
    assert( 0 <= iV && iV < (int)m_akPoint.size() );
    return m_akPoint[iV];
}
//----------------------------------------------------------------------------
template <class Real>
const typename ConvexPolygon2<Real>::LArray&
ConvexPolygon2<Real>::GetLines () const
{
    return m_akLine;
}
//----------------------------------------------------------------------------
template <class Real>
const Line2<Real>& ConvexPolygon2<Real>::GetLine (int iT) const
{
    assert( 0 <= iT && iT < (int)m_akLine.size() );
    return m_akLine[iT];
}
//----------------------------------------------------------------------------
template <class Real>
const Vector2<Real>& ConvexPolygon2<Real>::GetCentroid () const
{
    return m_kCentroid;
}
//----------------------------------------------------------------------------
template <class Real>
ConvexPolygon2<Real>& ConvexPolygon2<Real>::operator= (
    const ConvexPolygon2& rkPoly)
{
    m_akPoint = rkPoly.m_akPoint;
    m_akLine = rkPoly.m_akLine;
    m_kCentroid = rkPoly.m_kCentroid;
    return *this;
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexPolygon2<Real>::UpdateLines ()
{
    m_akLine.resize(m_akPoint.size());
    ComputeCentroid();

    // compute lines with inner pointing normals
    int iQuantity = (int)m_akPoint.size();
    for (int i0 = iQuantity-1, i1 = 0; i1 < iQuantity; i0 = i1++)
    {
        Vector2<Real> kDiff = m_kCentroid - m_akPoint[i0];
        Vector2<Real> kDir = m_akPoint[i1] - m_akPoint[i0];

        Real fKross = kDir.Kross(kDiff);
        if ( fKross > (Real)0.0 )
            m_akLine[i1].Normal() = -kDir.Perp();
        else
            m_akLine[i1].Normal() = kDir.Perp();

        m_akLine[i1].Normal().Normalize();
        m_akLine[i1].Constant() = m_akPoint[i0].Dot(m_akLine[i1].Normal());
    }
}
//----------------------------------------------------------------------------
template <class Real>
bool ConvexPolygon2<Real>::ValidateHalfSpaceProperty (Real fThreshold) const
{
    Real fMax = -Math<Real>::MAX_REAL, fMin = Math<Real>::MAX_REAL;
    for (int j = 0; j < (int)m_akLine.size(); j++)
    {
        const Line2<Real>& rkLine = m_akLine[j];
        for (int i = 0; i < (int)m_akPoint.size(); i++)
        {
            Real fDistance = rkLine.GetPseudodistance(m_akPoint[i]);
            if ( fDistance < fMin )
                fMin = fDistance;
            if ( fDistance > fMax )
                fMax = fDistance;
            if ( fDistance < fThreshold )
                return false;
        }
    }

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexPolygon2<Real>::ComputeCentroid ()
{
    m_kCentroid = Vector2<Real>::ZERO;
    for (int i = 0; i < (int)m_akPoint.size(); i++)
        m_kCentroid += m_akPoint[i];
    m_kCentroid /= (Real)m_akPoint.size();
}
//----------------------------------------------------------------------------
template <class Real>
Real ConvexPolygon2<Real>::GetPerimeterLength () const
{
    Real fLength = (Real)0.0;
    int iQuantity = (int)m_akPoint.size();
    for (int i0 = iQuantity-1, i1 = 0; i1 < iQuantity; i0 = i1++)
    {
        Vector2<Real> kDir = m_akPoint[i1] - m_akPoint[i0];
        fLength += kDir.Length();
    }
    return fLength;
}
//----------------------------------------------------------------------------
template <class Real>
Real ConvexPolygon2<Real>::GetArea () const
{
    int iQuantity = (int)m_akPoint.size();
    int iQm1 = iQuantity-1;
    Real fArea = m_akPoint[iQm1].X()*(m_akPoint[0].Y() -
        m_akPoint[iQm1-1].Y());
    for (int i0 = iQm1, i1 = 0, i2 = 1; i1 < iQm1; i0 = i1, i1 = i2++)
        fArea += m_akPoint[i1].X()*(m_akPoint[i2].Y() - m_akPoint[i0].Y());
    fArea *= (Real)0.5;
    return fArea;
}
//----------------------------------------------------------------------------
template <class Real>
bool ConvexPolygon2<Real>::ContainsPoint (const Vector2<Real>& rkP) const
{
    for (int i = 0; i < (int)m_akLine.size(); i++)
    {
        Real fDistance = m_akLine[i].GetPseudodistance(rkP);
        if ( fDistance < (Real)0.0 )
            return false;
    }
    return true;
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexPolygon2<Real>::CreateEggShape (const Vector2<Real>& rkCenter,
    Real fX0, Real fX1, Real fY0, Real fY1, int iMaxSteps,
    ConvexPolygon2& rkEgg)
{
    assert( fX0 > (Real)0.0 && fX1 > (Real)0.0 );
    assert( fY0 > (Real)0.0 && fY1 > (Real)0.0 );
    assert( 0 <= iMaxSteps && iMaxSteps < 16 );

    // Start with a quadrilateral whose 4 vertices are (-x0,0), (x1,0),
    // (0,-y0), (0,y1).  The egg has 4*2^s vertices where s is the number of
    // steps.  Store the initial vertices at indices 0, 2^s, 2*s^2, 3*2^s to
    // allow subdivision in-place.
    int iStride = (1 << iMaxSteps);
    int iQuantity = 4*iStride;
    VArray akPoint(iQuantity);
    akPoint[0] = Vector2<Real>(fX1,(Real)0.0);
    akPoint[iStride] = Vector2<Real>((Real)0.0,fY1);
    akPoint[2*iStride] = Vector2<Real>(-fX0,(Real)0.0);
    akPoint[3*iStride] = Vector2<Real>((Real)0.0,-fY0);

    // subdivide the edges
    for (int iStep = 1; iStep <= iMaxSteps; iStep++, iStride >>= 1)
    {
        for (int i0 = 0, i1 = iStride; i0 < iQuantity; i0 = i1, i1 += iStride)
        {
            Vector2<Real>& rkSub = akPoint[(i0+i1)/2];
            if ( i1 < iQuantity )
                rkSub = akPoint[i0] + akPoint[i1];
            else
                rkSub = akPoint[i0] + akPoint[0];
            Real fXR = (rkSub.X() > (Real)0.0 ? rkSub.X()/fX1 :
                rkSub.X()/fX0);
            Real fYR = (rkSub.Y() > (Real)0.0 ? rkSub.Y()/fY1 :
                rkSub.Y()/fY0);
            rkSub *= Math<Real>::InvSqrt(fXR*fXR+fYR*fYR);
        }
    }

    // translate egg to the appropriate center
    for (int i = 0; i < (int)akPoint.size(); i++)
        akPoint[i] += rkCenter;

    rkEgg.Create(akPoint);
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexPolygon2<Real>::Print (ofstream& rkOStr) const
{
    int i;
    char acMsg[512];

    rkOStr << "points:" << endl;
    for (i = 0; i < (int)m_akPoint.size(); i++)
    {
        const Vector2<Real>& rkV = m_akPoint[i];
        sprintf(acMsg,"point<%d> = (%+8.4f,%+8.4f)",i,rkV.X(),rkV.Y());
        rkOStr << acMsg << endl;
    }
    rkOStr << endl;

    rkOStr << "lines:" << endl;
    for (i = 0; i < (int)m_akLine.size(); i++)
    {
        const Line2<Real>& rkL = m_akLine[i];
        sprintf(acMsg,"line<%d> = (%+8.6f,%+8.6f;%+8.4f)",i,rkL.Normal().X(),
            rkL.Normal().Y(),rkL.Constant());
        rkOStr << acMsg << endl;
    }
    rkOStr << endl;
}
//----------------------------------------------------------------------------
template <class Real>
bool ConvexPolygon2<Real>::Print (const char* acFilename) const
{
    ofstream kOStr(acFilename);
    if ( !kOStr )
        return false;

    Print(kOStr);
    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool ConvexPolygon2<Real>::Clip (const Line2<Real>& rkLine,
    ConvexPolygon2& rkIntr) const
{
    // compute signed distances from vertices to line
    const Real fEpsilon = (Real)1e-05;
    int iQuantity = (int)m_akPoint.size();
    Real* afDistance = new Real[iQuantity];
    int iPositive = 0, iNegative = 0, iPIndex = -1;
    for (int i = 0; i < iQuantity; i++)
    {
        afDistance[i] = rkLine.GetPseudodistance(m_akPoint[i]);
        if ( afDistance[i] >= fEpsilon )
        {
            iPositive++;
            if ( iPIndex < 0 )
                iPIndex = i;
        }
        else if ( afDistance[i] <= -fEpsilon )
        {
            iNegative++;
        }
        else
        {
            // The point is on the line (within floating point tolerance).
            afDistance[i] = (Real)0.0;
        }
    }

    if ( iPositive == 0 )
    {
        // polygon is in negative half-space, fully clipped
        delete[] afDistance;
        return false;
    }

    if ( iNegative == 0 )
    {
        // polygon is in positive half-space, fully visible
        rkIntr = *this;
        delete[] afDistance;
        return true;
    }

    // line transversely intersects polygon
    VArray kCV;
    int iCur, iPrv;
    Real fT;

    if ( iPIndex > 0 )
    {
        // first clip vertex on line
        iCur = iPIndex;
        iPrv = iCur-1;
        fT = afDistance[iCur]/(afDistance[iCur]-afDistance[iPrv]);
        kCV.push_back(m_akPoint[iCur]+fT*(m_akPoint[iPrv]-m_akPoint[iCur]));

        // vertices on positive side of line
        while ( iCur < iQuantity && afDistance[iCur] > (Real)0.0 )
            kCV.push_back(m_akPoint[iCur++]);

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
        fT = afDistance[iCur]/(afDistance[iCur]-afDistance[iPrv]);
        kCV.push_back(m_akPoint[iCur]+fT*(m_akPoint[iPrv]-m_akPoint[iCur]));
    }
    else  // iPIndex is 0
    {
        // vertices on positive side of line
        iCur = 0;
        while ( iCur < iQuantity && afDistance[iCur] > (Real)0.0 )
            kCV.push_back(m_akPoint[iCur++]);

        // last clip vertex on line
        iPrv = iCur-1;
        fT = afDistance[iCur]/(afDistance[iCur]-afDistance[iPrv]);
        kCV.push_back(m_akPoint[iCur]+fT*(m_akPoint[iPrv]-m_akPoint[iCur]));

        // skip vertices on negative side
        while ( iCur < iQuantity && afDistance[iCur] <= (Real)0.0 )
            iCur++;

        // first clip vertex on line
        if ( iCur < iQuantity )
        {
            iPrv = iCur-1;
            fT = afDistance[iCur]/(afDistance[iCur]-afDistance[iPrv]);
            kCV.push_back(m_akPoint[iCur]+fT*(m_akPoint[iPrv] -
                m_akPoint[iCur]));

            // vertices on positive side of line
            while ( iCur < iQuantity && afDistance[iCur] > (Real)0.0 )
                kCV.push_back(m_akPoint[iCur++]);
        }
        else
        {
            // iCur = 0
            iPrv = iQuantity - 1;
            fT = afDistance[0]/(afDistance[0]-afDistance[iPrv]);
            kCV.push_back(m_akPoint[0]+fT*(m_akPoint[iPrv]-m_akPoint[0]));
        }
    }

    delete[] afDistance;
    rkIntr.Create(kCV);
    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool ConvexPolygon2<Real>::FindIntersection (const ConvexPolygon2& rkPoly,
    ConvexPolygon2& rkIntr) const
{
    rkIntr = *this;

    const LArray& rakLine = rkPoly.GetLines();
    for (int i = 0; i < (int)rakLine.size(); i++)
    {
        if ( !rkIntr.Clip(rakLine[i],rkIntr) )
            return false;
    }

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
ConvexPolygon2<Real>* ConvexPolygon2<Real>::FindSolidIntersection (
    const ConvexPolygon2& rkPoly0, const ConvexPolygon2& rkPoly1)
{
    ConvexPolygon2* pkIntr = new ConvexPolygon2;
    if ( rkPoly0.FindIntersection(rkPoly1,*pkIntr) )
        return pkIntr;
    
    // As curves, the polygons do not intersect.  However, as solids, one
    // polygon might be fully contained in the other.
    if ( rkPoly0.ContainsPoint(rkPoly1.GetCentroid()) )
    {
        *pkIntr = rkPoly1;
        return pkIntr;
    }
        
    if ( rkPoly1.ContainsPoint(rkPoly0.GetCentroid()) )
    {
        *pkIntr = rkPoly0;
        return pkIntr;
    }

    delete pkIntr;
    return NULL;
}
//----------------------------------------------------------------------------
template <class Real>
void ConvexPolygon2<Real>::FindAllIntersections (int iQuantity,
    ConvexPolygon2* akPoly, int& riCombos, ConvexPolygon2**& rapkIntr)
{
    // Only 2^16 possible combinations for intersections are currently
    // supported.  If you need more, then GetHighBit(int) must be modified
    // to handle more than 16-bit inputs.
    if ( iQuantity <= 0 || iQuantity > 16 )
    {
        riCombos = 0;
        rapkIntr = NULL;
        return;
    }

    riCombos = (1 << iQuantity);
    bool* abNeedsTesting = new bool[riCombos];
    rapkIntr = new ConvexPolygon2*[riCombos];
    int i;
    for (i = 0; i < riCombos; i++)
    {
        abNeedsTesting[i] = true;
        rapkIntr[i] = NULL;
    }

    // trivial cases, zero or one polyhedron--already the intersection
    abNeedsTesting[0] = false;
    for (i = 0; i < iQuantity; i++)
    {
        int j = (1 << i);
        abNeedsTesting[j] = false;
        rapkIntr[j] = new ConvexPolygon2(akPoly[i]);
    }
    
    for (i = 3; i < riCombos; i++)
    {
        if ( abNeedsTesting[i] )
        {
            // In binary, i = b[m]...b[0] where b[m] is not zero (the
            // high-order bit.  Also, i1 = b[m-1]...b[0] is not zero
            // since, if it were, we would have ruled out the combination
            // by the j-loop below.  Therefore, i0 = b[m]0...0 and
            // i1 correspond to already existing polyhedra.  The
            // intersection finding just needs to look at the intersection
            // of the two polyhedra.
            int i0 = GetHighBit(i);
            int i1 = i & ~i0;
            rapkIntr[i] = FindSolidIntersection(*rapkIntr[i0],*rapkIntr[i1]);
            if ( !rapkIntr[i] )
            {
                // No intersection for this combination.  No need to test
                // other combinations that include this one.
                for (int j = 0; j < riCombos; j++)
                {
                    if ( (i & j) == i )
                        abNeedsTesting[j] = false;
                }
            }
#ifdef _DEBUG
            else  // test if well-formed convex polyhedron
            {
                Vector2<Real> kCentroid = rapkIntr[i]->GetCentroid();
                bool bContains = rapkIntr[i]->ContainsPoint(kCentroid);
                assert( bContains );
            }
#endif
        }
    }

    delete[] abNeedsTesting;
}
//----------------------------------------------------------------------------
template <class Real>
int ConvexPolygon2<Real>::GetHighBit (int i)
{
    // assert: i in [1,2^16].  That is, (i>0) && (0xFFFF0000&i)==0.
    // This is a binary search for the high-order bit of i.
    if ( (i & 0xFF00) != 0 )
    {
        if ( (i & 0xF000) != 0 )
        {
            if ( (i & 0xC000) != 0 )
            {
                if ( (i & 0x8000) != 0 )
                    return 0x8000;
                else // (i & 0x4000) != 0
                    return 0x4000;
            }
            else  // (i & 0x3000) != 0
            {
                if ( (i & 0x2000) != 0 )
                    return 0x2000;
                else  // (i & 0x1000) != 0
                    return 0x1000;
            }
        }
        else  // (i & 0x0F00) != 0
        {
            if ( (i & 0x0C00) != 0 )
            {
                if ( (i & 0x0800) != 0 )
                    return 0x0800;
                else  // (i & 0x0400) != 0
                    return 0x0400;
            }
            else  // (i & 0x0300) != 0
            {
                if ( (i & 0x0200) != 0 )
                    return 0x0200;
                else  // (i & 0x0100) != 0
                    return 0x0100;
            }
        }
    }
    else  // (i & 0x00FF)
    {
        if ( (i & 0x00F0) != 0 )
        {
            if ( (i & 0x00C0) != 0 )
            {
                if ( (i & 0x0080) != 0 )
                    return 0x0080;
                else  // (i & 0x0040) != 0
                    return 0x0040;
            }
            else  // (i & 0x0030) != 0
            {
                if ( (i & 0x0020) != 0 )
                    return 0x0020;
                else  // (i & 0x0010) != 0
                    return 0x0010;
            }
        }
        else  // (i & 0x000F) != 0
        {
            if ( (i & 0x000C) != 0 )
            {
                if ( (i & 0x0008) != 0 )
                    return 0x0008;
                else  // (i & 0x0004) != 0
                    return 0x0004;
            }
            else  // (i & 0x0003) != 0
            {
                if ( (i & 0x0002) != 0 )
                    return 0x0002;
                else  // (i & 0x0001) != 0
                    return 0x0001;
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
template class WML_ITEM ConvexPolygon2<float>;
template class WML_ITEM ConvexPolygon2<double>;
}
//----------------------------------------------------------------------------
