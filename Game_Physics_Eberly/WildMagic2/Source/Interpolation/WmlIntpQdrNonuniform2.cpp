// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntpQdrNonuniform2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
IntpQdrNonuniform2<Real>::IntpQdrNonuniform2 (int iVertexQuantity,
    Vector2<Real>* akVertex, Real* afF, Real* afFx, Real* afFy)
    :
    Delaunay2<Real>(iVertexQuantity,akVertex)
{
    assert( afF && afFx && afFy );
    m_afF = afF;
    m_afFx = afFx;
    m_afFy = afFy;
    ProcessTriangles();
}
//----------------------------------------------------------------------------
template <class Real>
IntpQdrNonuniform2<Real>::IntpQdrNonuniform2 (int iVertexQuantity,
    Vector2<Real>* akVertex, Real* afF)
    :
    Delaunay2<Real>(iVertexQuantity,akVertex)
{
    assert( afF );
    m_afF = afF;
    EstimateDerivatives();
    ProcessTriangles();
}
//----------------------------------------------------------------------------
template <class Real>
IntpQdrNonuniform2<Real>::IntpQdrNonuniform2 (Delaunay2<Real>& rkNet,
    Real* afF, Real* afFx, Real* afFy)
    :
    Delaunay2<Real>(rkNet)
{
    assert( afF && afFx && afFy );
    m_afF = afF;
    m_afFx = afFx;
    m_afFy = afFy;
    ProcessTriangles();
}
//----------------------------------------------------------------------------
template <class Real>
IntpQdrNonuniform2<Real>::IntpQdrNonuniform2 (Delaunay2<Real>& rkNet,
    Real* afF)
    :
    Delaunay2<Real>(rkNet)
{
    assert( afF );
    m_afF = afF;
    EstimateDerivatives();
    ProcessTriangles();
}
//----------------------------------------------------------------------------
template <class Real>
IntpQdrNonuniform2<Real>::~IntpQdrNonuniform2 ()
{
    delete[] m_afF;
    delete[] m_afFx;
    delete[] m_afFy;
    delete[] m_akTData;
    delete[] m_akECenter;
}
//----------------------------------------------------------------------------
template <class Real>
void IntpQdrNonuniform2<Real>::EstimateDerivatives ()
{
    m_afFx = new Real[m_iVertexQuantity];
    m_afFy = new Real[m_iVertexQuantity];
    Real* afFz = new Real[m_iVertexQuantity];
    memset(m_afFx,0,m_iVertexQuantity*sizeof(Real));
    memset(m_afFy,0,m_iVertexQuantity*sizeof(Real));
    memset(afFz,0,m_iVertexQuantity*sizeof(Real));

    // accumulate normals at spatial locations (averaging process)
    int i;
    for (i = 0; i < m_iTriangleQuantity; i++)
    {
        typename Delaunay2<Real>::Triangle& rkTri = m_akTriangle[i];

        // get three vertices of triangle
        int j0 = rkTri.m_aiVertex[0];
        int j1 = rkTri.m_aiVertex[1];
        int j2 = rkTri.m_aiVertex[2];

        // compute normal vector of triangle (with positive z-component)
        Real fDx1 = m_akVertex[j1].X() - m_akVertex[j0].X();
        Real fDy1 = m_akVertex[j1].Y() - m_akVertex[j0].Y();
        Real fDz1 = m_afF[j1] - m_afF[j0];
        Real fDx2 = m_akVertex[j2].X() - m_akVertex[j0].X();
        Real fDy2 = m_akVertex[j2].Y() - m_akVertex[j0].Y();
        Real fDz2 = m_afF[j2] - m_afF[j0];
        Real fNx = fDy1*fDz2 - fDy2*fDz1;
        Real fNy = fDz1*fDx2 - fDz2*fDx1;
        Real fNz = fDx1*fDy2 - fDx2*fDy1;
        if ( fNz < (Real)0.0 )
        {
            fNx = -fNx;
            fNy = -fNy;
            fNz = -fNz;
        }

        m_afFx[j0] += fNx;  m_afFy[j0] += fNy;  afFz[j0] += fNz;
        m_afFx[j1] += fNx;  m_afFy[j1] += fNy;  afFz[j1] += fNz;
        m_afFx[j2] += fNx;  m_afFy[j2] += fNy;  afFz[j2] += fNz;
    }

    // scale the normals to form (x,y,-1)
    for (i = 0; i < m_iVertexQuantity; i++)
    {
        if ( Math<Real>::FAbs(afFz[i]) > Math<Real>::EPSILON )
        {
            Real fInv = -((Real)1.0)/afFz[i];
            m_afFx[i] *= fInv;
            m_afFy[i] *= fInv;
        }
        else
        {
            m_afFx[i] = (Real)0.0;
            m_afFy[i] = (Real)0.0;
        }
    }

    delete[] afFz;
}
//----------------------------------------------------------------------------
template <class Real>
void IntpQdrNonuniform2<Real>::ProcessTriangles ()
{
    // Add degenerate triangles to boundary triangles so that interpolation
    // at the boundary can be treated in the same way as interpolation in
    // the interior.

    // add quadratic data to triangle network
    m_akTData = new TriangleData[m_iTriangleQuantity];
    m_akECenter = new Vector2<Real>[m_iExtraTriangleQuantity];

    // compute centers of inscribed circles for triangles
    int iT;
    for (iT = 0; iT < m_iTriangleQuantity; iT++)
    {
        typename Delaunay2<Real>::Triangle& rkTri = m_akTriangle[iT];
        Delaunay2<Real>::ComputeInscribedCenter(
            m_akVertex[rkTri.m_aiVertex[0]],
            m_akVertex[rkTri.m_aiVertex[1]],
            m_akVertex[rkTri.m_aiVertex[2]],
            m_akTData[iT].m_kCenter);
    }

    // compute centers of edges on convex hull
    int iE = 0;
    for (iT = 0; iT < m_iTriangleQuantity; iT++)
    {
        typename Delaunay2<Real>::Triangle& rkTri = m_akTriangle[iT];
        for (int j = 0; j < 3; j++)
        {
            if ( rkTri.m_aiAdjacent[j] >= m_iTriangleQuantity )
            {
                m_akECenter[iE] = ((Real)0.5)*(
                    m_akVertex[rkTri.m_aiVertex[j]] +
                    m_akVertex[rkTri.m_aiVertex[(j+1) % 3]]);
                iE++;
            }
        }
    }

    // compute cross-edge intersections
    for (iT = 0; iT < m_iTriangleQuantity; iT++)
        ComputeCrossEdgeIntersections(iT);

    // compute Bezier coefficients
    for (iT = 0; iT < m_iTriangleQuantity; iT++)
        ComputeCoefficients(iT);
}
//----------------------------------------------------------------------------
template <class Real>
void IntpQdrNonuniform2<Real>::ComputeCrossEdgeIntersections (int iT)
{
    typename Delaunay2<Real>::Triangle& rkTri = m_akTriangle[iT];
    const Vector2<Real>& rkV0 = m_akVertex[rkTri.m_aiVertex[0]];
    const Vector2<Real>& rkV1 = m_akVertex[rkTri.m_aiVertex[1]];
    const Vector2<Real>& rkV2 = m_akVertex[rkTri.m_aiVertex[2]];

    Vector2<Real> akU[3];
    for (int i = 0; i < 3; i++)
    {
        int iA = rkTri.m_aiAdjacent[i];
        if ( iA < m_iTriangleQuantity )
            akU[i] = m_akTData[iA].m_kCenter;
        else
            akU[i] = m_akECenter[iA - m_iTriangleQuantity];
    }

    Real fM00, fM01, fM10, fM11, fR0, fR1, fInvDet;

    // intersection on edge <V0,V1>
    fM00 = rkV0.Y() - rkV1.Y();
    fM01 = rkV1.X() - rkV0.X();
    fM10 = m_akTData[iT].m_kCenter.Y() - akU[0].Y();
    fM11 = akU[0].X() - m_akTData[iT].m_kCenter.X();
    fR0  = fM00*rkV0.X() + fM01*rkV0.Y();
    fR1  = fM10*m_akTData[iT].m_kCenter.X() +
        fM11*m_akTData[iT].m_kCenter.Y();
    fInvDet = ((Real)1.0)/(fM00*fM11 - fM01*fM10);
    m_akTData[iT].m_akIntersect[0].X() = (fM11*fR0-fM01*fR1)*fInvDet;
    m_akTData[iT].m_akIntersect[0].Y() = (fM00*fR1-fM10*fR0)*fInvDet;

    // intersection on edge <V1,V2>
    fM00 = rkV1.Y() - rkV2.Y();
    fM01 = rkV2.X() - rkV1.X();
    fM10 = m_akTData[iT].m_kCenter.Y() - akU[1].Y();
    fM11 = akU[1].X() - m_akTData[iT].m_kCenter.X();
    fR0  = fM00*rkV1.X() + fM01*rkV1.Y();
    fR1  = fM10*m_akTData[iT].m_kCenter.X() +
        fM11*m_akTData[iT].m_kCenter.Y();
    fInvDet = ((Real)1.0)/(fM00*fM11 - fM01*fM10);
    m_akTData[iT].m_akIntersect[1].X() = (fM11*fR0-fM01*fR1)*fInvDet;
    m_akTData[iT].m_akIntersect[1].Y() = (fM00*fR1-fM10*fR0)*fInvDet;

    // intersection on edge <V0,V2>
    fM00 = rkV0.Y() - rkV2.Y();
    fM01 = rkV2.X() - rkV0.X();
    fM10 = m_akTData[iT].m_kCenter.Y() - akU[2].Y();
    fM11 = akU[2].X() - m_akTData[iT].m_kCenter.X();
    fR0  = fM00*rkV0.X() + fM01*rkV0.Y();
    fR1  = fM10*m_akTData[iT].m_kCenter.X() +
        fM11*m_akTData[iT].m_kCenter.Y();
    fInvDet = ((Real)1.0)/(fM00*fM11 - fM01*fM10);
    m_akTData[iT].m_akIntersect[2].X() = (fM11*fR0-fM01*fR1)*fInvDet;
    m_akTData[iT].m_akIntersect[2].Y() = (fM00*fR1-fM10*fR0)*fInvDet;
}
//----------------------------------------------------------------------------
template <class Real>
void IntpQdrNonuniform2<Real>::ComputeCoefficients (int iT)
{
    typename Delaunay2<Real>::Triangle& rkTri = m_akTriangle[iT];
    TriangleData& rkTData = m_akTData[iT];

    // get sample data at main triangle vertices
    Vector2<Real> akV[3];
    Jet afJet[3];
    int i;
    for (i = 0; i < 3; i++)
    {
        int j = rkTri.m_aiVertex[i];
        akV[i] = m_akVertex[j];
        afJet[i].m_fF = m_afF[j];
        afJet[i].m_fFx = m_afFx[j];
        afJet[i].m_fFy = m_afFy[j];
    }

    Vector2<Real> akU[3];
    for (i = 0; i < 3; i++)
    {
        int iA = rkTri.m_aiAdjacent[i];
        if ( iA < m_iTriangleQuantity )
            akU[i] = m_akTData[iA].m_kCenter;
        else
            akU[i] = m_akECenter[iA - m_iTriangleQuantity];
    }

    // compute intermediate terms
    Real afCenT[3], afCen0[3], afCen1[3], afCen2[3];

    ComputeBarycenter(akV[0],akV[1],akV[2],rkTData.m_kCenter,afCenT);
    ComputeBarycenter(akV[0],akV[1],akV[2],akU[0],afCen0);
    ComputeBarycenter(akV[0],akV[1],akV[2],akU[1],afCen1);
    ComputeBarycenter(akV[0],akV[1],akV[2],akU[2],afCen2);

    Real fAlpha = (afCenT[1]*afCen1[0]-afCenT[0]*afCen1[1]) /
        (afCen1[0]-afCenT[0]);
    Real fBeta = (afCenT[2]*afCen2[1]-afCenT[1]*afCen2[2]) /
        (afCen2[1]-afCenT[1]);
    Real fGamma = (afCenT[0]*afCen0[2]-afCenT[2]*afCen0[0]) /
        (afCen0[2]-afCenT[2]);
    Real fOmAlpha = (Real)1.0 - fAlpha;
    Real fOmBeta  = (Real)1.0 - fBeta;
    Real fOmGamma = (Real)1.0 - fGamma;

    Real fTmp, afA[9], afB[9];

    fTmp = afCenT[0]*akV[0].X()+afCenT[1]*akV[1].X()+afCenT[2]*akV[2].X();
    afA[0] = ((Real)0.5)*(fTmp-akV[0].X());
    afA[1] = ((Real)0.5)*(fTmp-akV[1].X());
    afA[2] = ((Real)0.5)*(fTmp-akV[2].X());
    afA[3] = ((Real)0.5)*fBeta*(akV[2].X()-akV[0].X());
    afA[4] = ((Real)0.5)*fOmGamma*(akV[1].X()-akV[0].X());
    afA[5] = ((Real)0.5)*fGamma*(akV[0].X()-akV[1].X());
    afA[6] = ((Real)0.5)*fOmAlpha*(akV[2].X()-akV[1].X());
    afA[7] = ((Real)0.5)*fAlpha*(akV[1].X()-akV[2].X());
    afA[8] = ((Real)0.5)*fOmBeta*(akV[0].X()-akV[2].X());

    fTmp = afCenT[0]*akV[0].Y()+afCenT[1]*akV[1].Y()+afCenT[2]*akV[2].Y();
    afB[0] = ((Real)0.5)*(fTmp-akV[0].Y());
    afB[1] = ((Real)0.5)*(fTmp-akV[1].Y());
    afB[2] = ((Real)0.5)*(fTmp-akV[2].Y());
    afB[3] = ((Real)0.5)*fBeta*(akV[2].Y()-akV[0].Y());
    afB[4] = ((Real)0.5)*fOmGamma*(akV[1].Y()-akV[0].Y());
    afB[5] = ((Real)0.5)*fGamma*(akV[0].Y()-akV[1].Y());
    afB[6] = ((Real)0.5)*fOmAlpha*(akV[2].Y()-akV[1].Y());
    afB[7] = ((Real)0.5)*fAlpha*(akV[1].Y()-akV[2].Y());
    afB[8] = ((Real)0.5)*fOmBeta*(akV[0].Y()-akV[2].Y());

    // compute Bezier coefficients
    rkTData.m_afCoeff[ 2] = afJet[0].m_fF;
    rkTData.m_afCoeff[ 4] = afJet[1].m_fF;
    rkTData.m_afCoeff[ 6] = afJet[2].m_fF;

    rkTData.m_afCoeff[14] = afJet[0].m_fF + afA[0]*afJet[0].m_fFx +
        afB[0]*afJet[0].m_fFy;

    rkTData.m_afCoeff[ 7] = afJet[0].m_fF + afA[3]*afJet[0].m_fFx +
        afB[3]*afJet[0].m_fFy;

    rkTData.m_afCoeff[ 8] = afJet[0].m_fF + afA[4]*afJet[0].m_fFx +
        afB[4]*afJet[0].m_fFy;

    rkTData.m_afCoeff[16] = afJet[1].m_fF + afA[1]*afJet[1].m_fFx +
        afB[1]*afJet[1].m_fFy;

    rkTData.m_afCoeff[ 9] = afJet[1].m_fF + afA[5]*afJet[1].m_fFx +
        afB[5]*afJet[1].m_fFy;

    rkTData.m_afCoeff[10] = afJet[1].m_fF + afA[6]*afJet[1].m_fFx +
        afB[6]*afJet[1].m_fFy;

    rkTData.m_afCoeff[18] = afJet[2].m_fF + afA[2]*afJet[2].m_fFx +
        afB[2]*afJet[2].m_fFy;

    rkTData.m_afCoeff[11] = afJet[2].m_fF + afA[7]*afJet[2].m_fFx +
        afB[7]*afJet[2].m_fFy;

    rkTData.m_afCoeff[12] = afJet[2].m_fF + afA[8]*afJet[2].m_fFx +
        afB[8]*afJet[2].m_fFy;

    rkTData.m_afCoeff[ 5] = fAlpha*rkTData.m_afCoeff[10] +
        fOmAlpha*rkTData.m_afCoeff[11];

    rkTData.m_afCoeff[17] = fAlpha*rkTData.m_afCoeff[16] +
        fOmAlpha*rkTData.m_afCoeff[18];

    rkTData.m_afCoeff[ 1] = fBeta*rkTData.m_afCoeff[12] +
        fOmBeta*rkTData.m_afCoeff[ 7];

    rkTData.m_afCoeff[13] = fBeta*rkTData.m_afCoeff[18] +
        fOmBeta*rkTData.m_afCoeff[14];

    rkTData.m_afCoeff[ 3] = fGamma*rkTData.m_afCoeff[ 8] +
        fOmGamma*rkTData.m_afCoeff[ 9];

    rkTData.m_afCoeff[15] = fGamma*rkTData.m_afCoeff[14] +
        fOmGamma*rkTData.m_afCoeff[16]; 

    rkTData.m_afCoeff[ 0] = afCenT[0]*rkTData.m_afCoeff[14] + 
        afCenT[1]*rkTData.m_afCoeff[16] + afCenT[2]*rkTData.m_afCoeff[18];
}
//----------------------------------------------------------------------------
template <class Real>
bool IntpQdrNonuniform2<Real>::Evaluate (const Vector2<Real>& rkPoint,
    Real& rfF, Real& rfFx, Real& rfFy)
{
    // determine which triangle contains the target point
    Vector2<Real> kV0, kV1, kV2;
    int i;
    for (i = 0; i < m_iTriangleQuantity; i++)
    {
        typename Delaunay2<Real>::Triangle& rkTri = m_akTriangle[i];
        kV0 = m_akVertex[rkTri.m_aiVertex[0]];
        kV1 = m_akVertex[rkTri.m_aiVertex[1]];
        kV2 = m_akVertex[rkTri.m_aiVertex[2]];

        if ( InTriangle(kV0,kV1,kV2,rkPoint) )
            break;
    }
    if ( i == m_iTriangleQuantity )
    {
        // point is outside interpolation region
        return false;
    }

    // the input point is in this triangle
    typename Delaunay2<Real>::Triangle& rkTri = m_akTriangle[i];
    TriangleData& rkTData = m_akTData[i];

    // determine which of the six subtriangles contains the target point
    Vector2<Real> kSub0 = rkTData.m_kCenter;
    Vector2<Real> kSub1;
    Vector2<Real> kSub2 = rkTData.m_akIntersect[2];
    int iIndex;
    for (iIndex = 1; iIndex <= 6; iIndex++)
    {
        kSub1 = kSub2;
        if ( iIndex % 2 )
            kSub2 = m_akVertex[rkTri.m_aiVertex[iIndex/2]];
        else
            kSub2 = rkTData.m_akIntersect[iIndex/2-1];

        if ( InTriangle(kSub0,kSub1,kSub2,rkPoint) )
            break;
    }

    // This should not happen theoretically, but it can happen due to
    // numerical round-off errors.  Just in case, select an index and go
    // with it.  Probably better is to keep track of the dot products in
    // InTriangle and find the one closest to zero and use a triangle that
    // contains the edge as the one that contains the input point.
    assert( iIndex <= 6 );
    if ( iIndex > 6 )
        iIndex = 1;

    // compute barycentric coordinates with respect to subtriangle
    Real afBary[3];
    ComputeBarycenter(kSub0,kSub1,kSub2,rkPoint,afBary);

    // fetch Bezier control points
    Real afBez[6] =
    {
        rkTData.m_afCoeff[0],
        rkTData.m_afCoeff[12 + iIndex],
        rkTData.m_afCoeff[13 + (iIndex % 6)],
        rkTData.m_afCoeff[iIndex],
        rkTData.m_afCoeff[6 + iIndex],
        rkTData.m_afCoeff[1 + (iIndex % 6)]
    };

    // evaluate Bezier quadratic
    rfF = afBary[0]*(afBez[0]*afBary[0] + afBez[1]*afBary[1] +
        afBez[2]*afBary[2]) + afBary[1]*(afBez[1]*afBary[0] +
        afBez[3]*afBary[1] + afBez[4]*afBary[2]) + afBary[2]*(
        afBez[2]*afBary[0] + afBez[4]*afBary[1] + afBez[5]*afBary[2]);

    // evaluate barycentric derivatives of F
    Real fFu = ((Real)2.0)*(afBez[0]*afBary[0] + afBez[1]*afBary[1] +
        afBez[2]*afBary[2]);
    Real fFv = ((Real)2.0)*(afBez[1]*afBary[0] + afBez[3]*afBary[1] +
        afBez[4]*afBary[2]);
    Real fFw = ((Real)2.0)*(afBez[2]*afBary[0] + afBez[4]*afBary[1] +
        afBez[5]*afBary[2]);
    Real fDuw = fFu - fFw;
    Real fDvw = fFv - fFw;

    // convert back to (x,y) coordinates
    Real fM00 = kSub0.X() - kSub2.X();
    Real fM10 = kSub0.Y() - kSub2.Y();
    Real fM01 = kSub1.X() - kSub2.X();
    Real fM11 = kSub1.Y() - kSub2.Y();
    Real fInvDet = ((Real)1.0)/(fM00*fM11 - fM10*fM01);

    rfFx = fInvDet*(fM11*fDuw - fM10*fDvw);
    rfFy = fInvDet*(fM00*fDvw - fM01*fDuw);

    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM IntpQdrNonuniform2<float>;
template class WML_ITEM IntpQdrNonuniform2<double>;
}
//----------------------------------------------------------------------------
