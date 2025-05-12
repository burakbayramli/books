// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlCamera.h"
#include "WmlIntrBox3Fru3.h"
#include "WmlTerrainBlock.h"
#include "WmlTerrainPage.h"
#include "WmlTerrainVertex.h"
using namespace Wml;

//----------------------------------------------------------------------------
void TerrainBlock::GetVertex9 (unsigned short usSize,
    TerrainVertex* pkVOrigin, TerrainVertex* apkTVertex[9])
{
    unsigned short usOrigin = m_ucX+usSize*m_ucY;
    unsigned short usTwoStride = 2*m_ucStride;
    unsigned short usSizePlus1 = usSize+1;
    unsigned short usSizePlus2 = usSize+2;
    unsigned short usTwoSizePlus1 = 2*usSize+1;
    unsigned short usSizeTimesStride = usSize*m_ucStride;
    unsigned short usSizePlus1TimesStride = usSizePlus1*m_ucStride;
    unsigned short usSizePlus2TimesStride = usSizePlus2*m_ucStride;
    unsigned short usSizeTimesTwoStride = usSize*usTwoStride;
    unsigned short usTwoSizePlus1TimesStride = usTwoSizePlus1*m_ucStride;
    unsigned short usSizePlus1TimesTwoStride = usSizePlus1*usTwoStride;

    pkVOrigin += usOrigin;
    apkTVertex[0] = &pkVOrigin[0];
    apkTVertex[1] = &pkVOrigin[m_ucStride];
    apkTVertex[2] = &pkVOrigin[usTwoStride];
    apkTVertex[3] = &pkVOrigin[usSizeTimesStride];
    apkTVertex[4] = &pkVOrigin[usSizePlus1TimesStride];
    apkTVertex[5] = &pkVOrigin[usSizePlus2TimesStride];
    apkTVertex[6] = &pkVOrigin[usSizeTimesTwoStride];
    apkTVertex[7] = &pkVOrigin[usTwoSizePlus1TimesStride];
    apkTVertex[8] = &pkVOrigin[usSizePlus1TimesTwoStride];
}
//----------------------------------------------------------------------------
void TerrainBlock::Initialize (TerrainPage* pkPage, TerrainBlock* pkBlock,
    unsigned short usBlock, unsigned char ucX, unsigned char ucY,
    unsigned char ucStride, bool bEven)
{
    m_ucX = ucX;
    m_ucY = ucY;
    m_ucStride = ucStride;
    m_ucFlags = 0;
    m_fDeltaL = 0.0f;
    m_fDeltaH = Mathf::MAX_REAL;
    SetEven(bEven);

    TerrainVertex* akTVertex[9];
    GetVertex9(pkPage->m_usSize,pkPage->m_akTVertex,akTVertex);

    // set vertex dependencies
    akTVertex[1]->SetDependent(0,akTVertex[4]);
    akTVertex[3]->SetDependent(1,akTVertex[4]);
    akTVertex[5]->SetDependent(0,akTVertex[4]);
    akTVertex[7]->SetDependent(1,akTVertex[4]);
    if ( bEven )
    {
        akTVertex[4]->SetDependent(0,akTVertex[6]);
        akTVertex[4]->SetDependent(1,akTVertex[2]);
    }
    else
    {
        akTVertex[4]->SetDependent(0,akTVertex[0]);
        akTVertex[4]->SetDependent(1,akTVertex[8]);
    }

    // recurse on children (if any)
    if ( ucStride > 1 )
    {
        // child stride is half the parent stride
        unsigned char ucChildStride = ( ucStride >> 1 );

        // process child 00
        unsigned short usChild = GetChildIndex(usBlock,1);
        TerrainBlock* pkChild = &pkBlock[usChild];
        pkChild->Initialize(pkPage,pkBlock,usChild,ucX,ucY,ucChildStride,
            true);

        // process child 01
        usChild++;
        pkChild++;
        pkChild->Initialize(pkPage,pkBlock,usChild,ucX+ucStride,ucY,
            ucChildStride,false);

        // process child 10
        usChild++;
        pkChild++;
        pkChild->Initialize(pkPage,pkBlock,usChild,ucX,ucY+ucStride,
            ucChildStride,false);

        // process child 11
        usChild++;
        pkChild++;
        pkChild->Initialize(pkPage,pkBlock,usChild,ucX+ucStride,ucY+ucStride,
            ucChildStride,true);
    }
}
//----------------------------------------------------------------------------
void TerrainBlock::UpdateBoundingBox (TerrainPage* pkPage,
    TerrainBlock* pkBlock, unsigned short usBlock, unsigned char ucStride)
{
    Vector3f akPoint[9];
    akPoint[0].X() = pkPage->GetX(m_ucX);
    akPoint[0].Y() = pkPage->GetY(m_ucY);
    akPoint[1].X() = pkPage->GetX(m_ucX + m_ucStride);
    akPoint[1].Y() = akPoint[0].Y();
    akPoint[2].X() = pkPage->GetX(m_ucX + 2*m_ucStride);
    akPoint[2].Y() = akPoint[0].Y();
    akPoint[3].X() = akPoint[0].X();
    akPoint[3].Y() = pkPage->GetY(m_ucY + m_ucStride);
    akPoint[4].X() = akPoint[1].X();
    akPoint[4].Y() = akPoint[3].Y();
    akPoint[5].X() = akPoint[2].X();
    akPoint[5].Y() = akPoint[3].Y();
    akPoint[6].X() = akPoint[0].X();
    akPoint[6].Y() = pkPage->GetY(m_ucY + 2*m_ucStride);
    akPoint[7].X() = akPoint[1].X();
    akPoint[7].Y() = akPoint[6].Y();
    akPoint[8].X() = akPoint[2].X();
    akPoint[8].Y() = akPoint[6].Y();

    unsigned short usOrigin = m_ucX + pkPage->m_usSize*m_ucY;
    akPoint[0].Z() = pkPage->GetHeight(usOrigin);
    akPoint[1].Z() = pkPage->GetHeight(usOrigin+ucStride);
    akPoint[2].Z() = pkPage->GetHeight(usOrigin+2*ucStride);
    usOrigin += pkPage->m_usSize*ucStride;
    akPoint[3].Z() = pkPage->GetHeight(usOrigin);
    akPoint[4].Z() = pkPage->GetHeight(usOrigin+ucStride);
    akPoint[5].Z() = pkPage->GetHeight(usOrigin+2*ucStride);
    usOrigin += pkPage->m_usSize*ucStride;
    akPoint[6].Z() = pkPage->GetHeight(usOrigin);
    akPoint[7].Z() = pkPage->GetHeight(usOrigin+ucStride);
    akPoint[8].Z() = pkPage->GetHeight(usOrigin+2*ucStride);

    // compute (x,y) components of bounding box
    m_kMin.X() = akPoint[0].X();
    m_kMin.Y() = akPoint[0].Y();
    m_kMax.X() = akPoint[8].X();
    m_kMax.Y() = akPoint[8].Y();
    
    // compute delta max
    float fC0 = akPoint[0].Z();
    float fMd = akPoint[1].Z();
    float fC1 = akPoint[2].Z();
    m_fDelta[0] = 0.5f*(fC0+fC1)-fMd;
    m_fDelta[0] *= m_fDelta[0];
    
    fC0 = akPoint[8].Z();
    fMd = akPoint[5].Z();
    m_fDelta[1] = 0.5f*(fC0+fC1)-fMd;
    m_fDelta[1] *= m_fDelta[1];
    
    fMd = akPoint[7].Z();
    fC1 = akPoint[6].Z();
    m_fDelta[2] = 0.5f*(fC0+fC1)-fMd;
    m_fDelta[2] *= m_fDelta[2];
    
    fC0 = akPoint[0].Z();
    fMd = akPoint[3].Z();
    m_fDelta[3] = 0.5f*(fC0+fC1)-fMd;
    m_fDelta[3] *= m_fDelta[3];
    
    fMd = akPoint[4].Z();
    if ( GetEven() )
    {
        fC0 = akPoint[2].Z();
        fC1 = akPoint[6].Z();
    }
    else
    {
        fC0 = akPoint[0].Z();
        fC1 = akPoint[8].Z();
    }
    m_fDelta[4] = 0.5f*(fC0+fC1)-fMd;
    m_fDelta[4] *= m_fDelta[4];

    m_fDeltaMax = m_fDelta[0];
    for (int i = 1; i < 5; i++)
    {
        if ( m_fDelta[i] > m_fDeltaMax )
            m_fDeltaMax = m_fDelta[i];
    }

    // recurse on children (if any)
    if ( ucStride > 1 )
    {
        // child stride is half the parent stride
        unsigned char ucChildStride = ( ucStride >> 1 );

        // process child 00
        unsigned short usChild = GetChildIndex(usBlock,1);
        TerrainBlock* pkChild = &pkBlock[usChild];
        pkChild->UpdateBoundingBox(pkPage,pkBlock,usChild,ucChildStride);
        if ( pkChild->m_fDeltaMax > m_fDeltaMax )
            m_fDeltaMax = pkChild->m_fDeltaMax;
        m_kMin.Z() = pkChild->m_kMin.Z();
        m_kMax.Z() = pkChild->m_kMax.Z();

        // process child 01
        usChild++;
        pkChild++;
        pkChild->UpdateBoundingBox(pkPage,pkBlock,usChild,ucChildStride);
        if ( pkChild->m_fDeltaMax > m_fDeltaMax )
            m_fDeltaMax = pkChild->m_fDeltaMax;
        if ( pkChild->m_kMin.Z() < m_kMin.Z() )
            m_kMin.Z() = pkChild->m_kMin.Z();
        if ( pkChild->m_kMax.Z() > m_kMax.Z() )
            m_kMax.Z() = pkChild->m_kMax.Z();

        // process child 10
        usChild++;
        pkChild++;
        pkChild->UpdateBoundingBox(pkPage,pkBlock,usChild,ucChildStride);
        if ( pkChild->m_fDeltaMax > m_fDeltaMax )
            m_fDeltaMax = pkChild->m_fDeltaMax;
        if ( pkChild->m_kMin.Z() < m_kMin.Z() )
            m_kMin.Z() = pkChild->m_kMin.Z();
        if ( pkChild->m_kMax.Z() > m_kMax.Z() )
            m_kMax.Z() = pkChild->m_kMax.Z();

        // process child 11
        usChild++;
        pkChild++;
        pkChild->UpdateBoundingBox(pkPage,pkBlock,usChild,ucChildStride);
        if ( pkChild->m_fDeltaMax > m_fDeltaMax )
            m_fDeltaMax = pkChild->m_fDeltaMax;
        if ( pkChild->m_kMin.Z() < m_kMin.Z() )
            m_kMin.Z() = pkChild->m_kMin.Z();
        if ( pkChild->m_kMax.Z() > m_kMax.Z() )
            m_kMax.Z() = pkChild->m_kMax.Z();
    }
    else
    {
        // compute z components of bounding box at leaf node of quadtree
        m_kMin.Z() = akPoint[0].Z();
        m_kMax.Z() = m_kMin.Z();
        for (unsigned short usIndex = 1; usIndex < 9; usIndex++)
        {
            float fZ = akPoint[usIndex].Z();
            if ( fZ < m_kMin.Z() )
                m_kMin.Z() = fZ;
            if ( fZ > m_kMax.Z() )
                m_kMax.Z() = fZ;
        }
    }
}
//----------------------------------------------------------------------------
void TerrainBlock::ComputeInterval (const Vector3f& rkModelEye,
    float fTolerance)
{
    // distant terrain assumption

    if ( fTolerance > 0.0f )
    {
        // compute hmin = |eye.Z() - clamp(bmin.Z(),eye.Z(),bmax.Z())|
        float fHMin;
        if ( rkModelEye.Z() < m_kMin.Z() )
        {
            fHMin = rkModelEye.Z() - m_kMin.Z();
            fHMin *= fHMin;
        }
        else if ( rkModelEye.Z() > m_kMax.Z() )
        {
            fHMin = rkModelEye.Z() - m_kMax.Z();
            fHMin *= fHMin;
        }
        else
        {
            fHMin = 0.0f;
        }

        // compute hmax = max{|eye.Z() - bmin.Z()|,|eye.Z() - bmax.Z()|}
        float fHMax = rkModelEye.Z() - m_kMin.Z();
        fHMax *= fHMax;
        float fTmp = rkModelEye.Z() - m_kMax.Z();
        fTmp *= fTmp;
        if ( fTmp > fHMax )
            fHMax = fTmp;

        // compute rmin and rmax
        float fDxMin = rkModelEye.X() - m_kMin.X();
        float fDxMax = rkModelEye.X() - m_kMax.X();
        float fDyMin = rkModelEye.Y() - m_kMin.Y();
        float fDyMax = rkModelEye.Y() - m_kMax.Y();
        fDxMin *= fDxMin;
        fDxMax *= fDxMax;
        fDyMin *= fDyMin;
        fDyMax *= fDyMax;
        float fRMin = 0.0f, fRMax = 0.0f;

        if ( rkModelEye.X() < m_kMin.X() )
        {
            fRMin += fDxMin;
            fRMax += fDxMax;
        }
        else if ( rkModelEye.X() <= m_kMax.X() )
        {
            fRMax += ( fDxMax >= fDxMin ? fDxMax : fDxMin );
        }
        else
        {
            fRMin += fDxMax;
            fRMax += fDxMin;
        }

        if ( rkModelEye.Y() < m_kMin.Y() )
        {
            fRMin += fDyMin;
            fRMax += fDyMax;
        }
        else if ( rkModelEye.Y() <= m_kMax.Y() )
        {
            fRMax += ( fDyMax >= fDyMin ? fDyMax : fDyMin );
        }
        else
        {
            fRMin += fDyMax;
            fRMax += fDyMin;
        }

        // compute fmin
        float fDenom = fRMin + fHMax;
        float fFMin =
            (fDenom > 0.0f ? fRMin/(fDenom*fDenom) : Mathf::MAX_REAL);
        fDenom = fRMax + fHMax;
        fTmp = (fDenom > 0.0f ? fRMax/(fDenom*fDenom) : Mathf::MAX_REAL);
        if ( fTmp < fFMin )
            fFMin = fTmp;
        
        // compute fmax
        float fFMax;
        if ( fRMin >= fHMin )
        {
            fDenom = fRMin + fHMin;
            fFMax = (fDenom > 0.0f ? fRMin/(fDenom*fDenom) : Mathf::MAX_REAL);
        }
        else if ( fRMax <= fHMin )
        {
            fDenom = fRMax + fHMin;
            fFMax = (fDenom > 0.0f ? fRMax/(fDenom*fDenom) : Mathf::MAX_REAL);
        }
        else
        {
            fFMax = (fHMin > 0.0f ? 0.25f/fHMin : Mathf::MAX_REAL);
        }
        
        m_fDeltaL = fTolerance/fFMax;
        m_fDeltaH = (fFMin > 0.0f ? fTolerance/fFMin : Mathf::MAX_REAL);
    }
    else  // fTolerance == 0
    {
        m_fDeltaL = 0.0f;
        m_fDeltaH = Mathf::MAX_REAL;
    }
}
//----------------------------------------------------------------------------
void TerrainBlock::ComputeInterval (const Vector3f& rkModelEye,
    const Vector3f& rkModelDir, float fTolerance, Vector2f& rkLoc,
    float fSpacing)
{
    // close terrain assumption

    if ( fTolerance > 0.0f )
    {
        // compute fmin and fmax

        // temporary quantities
        float fLmEx = rkLoc.X() - rkModelEye.X();
        float fLmEy = rkLoc.Y() - rkModelEye.Y();
        float fTmp = fSpacing*float(m_ucStride);
        float fXSum = fLmEx + fTmp;
        float fYSum = fLmEy + fTmp;
        float fXDif = fLmEx - fTmp;
        float fYDif = fLmEy - fTmp;

        // find corners of block closest to and farthest from eye
        float fFMin = fabsf(rkModelDir.X()*fXDif + rkModelDir.Y()*fYDif);
        float fFMax = fFMin;

        fTmp = fabsf(rkModelDir.X()*fXSum + rkModelDir.Y()*fYDif);
        if ( fTmp >= fFMin )
            fFMin = fTmp;
        else 
            fFMax = fTmp;

        fTmp = fabsf(rkModelDir.X()*fXSum + rkModelDir.Y()*fYSum);
        if ( fTmp >= fFMin )
            fFMin = fTmp;
        else 
            fFMax = fTmp;

        fTmp = fabsf(rkModelDir.X()*fXDif + rkModelDir.Y()*fYSum);
        if ( fTmp >= fFMin )
            fFMin = fTmp;
        else
            fFMax = fTmp;

        m_fDeltaL = fTolerance*fFMax;
        m_fDeltaH = fTolerance*fFMin;
    }
    else  // fTolerance == 0
    {
        m_fDeltaL = 0.0f;
        m_fDeltaH = Mathf::MAX_REAL;
    }
}
//----------------------------------------------------------------------------
void TerrainBlock::TestIntersectFrustum (const TerrainPage* pkPage,
    const Camera* pkCamera)
{
    SetVisibilityTested(true);

    // axis-aligned bounding box of page in terrain model space
    Vector3f kMCenter = 0.5f*(m_kMin + m_kMax);
    Vector3f kMExtent = 0.5f*(m_kMax - m_kMin);

    // transform to world space (parent's coordinate system)
    Box3f kWBox;
    kWBox.Center() = pkPage->Scale()*(pkPage->Rotate()*kMCenter) +
        pkPage->Translate();
    int i;
    for (i = 0; i < 3; i++)
    {
        kWBox.Axis(i) = pkPage->Rotate().GetColumn(i);
        kWBox.Extent(i) = pkPage->Scale()*kMExtent[i];
    }

    // inverse transform from world space to camera space
    Box3f kCBox;
    Vector3f kDiff = kWBox.Center() - pkCamera->GetLocation();
    kCBox.Center() = Vector3f(kDiff.Dot(pkCamera->GetLeft()),
        kDiff.Dot(pkCamera->GetUp()),kDiff.Dot(pkCamera->GetDirection()));
    for (i = 0; i < 3; i++)
    {
        kCBox.Axis(i) = Vector3f(
            pkCamera->GetLeft().Dot(kWBox.Axis(i)),
            pkCamera->GetUp().Dot(kWBox.Axis(i)),
            pkCamera->GetDirection().Dot(kWBox.Axis(i)));
        kCBox.Extent(i) = kWBox.Extent(i);
    }

    // default frustum matches camera coordinate frame
    Frustum3f kFrustum;
    kFrustum.LBound() = pkCamera->GetFrustumRight();
    kFrustum.UBound() = pkCamera->GetFrustumTop();
    kFrustum.DMin() = pkCamera->GetFrustumNear();
    kFrustum.DMax() = pkCamera->GetFrustumFar();
    kFrustum.Update();

    SetVisible(TestIntersection(kCBox,kFrustum));
}
//----------------------------------------------------------------------------
void TerrainBlock::SimplifyVertices (TerrainPage* pkPage,
    const Vector3f& rkModelEye, const Vector3f& rkModelDir, float fTolerance,
    bool bCloseAssumption)
{
    unsigned short usSize = pkPage->m_usSize;
    unsigned short usOrigin = m_ucX + usSize*m_ucY;
    TerrainVertex* pkVOrigin = pkPage->m_akTVertex + usOrigin;
    TerrainVertex* pkTVertex;
    unsigned short usOffset;
    Vector3f kDiff;
    float fRSqr, fLenSqr, fDistDir;

    // simplify at (stride,0)
    if ( m_fDeltaL <= m_fDelta[0] )
    {
        pkTVertex = pkVOrigin + m_ucStride;
        if ( m_fDelta[0] <= m_fDeltaH )
        {
            // test vertex delta against tolerance
            if ( !pkTVertex->GetEnabled() )
            {
                kDiff.X() = rkModelEye.X() - pkPage->GetX(m_ucX+m_ucStride);
                kDiff.Y() = rkModelEye.Y() - pkPage->GetY(m_ucY);
                if ( bCloseAssumption ) 
                {
                    fDistDir = rkModelDir.X()*kDiff.X() +
                        rkModelDir.Y()*kDiff.Y();
                    if ( m_fDelta[0] > fTolerance*fDistDir*fDistDir )
                        pkTVertex->Enable();
                }
                else // distant assumption
                {
                    kDiff.Z() = rkModelEye.Z() -
                        pkPage->GetHeight(usOrigin+m_ucStride);
                    fRSqr = kDiff.X()*kDiff.X() + kDiff.Y()*kDiff.Y();
                    fLenSqr = fRSqr + kDiff.Z()*kDiff.Z();
                    if ( m_fDelta[0]*fRSqr > fTolerance*fLenSqr*fLenSqr )
                        pkTVertex->Enable();
                }
            }
        }
        else
        {
            if ( !pkTVertex->GetEnabled() )
                pkTVertex->Enable();
        }
    }

    // simplify at (2*stride,stride)
    if ( m_fDeltaL <= m_fDelta[1] )
    {
        usOffset = (usSize+2)*m_ucStride;
        pkTVertex = pkVOrigin + usOffset;
        if ( m_fDelta[1] <= m_fDeltaH )
        {
            // test vertex delta against tolerance
            if ( !pkTVertex->GetEnabled() )
            {
                kDiff.X() = rkModelEye.X() - pkPage->GetX(m_ucX+2*m_ucStride);
                kDiff.Y() = rkModelEye.Y() - pkPage->GetY(m_ucY+m_ucStride);
                if ( bCloseAssumption ) 
                {
                    fDistDir = rkModelDir.X()*kDiff.X() +
                        rkModelDir.Y()*kDiff.Y();
                    if ( m_fDelta[1] > fTolerance*fDistDir*fDistDir )
                        pkTVertex->Enable();
                }
                else // distant assumption
                {
                    kDiff.Z() = rkModelEye.Z() -
                        pkPage->GetHeight(usOrigin+usOffset);
                    fRSqr = kDiff.X()*kDiff.X() + kDiff.Y()*kDiff.Y();
                    fLenSqr = fRSqr + kDiff.Z()*kDiff.Z();
                    if ( m_fDelta[1]*fRSqr > fTolerance*fLenSqr*fLenSqr )
                        pkTVertex->Enable();
                }
            }
        }
        else
        {
            if ( !pkTVertex->GetEnabled() )
            {
                pkTVertex->Enable();
            }
        }
    }

    // simplify at (stride,2*stride)
    if ( m_fDeltaL <= m_fDelta[2] )
    {
        usOffset = (2*usSize+1)*m_ucStride;
        pkTVertex = pkVOrigin + usOffset;
        if ( m_fDelta[2] <= m_fDeltaH )
        {
            // test vertex delta against tolerance
            if ( !pkTVertex->GetEnabled() )
            {
                kDiff.X() = rkModelEye.X() - pkPage->GetX(m_ucX+m_ucStride);
                kDiff.Y() = rkModelEye.Y() - pkPage->GetY(m_ucY+2*m_ucStride);
                if ( bCloseAssumption ) 
                {
                    fDistDir = rkModelDir.X()*kDiff.X() +
                        rkModelDir.Y()*kDiff.Y();
                    if ( m_fDelta[2] > fTolerance*fDistDir*fDistDir )
                        pkTVertex->Enable();
                }
                else // distant assumption
                {
                    kDiff.Z() = rkModelEye.Z() -
                        pkPage->GetHeight(usOrigin+usOffset);
                    fRSqr = kDiff.X()*kDiff.X() + kDiff.Y()*kDiff.Y();
                    fLenSqr = fRSqr + kDiff.Z()*kDiff.Z();
                    if ( m_fDelta[2]*fRSqr > fTolerance*fLenSqr*fLenSqr )
                        pkTVertex->Enable();
                }
            }
        }
        else
        {
            if ( !pkTVertex->GetEnabled() )
                pkTVertex->Enable();
        }
    }

    // simplify at (0,stride)
    if ( m_fDeltaL <= m_fDelta[3] )
    {
        usOffset = usSize*m_ucStride;
        pkTVertex = pkVOrigin + usOffset;
        if ( m_fDelta[3] <= m_fDeltaH )
        {
            // test vertex delta against tolerance
            if ( !pkTVertex->GetEnabled() )
            {
                kDiff.X() = rkModelEye.X() - pkPage->GetX(m_ucX);
                kDiff.Y() = rkModelEye.Y() - pkPage->GetY(m_ucY+m_ucStride);
                if ( bCloseAssumption ) 
                {
                    fDistDir = rkModelDir.X()*kDiff.X() +
                        rkModelDir.Y()*kDiff.Y();
                    if ( m_fDelta[3] > fTolerance*fDistDir*fDistDir )
                        pkTVertex->Enable();
                }
                else // distant terrain assumption
                {
                    kDiff.Z() = rkModelEye.Z() -
                        pkPage->GetHeight(usOrigin+usOffset);
                    fRSqr = kDiff.X()*kDiff.X() + kDiff.Y()*kDiff.Y();
                    fLenSqr = fRSqr + kDiff.Z()*kDiff.Z();
                    if ( m_fDelta[3]*fRSqr > fTolerance*fLenSqr*fLenSqr )
                        pkTVertex->Enable();
                }
            }
        }
        else
        {
            if ( !pkTVertex->GetEnabled() )
                pkTVertex->Enable();
        }
    }

    // simplify at (stride,stride)
    if ( m_fDeltaL <= m_fDelta[4] )
    {
        usOffset = (usSize+1)*m_ucStride;
        pkTVertex = pkVOrigin + usOffset;
        if ( m_fDelta[4] <= m_fDeltaH )
        {
            // test vertex delta against tolerance
            if ( !pkTVertex->GetEnabled() )
            {
                kDiff.X() = rkModelEye.X() - pkPage->GetX(m_ucX+m_ucStride);
                kDiff.Y() = rkModelEye.Y() - pkPage->GetY(m_ucY+m_ucStride);
                if ( bCloseAssumption ) 
                {
                    fDistDir = rkModelDir.X()*kDiff.X() +
                        rkModelDir.Y()*kDiff.Y();
                    if ( m_fDelta[4] > fTolerance*fDistDir*fDistDir )
                        pkTVertex->Enable();
                }
                else // distant terrain assumption
                {
                    kDiff.Z() = rkModelEye.Z() -
                        pkPage->GetHeight(usOrigin+usOffset);
                    fRSqr = kDiff.X()*kDiff.X() + kDiff.Y()*kDiff.Y();
                    fLenSqr = fRSqr + kDiff.Z()*kDiff.Z();
                    if ( m_fDelta[4]*fRSqr > fTolerance*fLenSqr*fLenSqr )
                        pkTVertex->Enable();
                }
            }
        }
        else
        {
            if ( !pkTVertex->GetEnabled() )
                pkTVertex->Enable();
        }
    }

    // enable the corner vertices
    if ( GetEven() )
    {
        pkTVertex = pkVOrigin + 2*m_ucStride;
        if ( !pkTVertex->GetEnabled() )
            pkTVertex->Enable();

        pkTVertex = pkVOrigin + 2*usSize*m_ucStride;
        if ( !pkTVertex->GetEnabled() )
            pkTVertex->Enable();
    }
    else
    {
        pkTVertex = pkVOrigin;
        if ( !pkTVertex->GetEnabled() )
            pkTVertex->Enable();

        pkTVertex = pkVOrigin + 2*(usSize+1)*m_ucStride;
        if ( !pkTVertex->GetEnabled() )
            pkTVertex->Enable();
    }
}
//----------------------------------------------------------------------------
void TerrainBlock::Disable (TerrainPage* pkPage)
{
    TerrainVertex* akTVertex[9];
    GetVertex9(pkPage->m_usSize,pkPage->m_akTVertex,akTVertex);
    for (int i = 0; i < 9; i++)
    {
        if ( akTVertex[i]->GetEnabled() )
            akTVertex[i]->Disable();
    }
}
//----------------------------------------------------------------------------
