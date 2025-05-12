// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlRenderer.h"
#include "WmlTerrainBlock.h"
#include "WmlTerrainPage.h"
#include "WmlTerrainVertex.h"
using namespace Wml;

WmlImplementRTTI(TerrainPage,TriMesh);
WmlImplementStream(TerrainPage);

//----------------------------------------------------------------------------
TerrainPage::TerrainPage (unsigned short usSize, unsigned short* ausHeight,
    const Vector2f& rkOrigin, float fMinElevation, float fMaxElevation,
    float fSpacing)
    :
    TriMesh(
        usSize*usSize,  // vertex quantity
        new Vector3f[usSize*usSize],  // vertices,
        0,  // no normals
        0,  // no colors
        new Vector2f[usSize*usSize],  // always use textures
        2*(usSize-1)*(usSize-1),  // triangle quantity
        new int[6*(usSize-1)*(usSize-1)])  // connectivity
{
    // usSize = 2^p + 1, p <= 7
    assert( usSize ==  3 || usSize ==  5 || usSize ==   9 || usSize == 17
        ||  usSize == 33 || usSize == 65 || usSize == 129 );

    // native data
    m_usSize = usSize;
    m_ausHeight = ausHeight;
    m_kOrigin = rkOrigin;
    m_fMinElevation = fMinElevation;
    m_fMaxElevation = fMaxElevation;
    m_fSpacing = fSpacing;

    InitializeDerivedData();
}
//----------------------------------------------------------------------------
TerrainPage::TerrainPage ()
    :
    m_kOrigin(Vector2f::ZERO)
{
    // native data
    m_usSize = 0;
    m_ausHeight = NULL;
    m_fMinElevation = 0.0f;
    m_fMaxElevation = 0.0f;
    m_fSpacing = 0.0f;

    // derived data
    m_usSizeM1 = 0;
    m_fPixelTolerance = 0.0f;
    m_fWorldTolerance = 0.0f;
    m_fInvSpacing = 0.0f;
    m_fTextureSpacing = 0.0f;
    m_fMultiplier = 0.0f;
    m_bNeedsTessellation = false;
    m_ausLookup = NULL;
    m_iConnectLength = 0;
    m_akTVertex = NULL;
    m_usBlockQuantity = 0;
    m_akBlock = NULL;
    m_usQueueQuantity = 0;
    m_ausQueue = NULL;
    m_usFront = 0;
    m_usRear = 0;
    m_usItemsInQueue = 0;
}
//----------------------------------------------------------------------------
TerrainPage::~TerrainPage ()
{
    delete[] m_ausHeight;
    delete[] m_akTVertex;
    delete[] m_akBlock;
    delete[] m_ausQueue;
    delete[] m_ausLookup;
}
//----------------------------------------------------------------------------
void TerrainPage::InitializeDerivedData ()
{
    m_usSizeM1 = m_usSize - 1;
    m_fPixelTolerance = 1.0f;
    m_fWorldTolerance = -1.0f;
    m_fInvSpacing = 1.0f/m_fSpacing;
    m_fTextureSpacing = 1.0f/float(m_usSizeM1);
    m_fMultiplier = (m_fMaxElevation - m_fMinElevation)/65535.0f;
    m_bNeedsTessellation = true;

    // for tessellation (mapping of connectivity indices)
    m_ausLookup = new unsigned short[m_iVertexQuantity];
    m_iConnectLength = 0;

    // initialize vertex information array
    m_akTVertex = new TerrainVertex[m_iVertexQuantity];
    memset(m_akTVertex,0,m_iVertexQuantity*sizeof(TerrainVertex));

    // allocate quadtree
    m_usBlockQuantity = m_usSize*(m_usSize-2)/3;
    m_akBlock = new TerrainBlock[m_usBlockQuantity];

    // initialize quadtree
    unsigned char ucStride = (unsigned char)((m_usSize-1)/2);
    m_akBlock[0].Initialize(this,m_akBlock,0,0,0,ucStride,true);
    m_akBlock[0].UpdateBoundingBox(this,m_akBlock,0,ucStride);

    // model bounding sphere contains the top level block's bounding box
    Vector3f kCenter = 0.5f*(m_akBlock[0].GetMax() + m_akBlock[0].GetMin());
    Vector3f kDiag = 0.5f*(m_akBlock[0].GetMax() - m_akBlock[0].GetMin());
    m_kBound.Center() = kCenter;
    m_kBound.Radius() = kDiag.Length();

    // allocate queue
    m_usQueueQuantity = m_usBlockQuantity - m_usBlockQuantity/4;
    m_ausQueue = new unsigned short[m_usQueueQuantity];

    // root of quadtree is initially active
    m_ausQueue[0] = 0;
    m_usFront = 0;
    m_usRear = 1;
    m_usItemsInQueue = 1;
}
//----------------------------------------------------------------------------
void TerrainPage::SetPixelTolerance (Renderer* pkRenderer, float fTolerance)
{
    float fWidth = float(pkRenderer->GetWidth());
    CameraPtr spCamera = pkRenderer->GetCamera();
    float fRight = spCamera->GetFrustumRight();
    float fNear = spCamera->GetFrustumNear();

    m_fPixelTolerance = fTolerance;
    m_fWorldTolerance = 2.0f*fRight*m_fPixelTolerance/(fNear*fWidth);
    m_fWorldTolerance *= m_fWorldTolerance;
}
//----------------------------------------------------------------------------
float TerrainPage::GetHeight (const Vector2f& rkLocation) const
{
    float fXGrid = (rkLocation.X() - m_kOrigin.X())*m_fInvSpacing;
    if ( fXGrid < 0.0f || fXGrid >= float(m_usSizeM1) )
    {
        // location not in page
        return Mathf::MAX_REAL;
    }

    float fYGrid = (rkLocation.Y() - m_kOrigin.Y())*m_fInvSpacing;
    if ( fYGrid < 0.0f || fYGrid >= float(m_usSizeM1) )
    {
        // location not in page
        return Mathf::MAX_REAL;
    }

    float fCol = floorf(fXGrid);
    unsigned short usCol = (unsigned short) fCol;
    float fRow = floorf(fYGrid);
    unsigned short usRow = (unsigned short) fRow;

    unsigned short usIndex = usCol + m_usSize*usRow;
    float fDx = fXGrid - fCol;
    float fDy = fYGrid - fRow;
    float fH00, fH10, fH01, fH11, fHeight;

    if ( (usCol & 1) == (usRow & 1) )
    {
        float fDiff = fDx - fDy;
        fH00 = m_fMinElevation + m_fMultiplier*m_ausHeight[usIndex];
        fH11 = m_fMinElevation + m_fMultiplier *
            m_ausHeight[usIndex+1+m_usSize];
        if ( fDiff > 0.0f )
        {
            fH10 = m_fMinElevation + m_fMultiplier*m_ausHeight[usIndex+1];
            fHeight = (1.0f-fDiff-fDy)*fH00+fDiff*fH10+fDy*fH11;
        }
        else
        {
            fH01 = m_fMinElevation + m_fMultiplier *
                m_ausHeight[usIndex+m_usSize];
            fHeight = (1.0f+fDiff-fDx)*fH00-fDiff*fH01+fDx*fH11;
        }
    }
    else
    {
        float fSum = fDx + fDy;
        fH10 = m_fMinElevation + m_fMultiplier*m_ausHeight[usIndex+1];
        fH01 = m_fMinElevation + m_fMultiplier*m_ausHeight[usIndex+m_usSize];
        if ( fSum <= 1.0f )
        {
            fH00 = m_fMinElevation + m_fMultiplier*m_ausHeight[usIndex];
            fHeight = (1.0f-fSum)*fH00+fDx*fH10+fDy*fH01;
        }
        else
        {
            fH11 = m_fMinElevation + m_fMultiplier *
                m_ausHeight[usIndex+1+m_usSize];
            fHeight = (fSum-1.0f)*fH11+(1.0f-fDy)*fH10+(1.0f-fDx)*fH01;
        }
    }

    return fHeight;
}
//----------------------------------------------------------------------------
bool TerrainPage::IntersectFrustum (const Camera* pkCamera)
{
    // check if terrain page itself is inside frustum
    m_akBlock[0].TestIntersectFrustum(this,pkCamera);
    bool bIntersect = m_akBlock[0].GetVisible();
    m_akBlock[0].ClearBits();
    return bIntersect;
}
//----------------------------------------------------------------------------
void TerrainPage::AddToQueue (unsigned short usBlock)
{
    m_ausQueue[m_usRear] = usBlock;
    if ( ++m_usRear == m_usQueueQuantity )
        m_usRear = 0;
    m_usItemsInQueue++;
}
//----------------------------------------------------------------------------
unsigned short TerrainPage::RemoveFromQueue ()
{
    unsigned short usBlock = m_ausQueue[m_usFront];
    if ( ++m_usFront == m_usQueueQuantity )
        m_usFront = 0;
    m_usItemsInQueue--;
    return usBlock;
}
//----------------------------------------------------------------------------
unsigned short TerrainPage::ReadFromQueue (unsigned short usIndex)
{
    usIndex += m_usFront;
    if ( usIndex < m_usQueueQuantity )
        return m_ausQueue[usIndex];
    else
        return m_ausQueue[usIndex - m_usQueueQuantity];
}
//----------------------------------------------------------------------------
void TerrainPage::ResetBlocks ()
{
    unsigned short usQueue, usBlock;
    if ( m_usFront < m_usRear )
    {
        m_usNumUnprocessed = m_usRear - m_usFront;
        for (usQueue = m_usFront; usQueue < m_usRear; usQueue++)
        {
            usBlock = m_ausQueue[usQueue];
            if ( m_akBlock[usBlock].BitsSet() )
            {
                m_akBlock[usBlock].Disable(this);
                m_akBlock[usBlock].ClearBits();
            }
        }
    }
    else
    {
        m_usNumUnprocessed = m_usQueueQuantity - m_usFront + m_usRear;
        for (usQueue = m_usFront; usQueue < m_usQueueQuantity; usQueue++)
        {
            usBlock = m_ausQueue[usQueue];
            if ( m_akBlock[usBlock].BitsSet() )
            {
                m_akBlock[usBlock].Disable(this);
                m_akBlock[usBlock].ClearBits();
            }
        }
        for (usQueue = 0; usQueue < m_usRear; usQueue++)
        {
            usBlock = m_ausQueue[usQueue];
            if ( m_akBlock[usBlock].BitsSet() )
            {
                m_akBlock[usBlock].Disable(this);
                m_akBlock[usBlock].ClearBits();
            }
        }
    }
}
//----------------------------------------------------------------------------
void TerrainPage::SimplifyBlocks (const Camera* pkCamera,
    const Vector3f& rkModelEye, const Vector3f& rkModelDir,
    bool bCloseAssumption)
{
    while ( m_usNumUnprocessed > 0 )
    {
        // process the block in the front of queue
        unsigned short usBlock = RemoveFromQueue();
        TerrainBlock* pkBlock = &m_akBlock[usBlock];

        if ( !pkBlock->GetProcessed() )
        {
            m_usNumUnprocessed--;

            unsigned short usChild;
            TerrainBlock* pkChild;
            Vector2f kCLoc;
            int i;

            if ( pkBlock->IsFirstChild(usBlock) )
            {
                // first child has responsibility for replacing by parent
                if ( pkBlock->IsSibling(usBlock,ReadFromQueue(2)) )
                {
                    pkChild = pkBlock;
                    if ( bCloseAssumption )
                    {
                        for (i = 0; i < 4; i++, pkChild++)
                        {
                            kCLoc.X() = pkChild->GetX()*m_fSpacing +
                                m_kOrigin.X();
                            kCLoc.Y() = pkChild->GetY()*m_fSpacing +
                                m_kOrigin.Y();
                            pkChild->ComputeInterval(rkModelEye,rkModelDir,
                                m_fWorldTolerance,kCLoc,m_fSpacing);
                            if (pkChild->GetDeltaMax() > pkChild->GetDeltaL())
                                break;
                        }
                    }
                    else // distant assumption
                    {
                        for (i = 0; i < 4; i++, pkChild++)
                        {
                            pkChild->ComputeInterval(rkModelEye,
                                m_fWorldTolerance);
                            if (pkChild->GetDeltaMax() > pkChild->GetDeltaL())
                                break;
                        }
                    }

                    if ( i == 4 )
                    {
                        // remove child blocks (first child already removed)
                        for (i = 0; i < 3; i++)
                        {
                            usChild = RemoveFromQueue();
                            if ( !m_akBlock[usChild].GetProcessed() )
                                m_usNumUnprocessed--;
                            m_akBlock[usChild].ClearBits();
                        }

                        // add parent block
                        unsigned short usParent =
                            pkBlock->GetParentIndex(usBlock);
                        AddToQueue(usParent);
                        assert( !m_akBlock[usParent].GetProcessed() );
                        m_usNumUnprocessed++;
                        continue;
                    }
                }
            }

            if ( !pkBlock->GetVisibilityTested() )
                pkBlock->TestIntersectFrustum(this,pkCamera);

            if ( pkBlock->GetStride() > 1 )
            {
                // subdivide only if bounding box of block intersects frustum
                if ( pkBlock->GetVisible() )
                {
                    usChild = pkBlock->GetChildIndex(usBlock,1);
                    pkChild = &m_akBlock[usChild];
                    if ( bCloseAssumption ) 
                    {
                        for (i = 0; i < 4; i++, pkChild++)
                        {
                            kCLoc.X() = pkChild->GetX()*m_fSpacing +
                                m_kOrigin.X();
                            kCLoc.Y() = pkChild->GetY()*m_fSpacing +
                                m_kOrigin.Y();
                            pkChild->ComputeInterval(rkModelEye,rkModelDir,
                                m_fWorldTolerance,kCLoc,m_fSpacing);
                            if (pkChild->GetDeltaMax() > pkChild->GetDeltaL())
                                break;
                        }
                    }
                    else // distant assumption
                    {
                        for (i = 0; i < 4; i++, pkChild++)
                        {
                            pkChild->ComputeInterval(rkModelEye,
                                m_fWorldTolerance);
                            if (pkChild->GetDeltaMax() > pkChild->GetDeltaL())
                                break;
                        }
                    }

                    // subdivide only if children all agree it should happen
                    if ( i < 4 )
                    {
                        // add child blocks (parent already removed)
                        for (i = 0; i < 4; i++, usChild++)
                        {
                            // add child block
                            AddToQueue(usChild);
                            assert( !m_akBlock[usChild].GetProcessed() );
                            m_usNumUnprocessed++;
                        }
                        continue;
                    }
                }
            }

            // tag block as processed
            pkBlock->SetProcessed(true);
        }

        // put processed block at rear of queue
        AddToQueue(usBlock);
    }
}
//----------------------------------------------------------------------------
void TerrainPage::SimplifyVertices (const Vector3f& rkModelEye,
    const Vector3f& rkModelDir, bool bCloseAssumption)
{
    unsigned int usQueue, usBlock;

    if ( m_usFront < m_usRear )
    {
        for (usQueue = m_usFront; usQueue < m_usRear; usQueue++)
        {
            usBlock = m_ausQueue[usQueue];
            if ( m_akBlock[usBlock].GetVisible() )
            {
                m_akBlock[usBlock].SimplifyVertices(this,rkModelEye,
                    rkModelDir,m_fWorldTolerance,bCloseAssumption);
            }
        }
    }
    else
    {
        for (usQueue = m_usFront; usQueue < m_usQueueQuantity; usQueue++)
        {
            usBlock = m_ausQueue[usQueue];
            if ( m_akBlock[usBlock].GetVisible() )
            {
                m_akBlock[usBlock].SimplifyVertices(this,rkModelEye,
                    rkModelDir,m_fWorldTolerance,bCloseAssumption);
            }
        }
        for (usQueue = 0; usQueue < m_usRear; usQueue++)
        {
            usBlock = m_ausQueue[usQueue];
            if ( m_akBlock[usBlock].GetVisible() )
            {
                m_akBlock[usBlock].SimplifyVertices(this,rkModelEye,
                    rkModelDir,m_fWorldTolerance,bCloseAssumption);
            }
        }
    }
}
//----------------------------------------------------------------------------
void TerrainPage::Simplify (Renderer* pkRenderer, const Vector3f& rkModelEye,
    const Vector3f& rkModelDir, bool bCloseAssumption)
{
    if ( m_fWorldTolerance == -1.0f )
        SetPixelTolerance(pkRenderer,m_fPixelTolerance);

    SimplifyBlocks(pkRenderer->GetCamera(),rkModelEye,rkModelDir,
        bCloseAssumption);
    SimplifyVertices(rkModelEye,rkModelDir,bCloseAssumption);

    m_bNeedsTessellation = true;
}
//----------------------------------------------------------------------------
void TerrainPage::Render (TerrainBlock& rkBlock)
{
    unsigned short usOrigin = rkBlock.GetX() + m_usSize*rkBlock.GetY();
    unsigned short usTwoStride = 2*rkBlock.GetStride();
    unsigned short usTwoStrideTimesSize = usTwoStride*m_usSize;
    unsigned short usIndex[5] =
    {
        usOrigin,
        usOrigin + usTwoStride,
        usOrigin + rkBlock.GetStride()*(m_usSize + 1),
        usOrigin + usTwoStrideTimesSize,
        usOrigin + usTwoStrideTimesSize + usTwoStride
    };

    if ( rkBlock.GetEven() )
    {
        RenderTriangle(usIndex[0],usIndex[3],usIndex[1]);
        RenderTriangle(usIndex[4],usIndex[1],usIndex[3]);
    }
    else
    {
        RenderTriangle(usIndex[1],usIndex[0],usIndex[4]);
        RenderTriangle(usIndex[3],usIndex[4],usIndex[0]);
    }

}
//----------------------------------------------------------------------------
void TerrainPage::RenderTriangle (unsigned short usT, unsigned short usL,
    unsigned short usR)
{
    // determine if triangle is leaf or interior
    bool bInterior;
    if ( usR > usT )
    {
        if ( usL > usT )
            bInterior = (usR - usT > 1);
        else
            bInterior = (usT - usL > 1);
    }
    else
    {
        if ( usL > usT )
            bInterior = (usL - usT > 1);
        else
            bInterior = (usT - usR > 1);
    }

    if ( bInterior )
    {
        // Triangle is part of internal block and can be subdivided.  M is
        // the midpoint of edge <L,R>.
        unsigned short usM = ((usL + usR) >> 1);
        if ( m_akTVertex[usM].GetEnabled() )
        {
            RenderTriangle(usM,usT,usL);
            RenderTriangle(usM,usR,usT);
            return;
        }
    }

    // pack the vertex data
    assert( m_iVertexQuantity < m_usSize*m_usSize );
    unsigned char ucX, ucY;
    if ( m_ausLookup[usT] == (unsigned short)(~0) )
    {
        ucX = usT % m_usSize;
        ucY = usT / m_usSize;

        m_akVertex[m_iVertexQuantity].X() = GetX(ucX);
        m_akVertex[m_iVertexQuantity].Y() = GetY(ucY);
        m_akVertex[m_iVertexQuantity].Z() = GetHeight(usT);
        m_akTexture[m_iVertexQuantity].X() = GetTexture(ucX);
        m_akTexture[m_iVertexQuantity].Y() = GetTexture(ucY);
        m_ausLookup[usT] = m_iVertexQuantity++;
    }

    if ( m_ausLookup[usR] == (unsigned short)(~0) )
    {
        ucX = usR % m_usSize;
        ucY = usR / m_usSize;
        
        m_akVertex[m_iVertexQuantity].X() = GetX(ucX);
        m_akVertex[m_iVertexQuantity].Y() = GetY(ucY);
        m_akVertex[m_iVertexQuantity].Z() = GetHeight(usR);
        m_akTexture[m_iVertexQuantity].X() = GetTexture(ucX);
        m_akTexture[m_iVertexQuantity].Y() = GetTexture(ucY);
        m_ausLookup[usR] = m_iVertexQuantity++;
    }

    if ( m_ausLookup[usL] == (unsigned short)(~0) )
    {
        ucX = usL % m_usSize;
        ucY = usL / m_usSize;

        m_akVertex[m_iVertexQuantity].X() = GetX(ucX);
        m_akVertex[m_iVertexQuantity].Y() = GetY(ucY);
        m_akVertex[m_iVertexQuantity].Z() = GetHeight(usL);
        m_akTexture[m_iVertexQuantity].X() = GetTexture(ucX);
        m_akTexture[m_iVertexQuantity].Y() = GetTexture(ucY);
        m_ausLookup[usL] = m_iVertexQuantity++;
    }

    // add triangle to connectivity array
    assert( m_iTriangleQuantity < 2*(m_usSize-1)*(m_usSize-1) );
    assert( m_iConnectLength < 6*(m_usSize-1)*(m_usSize-1) );
    m_aiConnect[m_iConnectLength++] = m_ausLookup[usT];
    m_aiConnect[m_iConnectLength++] = m_ausLookup[usR];
    m_aiConnect[m_iConnectLength++] = m_ausLookup[usL];
    m_iTriangleQuantity++;
}
//----------------------------------------------------------------------------
void TerrainPage::RenderBlocks ()
{
    // reset dynamic quantities
    memset(m_ausLookup,0xFF,m_usSize*m_usSize*sizeof(unsigned short));
    m_iVertexQuantity = 0;
    m_iTriangleQuantity = 0;
    m_iConnectLength = 0;

    unsigned short usQueue;
    if ( m_usFront < m_usRear )
    {
        for (usQueue = m_usFront; usQueue < m_usRear; usQueue++)
        {
            TerrainBlock& rkBlock = m_akBlock[m_ausQueue[usQueue]];
            if ( rkBlock.GetVisible() )
                Render(rkBlock);
        }
    }
    else
    {
        for (usQueue = m_usFront; usQueue < m_usQueueQuantity; usQueue++)
        {
            TerrainBlock& rkBlock = m_akBlock[m_ausQueue[usQueue]];
            if ( rkBlock.GetVisible() )
                Render(rkBlock);
        }
        for (usQueue = 0; usQueue < m_usRear; usQueue++)
        {
            TerrainBlock& rkBlock = m_akBlock[m_ausQueue[usQueue]];
            if ( rkBlock.GetVisible() )
                Render(rkBlock);
        }
    }
}
//----------------------------------------------------------------------------
void TerrainPage::StitchLR (TerrainPage* pkRight)
{
    // 'this' is left page, 'pkRight' is right page
    assert( pkRight->m_usSize == m_usSize );

    unsigned short usR = m_usSize;
    for (unsigned short usY = 1; usY < m_usSizeM1; usY++, usR += m_usSize)
    {
        unsigned short usL = m_usSizeM1 + usR;
        TerrainVertex* pkLVertex = &m_akTVertex[usL];
        TerrainVertex* pkRVertex = &pkRight->m_akTVertex[usR];
        pkRVertex->SetDependent(0,pkLVertex);
        pkLVertex->SetDependent(1,pkRVertex);
    }
}
//----------------------------------------------------------------------------
void TerrainPage::StitchTB (TerrainPage* pkBottom)
{
    // 'this' is top page, 'pkBottom' is bottom page
    assert( pkBottom->m_usSize == m_usSize );

    unsigned short usOffset = m_usSize*m_usSizeM1;
    for (unsigned short usB = 1; usB < m_usSizeM1; usB++)
    {
        unsigned short usT = usB + usOffset;
        TerrainVertex* pkTVertex = &m_akTVertex[usT];
        TerrainVertex* pkBVertex = &pkBottom->m_akTVertex[usB];
        pkBVertex->SetDependent(1,pkTVertex);
        pkTVertex->SetDependent(0,pkBVertex);
    }
}
//----------------------------------------------------------------------------
void TerrainPage::UnstitchLR (TerrainPage* pkRight)
{
    // 'this' is left page, 'pkRight' is right page
    assert( pkRight->m_usSize == m_usSize );

    unsigned short usR = m_usSize;  // y = 1
    for (unsigned short usY = 1; usY < m_usSizeM1; usY++, usR += m_usSize)
    {
        unsigned short usL = m_usSizeM1 + usR;
        TerrainVertex* pkLVertex = &m_akTVertex[usL];
        TerrainVertex* pkRVertex = &pkRight->m_akTVertex[usR];
        pkRVertex->SetDependent(0,NULL);
        pkLVertex->SetDependent(1,NULL);
    }
}
//----------------------------------------------------------------------------
void TerrainPage::UnstitchTB (TerrainPage* pkBottom)
{
    // 'this' is top page, 'pBottom' is bottom page
    assert( pkBottom->m_usSize == m_usSize );

    unsigned short usOffset = m_usSize*m_usSizeM1;
    for (unsigned short usB = 1; usB < m_usSizeM1; usB++)
    {
        unsigned short usT = usB + usOffset;
        TerrainVertex* pkTVertex = &m_akTVertex[usT];
        TerrainVertex* pkBVertex = &pkBottom->m_akTVertex[usB];
        pkBVertex->SetDependent(1,NULL);
        pkTVertex->SetDependent(0,NULL);
    }
}
//----------------------------------------------------------------------------
void TerrainPage::Draw (Renderer& rkRenderer)
{
    if ( m_bNeedsTessellation )
    {
        m_bNeedsTessellation = false;
        RenderBlocks();
    }

    // It is possible (but not likely) that blocks are not culled, but the
    // camera is positioned so that no triangles are visible.  For example,
    // this can occur if you are below the terrain mesh and looking down.
    if ( m_iTriangleQuantity > 0 )
        TriMesh::Draw(rkRenderer);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* TerrainPage::Factory (Stream& rkStream)
{
    TerrainPage* pkObject = new TerrainPage;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void TerrainPage::Load (Stream& rkStream, Stream::Link* pkLink)
{
    TriMesh::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_usSize);
    unsigned short usQuantity = m_usSize*m_usSize;
    m_ausHeight = new unsigned short[usQuantity];
    StreamRead(rkStream,m_ausHeight,usQuantity);
    StreamRead(rkStream,m_kOrigin);
    StreamRead(rkStream,m_fMinElevation);
    StreamRead(rkStream,m_fMaxElevation);
    StreamRead(rkStream,m_fSpacing);

    InitializeDerivedData();
}
//----------------------------------------------------------------------------
void TerrainPage::Link (Stream& rkStream, Stream::Link* pkLink)
{
    TriMesh::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool TerrainPage::Register (Stream& rkStream)
{
    return TriMesh::Register(rkStream);
}
//----------------------------------------------------------------------------
void TerrainPage::Save (Stream& rkStream)
{
    // The vertex and triangle quantities are dynamically varied during the
    // simplification.  Write the maximum quantities to disk, then reset to
    // the current dynamic values.
    int iSaveVQ = m_iVertexQuantity;
    int iSaveTQ = m_iTriangleQuantity;
    int iSize = (int)m_usSize, iSizeM1 = iSize-1;
    m_iVertexQuantity = iSize*iSize;
    m_iTriangleQuantity = 2*iSizeM1*iSizeM1;

    TriMesh::Save(rkStream);

    m_iVertexQuantity = iSaveVQ;
    m_iTriangleQuantity = iSaveTQ;

    // native data
    StreamWrite(rkStream,m_usSize);
    StreamWrite(rkStream,m_ausHeight,m_usSize*m_usSize);
    StreamWrite(rkStream,m_kOrigin);
    StreamWrite(rkStream,m_fMinElevation);
    StreamWrite(rkStream,m_fMaxElevation);
    StreamWrite(rkStream,m_fSpacing);
}
//----------------------------------------------------------------------------
StringTree* TerrainPage::SaveStrings ()
{
    // TO DO.  Finish implementation.
    StringTree* pkTree = new StringTree(1,0,1,0);
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetChild(0,TriMesh::SaveStrings());
    return pkTree;
}
//----------------------------------------------------------------------------
int TerrainPage::GetMemoryUsed () const
{
    // The vertex and triangle quantities are dynamically varied during the
    // simplification.  Count the maximum quantities, then reset to the
    // current dynamic values.  The typecast to integer references is to
    // circumvent the 'const' of this function.  In effect, however, the
    // function is 'const' since the quantities are restored.
    int& riVQ = (int&)m_iVertexQuantity;
    int& riTQ = (int&)m_iTriangleQuantity;
    int iSaveVQ = riVQ;
    int iSaveTQ = riTQ;
    int iSize = (int)m_usSize, iSizeM1 = iSize-1;
    riVQ = iSize*iSize;
    riTQ = 2*iSizeM1*iSizeM1;

    int iBaseSize = sizeof(TerrainPage) - sizeof(TriMesh);

    int iDynaSize =
        m_iVertexQuantity*sizeof(m_ausHeight[0]) + 
        m_iVertexQuantity*sizeof(m_ausLookup[0]) +
        m_iVertexQuantity*sizeof(m_akTVertex[0]) +
        m_usBlockQuantity*sizeof(m_akBlock[0]) +
        m_usQueueQuantity*sizeof(m_ausQueue[0]);

    int iTotalSize = iBaseSize + iDynaSize + TriMesh::GetMemoryUsed();

    riVQ = iSaveVQ;
    riTQ = iSaveTQ;

    return iTotalSize;
}
//----------------------------------------------------------------------------
int TerrainPage::GetDiskUsed () const
{
    // The vertex and triangle quantities are dynamically varied during the
    // simplification.  Count the maximum quantities, then reset to the
    // current dynamic values.  The typecast to integer references is to
    // circumvent the 'const' of this function.  In effect, however, the
    // function is 'const' since the quantities are restored.
    int& riVQ = (int&)m_iVertexQuantity;
    int& riTQ = (int&)m_iTriangleQuantity;
    int iSaveVQ = riVQ;
    int iSaveTQ = riTQ;
    int iSize = (int)m_usSize, iSizeM1 = iSize-1;
    riVQ = iSize*iSize;
    riTQ = 2*iSizeM1*iSizeM1;

    int iDiskUsed = TriMesh::GetDiskUsed() +
        sizeof(m_usSize) +
        riVQ*sizeof(m_ausHeight[0]) +
        sizeof(m_kOrigin) +
        sizeof(m_fMinElevation) +
        sizeof(m_fMaxElevation) +
        sizeof(m_fSpacing);

    riVQ = iSaveVQ;
    riTQ = iSaveTQ;

    return iDiskUsed;
}
//----------------------------------------------------------------------------
