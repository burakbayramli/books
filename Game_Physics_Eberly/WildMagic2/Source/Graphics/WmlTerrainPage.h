// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLTERRAINPAGE_H
#define WMLTERRAINPAGE_H

#include "WmlTriMesh.h"

namespace Wml
{

class Camera;
class TerrainVertex;

class WML_ITEM TerrainPage : public TriMesh
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // size = 2^p + 1, p <= 7 (size = 3, 5, 9, 17, 33, 65, 129)
    TerrainPage (unsigned short usSize, unsigned short* ausHeight,
        const Vector2f& rkOrigin, float fMinElevation, float fMaxElevation,
        float fSpacing);

    virtual ~TerrainPage ();

    // height field access
    unsigned short GetSize () const;
    const unsigned short* GetHeights () const;
    const Vector2f& GetOrigin () const;
    float GetMinElevation () const;
    float GetMaxElevation () const;
    float GetSpacing () const;

    // pixel tolerance on projected vertex height
    void SetPixelTolerance (Renderer* pkRenderer, float fTolerance);
    float GetPixelTolerance () const;

    // Height field measurements.  If the location is not in the page, the
    // return value is INFINITY.
    float GetHeight (const Vector2f& rkLocation) const;

    // simplification
    void ResetBlocks ();
    void Simplify (Renderer* pkRenderer, const Vector3f& rkModelEye,
        const Vector3f& rkModelDir, bool bCloseAssumption);

protected:
    friend class TerrainBlock;

    // streaming support
    TerrainPage ();
    void InitializeDerivedData ();

    // queue handlers
    void AddToQueue (unsigned short usBlock);
    unsigned short RemoveFromQueue ();
    unsigned short ReadFromQueue (unsigned short usIndex);

    // page simplification
    bool IntersectFrustum (const Camera* pkCamera);

    // block simplification
    void SimplifyBlocks (const Camera* pCamera, const Vector3f& rkModelEye,
        const Vector3f& rkModelDir, bool bCloseAssumption);

    // vertex simplification
    void SimplifyVertices (const Vector3f& rkModelEye,
        const Vector3f& rkModelDir, bool bCloseTerrainAssumption);

    // tessellation
    float GetX (unsigned char ucX) const;
    float GetY (unsigned char ucY) const;
    float GetHeight (unsigned short usIndex) const;
    float GetTexture (unsigned char ucIndex) const;
    void Render (TerrainBlock& rkBlock);
    void RenderTriangle (unsigned short usT, unsigned short usL,
        unsigned short usR);
    void RenderBlocks ();

    virtual void Draw (Renderer& rkRenderer);

    // for multiple pages in the terrain system
    void StitchLR (TerrainPage* pkRight);
    void StitchTB (TerrainPage* pkBottom);
    void UnstitchLR (TerrainPage* pkRight);
    void UnstitchTB (TerrainPage* pkBottom);

    // height field
    unsigned short m_usSize, m_usSizeM1;
    unsigned short* m_ausHeight;
    Vector2f m_kOrigin;
    float m_fMinElevation, m_fMaxElevation, m_fSpacing;
    float m_fInvSpacing, m_fTextureSpacing, m_fMultiplier;

    // simplification
    float m_fPixelTolerance, m_fWorldTolerance;
    bool m_bNeedsTessellation;
    unsigned short* m_ausLookup;
    int m_iConnectLength;

    // (2^p+1) by (2^p+1) array of vertices, row-major order
    TerrainVertex* m_akTVertex;

    // quadtree of blocks
    unsigned short m_usBlockQuantity;
    TerrainBlock* m_akBlock;

    // circular queue of indices for active blocks
    unsigned short* m_ausQueue;
    unsigned short m_usQueueQuantity;
    unsigned short m_usFront, m_usRear;
    unsigned short m_usNumUnprocessed;
    unsigned short m_usItemsInQueue;
};

WmlSmartPointer(TerrainPage);
WmlRegisterStream(TerrainPage);
#include "WmlTerrainPage.inl"

}

#endif
