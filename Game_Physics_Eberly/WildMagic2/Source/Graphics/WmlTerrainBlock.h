// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLTERRAINBLOCK_H
#define WMLTERRAINBLOCK_H

#include "WmlVector2.h"
#include "WmlBox3.h"

namespace Wml
{

class Camera;
class Frustum;
class TerrainPage;
class TerrainVertex;

class WML_ITEM TerrainBlock
{
public:
    unsigned char GetX () const;
    unsigned char GetY () const;
    unsigned char GetStride () const;
    float GetDelta (int i) const;
    float GetDeltaMax () const;
    float GetDeltaL () const;
    float GetDeltaH () const;
    const Vector3f& GetMin () const;
    const Vector3f& GetMax () const;

    void SetEven (bool bSet);
    bool GetEven () const;
    void SetProcessed (bool bSet);
    bool GetProcessed () const;
    void SetVisible (bool bSet);
    bool GetVisible () const;
    void SetVisibilityTested (bool bSet);
    bool GetVisibilityTested () const;

    bool BitsSet () const;
    void ClearBits ();

    // creation of the quadtree
    void Initialize (TerrainPage* pkPage, TerrainBlock* pkBlock,
        unsigned short usBlock, unsigned char ucX, unsigned char ucY,
        unsigned char ucStride, bool bEven);

    // allows for changing the height data during runtime
    void UpdateBoundingBox (TerrainPage* pkPage, TerrainBlock* pkBlock,
        unsigned short usBlock, unsigned char ucStride);

    // test for intersection of page's bounding box and view frustum
    void TestIntersectFrustum (const TerrainPage* pkPage,
        const Camera* pkCamera);

    // distant terrain assumption
    void ComputeInterval (const Vector3f& rkModelEye, float fTolerance);

    // close terrain assumption
    void ComputeInterval (const Vector3f& rkModelDir,
        const Vector3f& rkModelEye, float fTolerance, Vector2f& rkLoc,
        float fSpacing);

    void SimplifyVertices (TerrainPage* pkPage, const Vector3f& rkModelEye,
        const Vector3f& rkModelDir, float fTolerance, bool bCloseAssumption);

    void Disable (TerrainPage* pkPage);

    // quadtree indexing
    static unsigned short GetParentIndex (unsigned short usChild);
    static unsigned short GetChildIndex (unsigned short usParent,
        unsigned short usIndex);
    static bool IsFirstChild (unsigned short usIndex);
    static bool IsSibling (unsigned short usIndex, unsigned short usTest);

protected:
    // bit flags for m_ucFlags
    enum
    {
        EVEN_PARITY       = 0x01,
        PROCESSED         = 0x02,
        VISIBLE           = 0x04,
        VISIBILITY_TESTED = 0x08,
        BITS_MASK         = 0x0E  // all but even parity bit
    };

    void GetVertex9 (unsigned short usSize, TerrainVertex* pkVOrigin,
        TerrainVertex* apkTVertex[9]);

    unsigned char m_ucX, m_ucY, m_ucStride, m_ucFlags;
    float m_fDelta[5], m_fDeltaMax;
    float m_fDeltaL, m_fDeltaH;
    Vector3f m_kMin, m_kMax;
};

#include "WmlTerrainBlock.inl"

}

#endif
