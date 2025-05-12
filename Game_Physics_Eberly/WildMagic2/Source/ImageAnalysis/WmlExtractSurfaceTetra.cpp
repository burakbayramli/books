// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlExtractSurfaceTetra.h"
#include "WmlVETMesh.h"
using namespace Wml;

#include <algorithm>
using namespace std;

typedef map<Vector3f,int> VMap;
typedef VMap::iterator VMapIterator;
typedef map<TriangleKey,int> TMap;
typedef TMap::iterator TMapIterator;

//----------------------------------------------------------------------------
ExtractSurfaceTetra::ExtractSurfaceTetra (int iXBound, int iYBound,
    int iZBound, int* aiData)
{
    assert( iXBound > 0 && iYBound > 0 && iZBound > 0 && aiData );
    m_iXBound = iXBound;
    m_iYBound = iYBound;
    m_iZBound = iZBound;
    m_iXYBound = iXBound*iYBound;
    m_iXYZBound = m_iXYBound*iZBound;
    m_aiData = aiData;
}
//----------------------------------------------------------------------------
float ExtractSurfaceTetra::GetFunction (const Vector3f& rkP) const
{
    int iX = (int) rkP.X();
    if ( iX < 0 || iX >= m_iXBound-1 )
        return 0.0f;

    int iY = (int) rkP.Y();
    if ( iY < 0 || iY >= m_iYBound-1 )
        return 0.0f;

    int iZ = (int) rkP.Z();
    if ( iZ < 0 || iZ >= m_iZBound-1 )
        return 0.0f;

    float fDX = rkP.X() - iX, fDY = rkP.Y() - iY, fDZ = rkP.Z() - iZ;

    int i000 = iX + m_iXBound*(iY + m_iYBound*iZ);
    int i100 = i000 + 1;
    int i010 = i000 + m_iXBound;
    int i110 = i100 + m_iXBound;
    int i001 = i000 + m_iXYBound;
    int i101 = i100 + m_iXYBound;
    int i011 = i010 + m_iXYBound;
    int i111 = i110 + m_iXYBound;
    float fF000 = (float) m_aiData[i000];
    float fF100 = (float) m_aiData[i100];
    float fF010 = (float) m_aiData[i010];
    float fF110 = (float) m_aiData[i110];
    float fF001 = (float) m_aiData[i001];
    float fF101 = (float) m_aiData[i101];
    float fF011 = (float) m_aiData[i011];
    float fF111 = (float) m_aiData[i111];
    float fC0, fC1, fC2, fInterp;

    if ( (iX & 1) ^ (iY & 1) ^ (iZ & 1) )
    {
        if ( fDX - fDY - fDZ >= 0.0f )
        {
            // 1205
            fInterp =
                (1.0f-(1.0f-fDX)-fDY-fDZ)*fF100 +
                (1.0f-fDX)*fF000 +
                fDY*fF110 +
                fDZ*fF101;
        }
        else if ( fDX - fDY + fDZ <= 0.0f )
        {
            // 3027
            fInterp =
                (1.0f-fDX-(1.0f-fDY)-fDZ)*fF010 +
                fDX*fF110 +
                (1.0f-fDY)*fF000 +
                fDZ*fF011;
        }
        else if ( fDX + fDY - fDZ <= 0.0f )
        {
            // 4750
            fInterp =
                (1.0f-fDX-fDY-(1-fDZ))*fF001 +
                fDX*fF101 +
                fDY*fF011 +
                (1.0f-fDZ)*fF000;
        }
        else if ( fDX + fDY + fDZ >= 2.0f )
        {
            // 6572
            fInterp =
                (1.0f-(1.0f-fDX)-(1.0f-fDY)-(1.0f-fDZ))*fF111 +
                (1.0f-fDX)*fF011 +
                (1.0f-fDY)*fF101 +
                (1.0f-fDZ)*fF110;
        }
        else
        {
            // 0752
            fC0 = 0.5f*(-fDX+fDY+fDZ);
            fC1 = 0.5f*(fDX-fDY+fDZ);
            fC2 = 0.5f*(fDX+fDY-fDZ);
            fInterp =
                (1.0f-fC0-fC1-fC2)*fF000 +
                fC0*fF011 +
                fC1*fF101 +
                fC2*fF110;
        }
    }
    else
    {
        if ( fDX + fDY + fDZ <= 1.0f )
        {
            // 0134
            fInterp =
                (1.0f-fDX-fDY-fDZ)*fF000 +
                fDX*fF100 +
                fDY*fF010 +
                fDZ*fF001;
        }
        else if ( fDX + fDY - fDZ >= 1.0f )
        {
            // 2316
            fInterp =
                (1.0f-(1.0f-fDX)-(1.0f-fDY)-fDZ)*fF110 +
                (1.0f-fDX)*fF010 +
                (1.0f-fDY)*fF100 +
                fDZ*fF111;
        }
        else if ( fDX - fDY + fDZ >= 1.0f )
        {
            // 5461
            fInterp =
                (1.0f-(1.0f-fDX)-fDY-(1.0f-fDZ))*fF101 +
                (1.0f-fDX)*fF001 +
                fDY*fF111 +
                (1.0f-fDZ)*fF100;
        }
        else if ( -fDX + fDY + fDZ >= 1.0f )
        {
            // 7643
            fInterp =
                (1.0f-fDX-(1.0f-fDY)-(1.0f-fDZ))*fF011 +
                fDX*fF111 +
                (1.0f-fDY)*fF001 +
                (1.0f-fDZ)*fF010;
        }
        else
        {
            // 6314
            fC0 = 0.5f*((1.0f-fDX)-(1.0f-fDY)+(1.0f-fDZ));
            fC1 = 0.5f*(-(1.0f-fDX)+(1.0f-fDY)+(1.0f-fDZ));
            fC2 = 0.5f*((1.0f-fDX)+(1.0f-fDY)-(1.0f-fDZ));
            fInterp =
                (1.0f-fC0-fC1-fC2)*fF111 +
                fC0*fF010 +
                fC1*fF100 +
                fC2*fF001;
        }
    }

    return fInterp;
}
//----------------------------------------------------------------------------
Vector3f ExtractSurfaceTetra::GetGradient (const Vector3f& rkP) const
{
    int iX = (int) rkP.X();
    if ( iX < 0 || iX >= m_iXBound-1 )
        return Vector3f::ZERO;

    int iY = (int) rkP.Y();
    if ( iY < 0 || iY >= m_iYBound-1 )
        return Vector3f::ZERO;

    int iZ = (int) rkP.Z();
    if ( iZ < 0 || iZ >= m_iZBound-1 )
        return Vector3f::ZERO;

    float fDX = rkP.X() - iX, fDY = rkP.Y() - iY, fDZ = rkP.Z() - iZ;

    int i000 = iX + m_iXBound*(iY + m_iYBound*iZ);
    int i100 = i000 + 1;
    int i010 = i000 + m_iXBound;
    int i110 = i100 + m_iXBound;
    int i001 = i000 + m_iXYBound;
    int i101 = i100 + m_iXYBound;
    int i011 = i010 + m_iXYBound;
    int i111 = i110 + m_iXYBound;
    float fF000 = (float) m_aiData[i000];
    float fF100 = (float) m_aiData[i100];
    float fF010 = (float) m_aiData[i010];
    float fF110 = (float) m_aiData[i110];
    float fF001 = (float) m_aiData[i001];
    float fF101 = (float) m_aiData[i101];
    float fF011 = (float) m_aiData[i011];
    float fF111 = (float) m_aiData[i111];
    Vector3f kInterp;

    if ( (iX & 1) ^ (iY & 1) ^ (iZ & 1) )
    {
        if ( fDX - fDY - fDZ >= 0.0f )
        {
            // 1205
            kInterp.X() = + fF100 - fF000;
            kInterp.Y() = - fF100 + fF110;
            kInterp.Z() = - fF100 + fF101; 
        }
        else if ( fDX - fDY + fDZ <= 0.0f )
        {
            // 3027
            kInterp.X() = - fF010 + fF110;
            kInterp.Y() = + fF010 - fF000;
            kInterp.Z() = - fF010 + fF011;
        }
        else if ( fDX + fDY - fDZ <= 0.0f )
        {
            // 4750
            kInterp.X() = - fF001 + fF101;
            kInterp.Y() = - fF001 + fF011;
            kInterp.Z() = + fF001 - fF000;
        }
        else if ( fDX + fDY + fDZ >= 2.0f )
        {
            // 6572
            kInterp.X() = + fF111 - fF011;
            kInterp.Y() = + fF111 - fF101;
            kInterp.Z() = + fF111 - fF110;
        }
        else
        {
            // 0752
            kInterp.X() = 0.5f*(-fF000-fF011+fF101+fF110);
            kInterp.Y() = 0.5f*(-fF000+fF011-fF101+fF110);
            kInterp.Z() = 0.5f*(-fF000+fF011+fF101-fF110);
        }
    }
    else
    {
        if ( fDX + fDY + fDZ <= 1.0f )
        {
            // 0134
            kInterp.X() = - fF000 + fF100;
            kInterp.Y() = - fF000 + fF010;
            kInterp.Z() = - fF000 + fF001;
        }
        else if ( fDX + fDY - fDZ >= 1.0f )
        {
            // 2316
            kInterp.X() = + fF110 - fF010;
            kInterp.Y() = + fF110 - fF100;
            kInterp.Z() = - fF110 + fF111;
        }
        else if ( fDX - fDY + fDZ >= 1.0f )
        {
            // 5461
            kInterp.X() = + fF101 - fF001;
            kInterp.Y() = - fF101 + fF111;
            kInterp.Z() = + fF101 - fF100;
        }
        else if ( -fDX + fDY + fDZ >= 1.0f )
        {
            // 7643
            kInterp.X() = - fF011 + fF111;
            kInterp.Y() = + fF011 - fF001;
            kInterp.Z() = + fF011 - fF010;
        }
        else
        {
            // 6314
            kInterp.X() = 0.5f*(fF111-fF010+fF100-fF001);
            kInterp.Y() = 0.5f*(fF111+fF010-fF100-fF001);
            kInterp.Z() = 0.5f*(fF111-fF010-fF100+fF001);
        }
    }

    return kInterp;
}
//----------------------------------------------------------------------------
void ExtractSurfaceTetra::ExtractContour (int iLevel, vector<Vector3f>& rkVA,
    vector<TriangleKey>& rkTA)
{
    // NOTE:  Isolated edges are computed, but not reported to the caller.
    // You can modify the code to return these if so desired.
    VtxMap kVMap;
    ESet kESet;
    TSet kTSet;
    m_iNextIndex = 0;

    // adjust image so level set is F(x,y,z) = 0
    int i;
    for (i = 0; i < m_iXYZBound; i++)
        m_aiData[i] = m_aiData[i] - iLevel;

    int iXBoundM1 = m_iXBound - 1;
    int iYBoundM1 = m_iYBound - 1;
    int iZBoundM1 = m_iZBound - 1;
    for (int iZ = 0, iZP = 1; iZ < iZBoundM1; iZ++, iZP++)
    {
        int iZParity = (iZ & 1);

        for (int iY = 0, iYP = 1; iY < iYBoundM1; iY++, iYP++)
        {
            int iYParity = (iY & 1);

            for (int iX = 0, iXP = 1; iX < iXBoundM1; iX++, iXP++)
            {
                int iXParity = (iX & 1);

                int i000 = iX + m_iXBound*(iY + m_iYBound*iZ);
                int i100 = i000 + 1;
                int i010 = i000 + m_iXBound;
                int i110 = i100 + m_iXBound;
                int i001 = i000 + m_iXYBound;
                int i101 = i100 + m_iXYBound;
                int i011 = i010 + m_iXYBound;
                int i111 = i110 + m_iXYBound;
                int iF000 = m_aiData[i000];
                int iF100 = m_aiData[i100];
                int iF010 = m_aiData[i010];
                int iF110 = m_aiData[i110];
                int iF001 = m_aiData[i001];
                int iF101 = m_aiData[i101];
                int iF011 = m_aiData[i011];
                int iF111 = m_aiData[i111];

                if ( iXParity ^ iYParity ^ iZParity )
                {
                    // 1205
                    ProcessTetrahedron(kVMap,kESet,kTSet,
                        iXP,iY,iZ,iF100,iXP,iYP,iZ,iF110,iX,iY,iZ,iF000,iXP,
                        iY,iZP,iF101);

                    // 3027
                    ProcessTetrahedron(kVMap,kESet,kTSet,
                        iX,iYP,iZ,iF010,iX,iY,iZ,iF000,iXP,iYP,iZ,iF110,iX,
                        iYP,iZP,iF011);

                    // 4750
                    ProcessTetrahedron(kVMap,kESet,kTSet,
                        iX,iY,iZP,iF001,iX,iYP,iZP,iF011,iXP,iY,iZP,iF101,iX,
                        iY,iZ,iF000);

                    // 6572
                    ProcessTetrahedron(kVMap,kESet,kTSet,
                        iXP,iYP,iZP,iF111,iXP,iY,iZP,iF101,iX,iYP,iZP,iF011,
                        iXP,iYP,iZ,iF110);

                    // 0752
                    ProcessTetrahedron(kVMap,kESet,kTSet,
                        iX,iY,iZ,iF000,iX,iYP,iZP,iF011,iXP,iY,iZP,iF101,iXP,
                        iYP,iZ,iF110);
                }
                else
                {
                    // 0134
                    ProcessTetrahedron(kVMap,kESet,kTSet,
                        iX,iY,iZ,iF000,iXP,iY,iZ,iF100,iX,iYP,iZ,iF010,iX,iY,
                        iZP,iF001);

                    // 2316
                    ProcessTetrahedron(kVMap,kESet,kTSet,
                        iXP,iYP,iZ,iF110,iX,iYP,iZ,iF010,iXP,iY,iZ,iF100,iXP,
                        iYP,iZP,iF111);

                    // 5461
                    ProcessTetrahedron(kVMap,kESet,kTSet,
                        iXP,iY,iZP,iF101,iX,iY,iZP,iF001,iXP,iYP,iZP,iF111,
                        iXP,iY,iZ,iF100);

                    // 7643
                    ProcessTetrahedron(kVMap,kESet,kTSet,
                        iX,iYP,iZP,iF011,iXP,iYP,iZP,iF111,iX,iY,iZP,iF001,iX,
                        iYP,iZ,iF010);

                    // 6314
                    ProcessTetrahedron(kVMap,kESet,kTSet,
                        iXP,iYP,iZP,iF111,iX,iYP,iZ,iF010,iXP,iY,iZ,iF100,iX,
                        iY,iZP,iF001);
                }
            }
        }
    }

    // readjust image so level set is F(x,y,z) = L
    for (i = 0; i < m_iXYZBound; i++)
        m_aiData[i] = m_aiData[i] + iLevel;

    // pack vertices into an array
    rkVA.resize(kVMap.size());
    if ( rkVA.size() > 0 )
    {
        VtxMapIterator pkVIter;
        for (pkVIter = kVMap.begin(); pkVIter != kVMap.end(); pkVIter++)
        {
            const Vertex& rkV = pkVIter->first;
            rkV.GetTriple(rkVA[pkVIter->second]);
        }
    }

    // pack edges into an array (computed, but not reported to caller)
    vector<EdgeKey> kEA;
    kEA.resize(kESet.size());
    if ( kEA.size() > 0 )
        copy(kESet.begin(),kESet.end(),&kEA.front());

    // pack triangles into an array
    rkTA.resize(kTSet.size());
    if ( rkTA.size() > 0 )
        copy(kTSet.begin(),kTSet.end(),&rkTA.front());
}
//----------------------------------------------------------------------------
void ExtractSurfaceTetra::MakeUnique (vector<Vector3f>& rkVA,
    vector<TriangleKey>& rkTA)
{
    int iVQuantity = (int)rkVA.size();
    int iTQuantity = (int)rkTA.size();
    if ( iVQuantity == 0 || iTQuantity == 0 )
        return;

    // use a hash table to generate unique storage
    VMap kVMap;
    VMapIterator pkVIter;
    for (int iV = 0, iNextVertex = 0; iV < iVQuantity; iV++)
    {
        // keep only unique vertices
        pair<VMapIterator,bool> kResult = kVMap.insert(
            make_pair(rkVA[iV],iNextVertex));
        if ( kResult.second == true )
            iNextVertex++;
    }

    // use a hash table to generate unique storage
    TMap kTMap;
    TMapIterator pkTIter;
    for (int iT = 0, iNextTriangle = 0; iT < iTQuantity; iT++)
    {
        // replace old vertex indices by new ones
        TriangleKey& rkTri = rkTA[iT];
        pkVIter = kVMap.find(rkVA[rkTri.V[0]]);
        assert( pkVIter != kVMap.end() );
        rkTri.V[0] = pkVIter->second;
        pkVIter = kVMap.find(rkVA[rkTri.V[1]]);
        assert( pkVIter != kVMap.end() );
        rkTri.V[1] = pkVIter->second;
        pkVIter = kVMap.find(rkVA[rkTri.V[2]]);
        assert( pkVIter != kVMap.end() );
        rkTri.V[2] = pkVIter->second;

        // keep only unique triangles
        pair<TMapIterator,bool> kResult = kTMap.insert(
            make_pair(rkTri,iNextTriangle));
        if ( kResult.second == true )
            iNextTriangle++;
    }

    // pack the vertices
    rkVA.resize(kVMap.size());
    for (pkVIter = kVMap.begin(); pkVIter != kVMap.end(); pkVIter++)
        rkVA[pkVIter->second] = pkVIter->first;

    // pack the triangles
    rkTA.resize(kTMap.size());
    for (pkTIter = kTMap.begin(); pkTIter != kTMap.end(); pkTIter++)
        rkTA[pkTIter->second] = pkTIter->first;
}
//----------------------------------------------------------------------------
void ExtractSurfaceTetra::OrientTriangles (vector<Vector3f>& rkVA,
    vector<TriangleKey>& rkTA, bool bSameDir)
{
    for (int i = 0; i < (int)rkTA.size(); i++)
    {
        TriangleKey& rkTri = rkTA[i];

        // get triangle vertices
        Vector3f kV0 = rkVA[rkTri.V[0]];
        Vector3f kV1 = rkVA[rkTri.V[1]];
        Vector3f kV2 = rkVA[rkTri.V[2]];
        
        // construct triangle normal based on current orientation
        Vector3f kEdge1 = kV1 - kV0;
        Vector3f kEdge2 = kV2 - kV0;
        Vector3f kNormal = kEdge1.Cross(kEdge2);

        // get the image gradient at the vertices
        Vector3f kGrad0 = GetGradient(kV0);
        Vector3f kGrad1 = GetGradient(kV1);
        Vector3f kGrad2 = GetGradient(kV2);

        // compute the average gradient
        Vector3f kGradAvr = (kGrad0 + kGrad1 + kGrad2)/3.0f;
        
        // compute the dot product of normal and average gradient
        float fDot = kGradAvr.Dot(kNormal);

        // choose triangle orientation based on gradient direction
        int iSave;
        if ( bSameDir )
        {
            if ( fDot < 0.0f )
            {
                // wrong orientation, reorder it
                iSave = rkTri.V[1];
                rkTri.V[1] = rkTri.V[2];
                rkTri.V[2] = iSave;
            }
        }
        else
        {
            if ( fDot > 0.0f )
            {
                // wrong orientation, reorder it
                iSave = rkTri.V[1];
                rkTri.V[1] = rkTri.V[2];
                rkTri.V[2] = iSave;
            }
        }
    }
}
//----------------------------------------------------------------------------
void ExtractSurfaceTetra::ComputeNormals (const vector<Vector3f>& rkVA,
    const vector<TriangleKey>& rkTA, vector<Vector3f>& rkNA)
{
    // maintain a running sum of triangle normals at each vertex
    int iVQuantity = (int)rkVA.size();
    int iTQuantity = (int)rkTA.size();
    rkNA.resize(iVQuantity);
    int i, j;
    for (i = 0; i < iVQuantity; i++)
        rkNA[i] = Vector3f::ZERO;

    for (i = 0, j = 0; i < iTQuantity; i++)
    {
        const TriangleKey& rkT = rkTA[i];
        Vector3f kV0 = rkVA[rkT.V[0]];
        Vector3f kV1 = rkVA[rkT.V[1]];
        Vector3f kV2 = rkVA[rkT.V[2]];

        // construct triangle normal
        Vector3f kEdge1 = kV1 - kV0;
        Vector3f kEdge2 = kV2 - kV0;
        Vector3f kNormal = kEdge1.Cross(kEdge2);

        // maintain the sum of normals at each vertex
        rkNA[rkT.V[0]] += kNormal;
        rkNA[rkT.V[1]] += kNormal;
        rkNA[rkT.V[2]] += kNormal;
    }

    // The normal vector storage was used to accumulate the sum of
    // triangle normals.  Now these vectors must be rescaled to be
    // unit length.
    for (i = 0; i < iVQuantity; i++)
        rkNA[i].Normalize();
}
//----------------------------------------------------------------------------
int ExtractSurfaceTetra::AddVertex (VtxMap& rkVMap, int iXNumer, int iXDenom,
    int iYNumer, int iYDenom, int iZNumer, int iZDenom)
{
    Vertex kVertex(iXNumer,iXDenom,iYNumer,iYDenom,iZNumer,iZDenom);
    VtxMapIterator pkVIter = rkVMap.find(kVertex);
    if ( pkVIter != rkVMap.end() )
    {
        // Vertex already in map, just return its unique index.
        return pkVIter->second;
    }
    else
    {
        // Vertex not in map, insert it and assign it a unique index.
        int i = m_iNextIndex++;
        rkVMap.insert(make_pair(kVertex,i));
        return i;
    }
}
//----------------------------------------------------------------------------
void ExtractSurfaceTetra::AddEdge (VtxMap& rkVMap, ESet& rkESet, int iXNumer0,
    int iXDenom0, int iYNumer0, int iYDenom0, int iZNumer0, int iZDenom0,
    int iXNumer1, int iXDenom1, int iYNumer1, int iYDenom1, int iZNumer1,
    int iZDenom1)
{
    int iV0 = AddVertex(rkVMap,iXNumer0,iXDenom0,iYNumer0,iYDenom0,iZNumer0,
        iZDenom0);

    int iV1 = AddVertex(rkVMap,iXNumer1,iXDenom1,iYNumer1,iYDenom1,iZNumer1,
        iZDenom1);

    rkESet.insert(EdgeKey(iV0,iV1));
}
//----------------------------------------------------------------------------
void ExtractSurfaceTetra::AddTriangle (VtxMap& rkVMap, ESet& rkESet,
    TSet& rkTSet, int iXNumer0, int iXDenom0, int iYNumer0, int iYDenom0,
    int iZNumer0, int iZDenom0, int iXNumer1, int iXDenom1, int iYNumer1,
    int iYDenom1, int iZNumer1, int iZDenom1, int iXNumer2, int iXDenom2,
    int iYNumer2, int iYDenom2, int iZNumer2, int iZDenom2)
{
    int iV0 = AddVertex(rkVMap,iXNumer0,iXDenom0,iYNumer0,iYDenom0,iZNumer0,
        iZDenom0);

    int iV1 = AddVertex(rkVMap,iXNumer1,iXDenom1,iYNumer1,iYDenom1,iZNumer1,
        iZDenom1);

    int iV2 = AddVertex(rkVMap,iXNumer2,iXDenom2,iYNumer2,iYDenom2,iZNumer2,
        iZDenom2);

    // nothing to do if triangle already exists
    TriangleKey kT(iV0,iV1,iV2);
    if ( rkTSet.find(kT) != rkTSet.end() )
        return;

    // prevent double-sided triangles
    int iSave = kT.V[1];
    kT.V[1] = kT.V[2];
    kT.V[2] = iSave;
    if ( rkTSet.find(kT) != rkTSet.end() )
        return;

    rkESet.insert(EdgeKey(iV0,iV1));
    rkESet.insert(EdgeKey(iV1,iV2));
    rkESet.insert(EdgeKey(iV2,iV0));

    // compute triangle normal assuming counterclockwise ordering
    Vector3f kV0(
        iXNumer0/(float)iXDenom0,
        iYNumer0/(float)iYDenom0,
        iZNumer0/(float)iZDenom0);

    Vector3f kV1(
        iXNumer1/(float)iXDenom1,
        iYNumer1/(float)iYDenom1,
        iZNumer1/(float)iZDenom1);

    Vector3f kV2(
        iXNumer2/(float)iXDenom2,
        iYNumer2/(float)iYDenom2,
        iZNumer2/(float)iZDenom2);

    Vector3f kE0 = kV1 - kV0;
    Vector3f kE1 = kV2 - kV0;
    Vector3f kN = kE0.Cross(kE1);

    // choose triangle orientation based on gradient direction
    Vector3f kCentroid = (kV0+kV1+kV2)/3.0f;
    Vector3f kGrad = GetGradient(kCentroid);
    if ( kGrad.Dot(kN) <= 0.0f )
        rkTSet.insert(TriangleKey(iV0,iV1,iV2));
    else
        rkTSet.insert(TriangleKey(iV0,iV2,iV1));
}
//----------------------------------------------------------------------------
void ExtractSurfaceTetra::ProcessTetrahedron (VtxMap& rkVM,
    ESet& rkES, TSet& rkTS, int iX0, int iY0, int iZ0, int iF0,
    int iX1, int iY1, int iZ1, int iF1, int iX2, int iY2, int iZ2, int iF2,
    int iX3, int iY3, int iZ3, int iF3)
{
    int iXN0, iYN0, iZN0, iD0;
    int iXN1, iYN1, iZN1, iD1;
    int iXN2, iYN2, iZN2, iD2;
    int iXN3, iYN3, iZN3, iD3;

    if ( iF0 != 0 )
    {
        // convert to case +***
        if ( iF0 < 0 )
        {
            iF0 = -iF0;
            iF1 = -iF1;
            iF2 = -iF2;
            iF3 = -iF3;
        }

        if ( iF1 > 0 )
        {
            if ( iF2 > 0 )
            {
                if ( iF3 > 0 )
                {
                    // ++++
                    return;
                }
                else if ( iF3 < 0 )
                {
                    // +++-
                    iD0 = iF0 - iF3;
                    iXN0 = iF0*iX3 - iF3*iX0;
                    iYN0 = iF0*iY3 - iF3*iY0;
                    iZN0 = iF0*iZ3 - iF3*iZ0;
                    iD1 = iF1 - iF3;
                    iXN1 = iF1*iX3 - iF3*iX1;
                    iYN1 = iF1*iY3 - iF3*iY1;
                    iZN1 = iF1*iZ3 - iF3*iZ1;
                    iD2 = iF2 - iF3;
                    iXN2 = iF2*iX3 - iF3*iX2;
                    iYN2 = iF2*iY3 - iF3*iY2;
                    iZN2 = iF2*iZ3 - iF3*iZ2;
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iXN2,iD2,iYN2,iD2,iZN2,iD2);
                }
                else
                {
                    // +++0
                    AddVertex(rkVM,iX3,1,iY3,1,iZ3,1);
                }
            }
            else if ( iF2 < 0 )
            {
                iD0 = iF0 - iF2;
                iXN0 = iF0*iX2 - iF2*iX0;
                iYN0 = iF0*iY2 - iF2*iY0;
                iZN0 = iF0*iZ2 - iF2*iZ0;
                iD1 = iF1 - iF2;
                iXN1 = iF1*iX2 - iF2*iX1;
                iYN1 = iF1*iY2 - iF2*iY1;
                iZN1 = iF1*iZ2 - iF2*iZ1;

                if ( iF3 > 0 )
                {
                    // ++-+
                    iD2 = iF3 - iF2;
                    iXN2 = iF3*iX2 - iF2*iX3;
                    iYN2 = iF3*iY2 - iF2*iY3;
                    iZN2 = iF3*iZ2 - iF2*iZ3;
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iXN2,iD2,iYN2,iD2,iZN2,iD2);
                }
                else if ( iF3 < 0 )
                {
                    // ++--
                    iD2 = iF0 - iF3;
                    iXN2 = iF0*iX3 - iF3*iX0;
                    iYN2 = iF0*iY3 - iF3*iY0;
                    iZN2 = iF0*iZ3 - iF3*iZ0;
                    iD3 = iF1 - iF3;
                    iXN3 = iF1*iX3 - iF3*iX1;
                    iYN3 = iF1*iY3 - iF3*iY1;
                    iZN3 = iF1*iZ3 - iF3*iZ1;
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iXN2,iD2,iYN2,iD2,iZN2,iD2);
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iXN3,iD3,iYN3,iD3,iZN3,iD3,
                        iXN2,iD2,iYN2,iD2,iZN2,iD2);
                }
                else
                {
                    // ++-0
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iX3,1,iY3,1,iZ3,1);
                }
            }
            else
            {
                if ( iF3 > 0 )
                {
                    // ++0+
                    AddVertex(rkVM,iX2,1,iY2,1,iZ2,1);
                }
                else if ( iF3 < 0 )
                {
                    // ++0-
                    iD0 = iF0 - iF3;
                    iXN0 = iF0*iX3 - iF3*iX0;
                    iYN0 = iF0*iY3 - iF3*iY0;
                    iZN0 = iF0*iZ3 - iF3*iZ0;
                    iD1 = iF1 - iF3;
                    iXN1 = iF1*iX3 - iF3*iX1;
                    iYN1 = iF1*iY3 - iF3*iY1;
                    iZN1 = iF1*iZ3 - iF3*iZ1;
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iX2,1,iY2,1,iZ2,1);
                }
                else
                {
                    // ++00
                    AddEdge(rkVM,rkES,iX2,1,iY2,1,iZ2,1,iX3,1,iY3,1,iZ3,1);
                }
            }
        }
        else if ( iF1 < 0 )
        {
            if ( iF2 > 0 )
            {
                iD0 = iF0 - iF1;
                iXN0 = iF0*iX1 - iF1*iX0;
                iYN0 = iF0*iY1 - iF1*iY0;
                iZN0 = iF0*iZ1 - iF1*iZ0;
                iD1 = iF2 - iF1;
                iXN1 = iF2*iX1 - iF1*iX2;
                iYN1 = iF2*iY1 - iF1*iY2;
                iZN1 = iF2*iZ1 - iF1*iZ2;

                if ( iF3 > 0 )
                {
                    // +-++
                    iD2 = iF3 - iF1;
                    iXN2 = iF3*iX1 - iF1*iX3;
                    iYN2 = iF3*iY1 - iF1*iY3;
                    iZN2 = iF3*iZ1 - iF1*iZ3;
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iXN2,iD2,iYN2,iD2,iZN2,iD2);
                }
                else if ( iF3 < 0 )
                {
                    // +-+-
                    iD2 = iF0 - iF3;
                    iXN2 = iF0*iX3 - iF3*iX0;
                    iYN2 = iF0*iY3 - iF3*iY0;
                    iZN2 = iF0*iZ3 - iF3*iZ0;
                    iD3 = iF2 - iF3;
                    iXN3 = iF2*iX3 - iF3*iX2;
                    iYN3 = iF2*iY3 - iF3*iY2;
                    iZN3 = iF2*iZ3 - iF3*iZ2;
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iXN2,iD2,iYN2,iD2,iZN2,iD2);
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iXN3,iD3,iYN3,iD3,iZN3,iD3,
                        iXN2,iD2,iYN2,iD2,iZN2,iD2);
                }
                else
                {
                    // +-+0
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iX3,1,iY3,1,iZ3,1);
                }
            }
            else if ( iF2 < 0 )
            {
                iD0 = iF1 - iF0;
                iXN0 = iF1*iX0 - iF0*iX1;
                iYN0 = iF1*iY0 - iF0*iY1;
                iZN0 = iF1*iZ0 - iF0*iZ1;
                iD1 = iF2 - iF0;
                iXN1 = iF2*iX0 - iF0*iX2;
                iYN1 = iF2*iY0 - iF0*iY2;
                iZN1 = iF2*iZ0 - iF0*iZ2;

                if ( iF3 > 0 )
                {
                    // +--+
                    iD2 = iF1 - iF3;
                    iXN2 = iF1*iX3 - iF3*iX1;
                    iYN2 = iF1*iY3 - iF3*iY1;
                    iZN2 = iF1*iZ3 - iF3*iZ1;
                    iD3 = iF2 - iF3;
                    iXN3 = iF2*iX3 - iF3*iX2;
                    iYN3 = iF2*iY3 - iF3*iY2;
                    iZN3 = iF2*iZ3 - iF3*iZ2;
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iXN2,iD2,iYN2,iD2,iZN2,iD2);
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iXN3,iD3,iYN3,iD3,iZN3,iD3,
                        iXN2,iD2,iYN2,iD2,iZN2,iD2);
                }
                else if ( iF3 < 0 )
                {
                    // +---
                    iD2 = iF3 - iF0;
                    iXN2 = iF3*iX0 - iF0*iX3;
                    iYN2 = iF3*iY0 - iF0*iY3;
                    iZN2 = iF3*iZ0 - iF0*iZ3;
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iXN2,iD2,iYN2,iD2,iZN2,iD2);
                }
                else
                {
                    // +--0
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iX3,1,iY3,1,iZ3,1);
                }
            }
            else
            {
                iD0 = iF1 - iF0;
                iXN0 = iF1*iX0 - iF0*iX1;
                iYN0 = iF1*iY0 - iF0*iY1;
                iZN0 = iF1*iZ0 - iF0*iZ1;

                if ( iF3 > 0 )
                {
                    // +-0+
                    iD1 = iF1 - iF3;
                    iXN1 = iF1*iX3 - iF3*iX1;
                    iYN1 = iF1*iY3 - iF3*iY1;
                    iZN1 = iF1*iZ3 - iF3*iZ1;
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iX2,1,iY2,1,iZ2,1);
                }
                else if ( iF3 < 0 )
                {
                    // +-0-
                    iD1 = iF3 - iF0;
                    iXN1 = iF3*iX0 - iF0*iX3;
                    iYN1 = iF3*iY0 - iF0*iY3;
                    iZN1 = iF3*iZ0 - iF0*iZ3;
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iX2,1,iY2,1,iZ2,1);
                }
                else
                {
                    // +-00
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iX2,1,iY2,1,iZ2,1,
                        iX3,1,iY3,1,iZ3,1);
                }
            }
        }
        else
        {
            if ( iF2 > 0 )
            {
                if ( iF3 > 0 )
                {
                    // +0++
                    AddVertex(rkVM,iX1,1,iY1,1,iZ1,1);
                }
                else if ( iF3 < 0 )
                {
                    // +0+-
                    iD0 = iF0 - iF3;
                    iXN0 = iF0*iX3 - iF3*iX0;
                    iYN0 = iF0*iY3 - iF3*iY0;
                    iZN0 = iF0*iZ3 - iF3*iZ0;
                    iD1 = iF2 - iF3;
                    iXN1 = iF2*iX3 - iF3*iX2;
                    iYN1 = iF2*iY3 - iF3*iY2;
                    iZN1 = iF2*iZ3 - iF3*iZ2;
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iX1,1,iY1,1,iZ1,1);
                }
                else
                {
                    // +0+0
                    AddEdge(rkVM,rkES,iX1,1,iY1,1,iZ1,1,iX3,1,iY3,1,iZ3,1);
                }
            }
            else if ( iF2 < 0 )
            {
                iD0 = iF2 - iF0;
                iXN0 = iF2*iX0 - iF0*iX2;
                iYN0 = iF2*iY0 - iF0*iY2;
                iZN0 = iF2*iZ0 - iF0*iZ2;

                if ( iF3 > 0 )
                {
                    // +0-+
                    iD1 = iF2 - iF3;
                    iXN1 = iF2*iX3 - iF3*iX2;
                    iYN1 = iF2*iY3 - iF3*iY2;
                    iZN1 = iF2*iZ3 - iF3*iZ2;
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iX1,1,iY1,1,iZ1,1);
                }
                else if ( iF3 < 0 )
                {
                    // +0--
                    iD1 = iF0 - iF3;
                    iXN1 = iF0*iX3 - iF3*iX0;
                    iYN1 = iF0*iY3 - iF3*iY0;
                    iZN1 = iF0*iZ3 - iF3*iZ0;
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iXN1,iD1,iYN1,iD1,iZN1,iD1,
                        iX1,1,iY1,1,iZ1,1);
                }
                else
                {
                    // +0-0
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iX1,1,iY1,1,iZ1,1,
                        iX3,1,iY3,1,iZ3,1);
                }
            }
            else
            {
                if ( iF3 > 0 )
                {
                    // +00+
                    AddEdge(rkVM,rkES,iX1,1,iY1,1,iZ1,1,iX2,1,iY2,1,iZ2,1);
                }
                else if ( iF3 < 0 )
                {
                    // +00-
                    iD0 = iF0 - iF3;
                    iXN0 = iF0*iX3 - iF3*iX0;
                    iYN0 = iF0*iY3 - iF3*iY0;
                    iZN0 = iF0*iZ3 - iF3*iZ0;
                    AddTriangle(rkVM,rkES,rkTS,
                        iXN0,iD0,iYN0,iD0,iZN0,iD0,
                        iX1,1,iY1,1,iZ1,1,
                        iX2,1,iY2,1,iZ2,1);
                }
                else
                {
                    // +000
                    AddTriangle(rkVM,rkES,rkTS,
                        iX1,1,iY1,1,iZ1,1,
                        iX2,1,iY2,1,iZ2,1,
                        iX3,1,iY3,1,iZ3,1);
                }
            }
        }
    }
    else if ( iF1 != 0 )
    {
        // convert to case 0+**
        if ( iF1 < 0 )
        {
            iF1 = -iF1;
            iF2 = -iF2;
            iF3 = -iF3;
        }

        if ( iF2 > 0 )
        {
            if ( iF3 > 0 )
            {
                // 0+++
                AddVertex(rkVM,iX0,1,iY0,1,iZ0,1);
            }
            else if ( iF3 < 0 )
            {
                // 0++-
                iD0 = iF2 - iF3;
                iXN0 = iF2*iX3 - iF3*iX2;
                iYN0 = iF2*iY3 - iF3*iY2;
                iZN0 = iF2*iZ3 - iF3*iZ2;
                iD1 = iF1 - iF3;
                iXN1 = iF1*iX3 - iF3*iX1;
                iYN1 = iF1*iY3 - iF3*iY1;
                iZN1 = iF1*iZ3 - iF3*iZ1;
                AddTriangle(rkVM,rkES,rkTS,
                    iXN0,iD0,iYN0,iD0,iZN0,iD0,
                    iXN1,iD1,iYN1,iD1,iZN1,iD1,
                    iX0,1,iY0,1,iZ0,1);
            }
            else
            {
                // 0++0
                AddEdge(rkVM,rkES,iX0,1,iY0,1,iZ0,1,iX3,1,iY3,1,iZ3,1);
            }
        }
        else if ( iF2 < 0 )
        {
            iD0 = iF2 - iF1;
            iXN0 = iF2*iX1 - iF1*iX2;
            iYN0 = iF2*iY1 - iF1*iY2;
            iZN0 = iF2*iZ1 - iF1*iZ2;

            if ( iF3 > 0 )
            {
                // 0+-+
                iD1 = iF2 - iF3;
                iXN1 = iF2*iX3 - iF3*iX2;
                iYN1 = iF2*iY3 - iF3*iY2;
                iZN1 = iF2*iZ3 - iF3*iZ2;
                AddTriangle(rkVM,rkES,rkTS,
                    iXN0,iD0,iYN0,iD0,iZN0,iD0,
                    iXN1,iD1,iYN1,iD1,iZN1,iD1,
                    iX0,1,iY0,1,iZ0,1);
            }
            else if ( iF3 < 0 )
            {
                // 0+--
                iD1 = iF1 - iF3;
                iXN1 = iF1*iX3 - iF3*iX1;
                iYN1 = iF1*iY3 - iF3*iY1;
                iZN1 = iF1*iZ3 - iF3*iZ1;
                AddTriangle(rkVM,rkES,rkTS,
                    iXN0,iD0,iYN0,iD0,iZN0,iD0,
                    iXN1,iD1,iYN1,iD1,iZN1,iD1,
                    iX0,1,iY0,1,iZ0,1);
            }
            else
            {
                // 0+-0
                AddTriangle(rkVM,rkES,rkTS,
                    iXN0,iD0,iYN0,iD0,iZN0,iD0,
                    iX0,1,iY0,1,iZ0,1,
                    iX3,1,iY3,1,iZ3,1);
            }
        }
        else
        {
            if ( iF3 > 0 )
            {
                // 0+0+
                AddEdge(rkVM,rkES,iX0,1,iY0,1,iZ0,1,iX2,1,iY2,1,iZ2,1);
            }
            else if ( iF3 < 0 )
            {
                // 0+0-
                iD0 = iF1 - iF3;
                iXN0 = iF1*iX3 - iF3*iX1;
                iYN0 = iF1*iY3 - iF3*iY1;
                iZN0 = iF1*iZ3 - iF3*iZ1;
                AddTriangle(rkVM,rkES,rkTS,
                    iXN0,iD0,iYN0,iD0,iZN0,iD0,
                    iX0,1,iY0,1,iZ0,1,
                    iX2,1,iY2,1,iZ2,1);
            }
            else
            {
                // 0+00
                AddTriangle(rkVM,rkES,rkTS,
                    iX0,1,iY0,1,iZ0,1,
                    iX2,1,iY2,1,iZ2,1,
                    iX3,1,iY3,1,iZ3,1);
            }
        }
    }
    else if ( iF2 != 0 )
    {
        // convert to case 00+*
        if ( iF2 < 0 )
        {
            iF2 = -iF2;
            iF3 = -iF3;
        }

        if ( iF3 > 0 )
        {
            // 00++
            AddEdge(rkVM,rkES,iX0,1,iY0,1,iZ0,1,iX1,1,iY1,1,iZ1,1);
        }
        else if ( iF3 < 0 )
        {
            // 00+-
            iD0 = iF2 - iF3;
            iXN0 = iF2*iX3 - iF3*iX2;
            iYN0 = iF2*iY3 - iF3*iY2;
            iZN0 = iF2*iZ3 - iF3*iZ2;
            AddTriangle(rkVM,rkES,rkTS,
                iXN0,iD0,iYN0,iD0,iZN0,iD0,
                iX0,1,iY0,1,iZ0,1,
                iX1,1,iY1,1,iZ1,1);
        }
        else
        {
            // 00+0
            AddTriangle(rkVM,rkES,rkTS,
                iX0,1,iY0,1,iZ0,1,
                iX1,1,iY1,1,iZ1,1,
                iX3,1,iY3,1,iZ3,1);
        }
    }
    else if ( iF3 != 0 )
    {
        // cases 000+ or 000-
        AddTriangle(rkVM,rkES,rkTS,
            iX0,1,iY0,1,iZ0,1,
            iX1,1,iY1,1,iZ1,1,
            iX2,1,iY2,1,iZ2,1);
    }
    else
    {
        // case 0000
        AddTriangle(rkVM,rkES,rkTS,
            iX0,1,iY0,1,iZ0,1,
            iX1,1,iY1,1,iZ1,1,
            iX2,1,iY2,1,iZ2,1);
        AddTriangle(rkVM,rkES,rkTS,
            iX0,1,iY0,1,iZ0,1,
            iX1,1,iY1,1,iZ1,1,
            iX3,1,iY3,1,iZ3,1);
        AddTriangle(rkVM,rkES,rkTS,
            iX0,1,iY0,1,iZ0,1,
            iX2,1,iY2,1,iZ2,1,
            iX3,1,iY3,1,iZ3,1);
        AddTriangle(rkVM,rkES,rkTS,
            iX1,1,iY1,1,iZ1,1,
            iX2,1,iY2,1,iZ2,1,
            iX3,1,iY3,1,iZ3,1);
    }
}
//----------------------------------------------------------------------------
ExtractSurfaceTetra::Vertex::Vertex (int iXNumer, int iXDenom, int iYNumer,
    int iYDenom, int iZNumer, int iZDenom)
{
    if ( iXDenom > 0 )
    {
        m_iXNumer = iXNumer;
        m_iXDenom = iXDenom;
    }
    else
    {
        m_iXNumer = -iXNumer;
        m_iXDenom = -iXDenom;
    }

    if ( iYDenom > 0 )
    {
        m_iYNumer = iYNumer;
        m_iYDenom = iYDenom;
    }
    else
    {
        m_iYNumer = -iYNumer;
        m_iYDenom = -iYDenom;
    }

    if ( iZDenom > 0 )
    {
        m_iZNumer = iZNumer;
        m_iZDenom = iZDenom;
    }
    else
    {
        m_iZNumer = -iZNumer;
        m_iZDenom = -iZDenom;
    }
}
//----------------------------------------------------------------------------
bool ExtractSurfaceTetra::Vertex::operator< (const Vertex& rkVertex) const
{
    unsigned int auiValue0[6] =
    {
        *(unsigned int*)&m_iXNumer,
        *(unsigned int*)&m_iXDenom,
        *(unsigned int*)&m_iYNumer,
        *(unsigned int*)&m_iYDenom,
        *(unsigned int*)&m_iZNumer,
        *(unsigned int*)&m_iZDenom
    };

    unsigned int auiValue1[6] =
    {
        *(unsigned int*)&rkVertex.m_iXNumer,
        *(unsigned int*)&rkVertex.m_iXDenom,
        *(unsigned int*)&rkVertex.m_iYNumer,
        *(unsigned int*)&rkVertex.m_iYDenom,
        *(unsigned int*)&rkVertex.m_iZNumer,
        *(unsigned int*)&rkVertex.m_iZDenom
    };

    for (int i = 0; i < 6; i++)
    {
        if ( auiValue0[i] < auiValue1[i] )
            return true;
        if ( auiValue0[i] > auiValue1[i] )
            return false;
    }

    return false;
}
//----------------------------------------------------------------------------
void ExtractSurfaceTetra::Vertex::GetTriple (Vector3f& rkMeshVertex)
    const
{
    rkMeshVertex.X() = float(m_iXNumer)/float(m_iXDenom);
    rkMeshVertex.Y() = float(m_iYNumer)/float(m_iYDenom);
    rkMeshVertex.Z() = float(m_iZNumer)/float(m_iZDenom);
}
//----------------------------------------------------------------------------
