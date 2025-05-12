// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlExtractSurfaceCubes.h"
using namespace Wml;

#include <map>
using namespace std;

typedef map<Vector3f,int> VMap;
typedef VMap::iterator VMapIterator;
typedef map<TriangleKey,int> TMap;
typedef TMap::iterator TMapIterator;

//----------------------------------------------------------------------------
ExtractSurfaceCubes::ExtractSurfaceCubes(int iXBound, int iYBound,
    int iZBound, int* aiData)
{
    assert( iXBound > 0 && iYBound > 0 && iZBound > 0 && aiData );
    m_iXBound = iXBound;
    m_iYBound = iYBound;
    m_iZBound = iZBound;
    m_iXYBound = iXBound*iYBound;
    m_aiData = aiData;
}
//----------------------------------------------------------------------------
void ExtractSurfaceCubes::ExtractContour (float fLevel,
    vector<Vector3f>& rkVA, vector<TriangleKey>& rkTA)
{
    rkVA.clear();
    rkTA.clear();

    for (int iZ = 0; iZ < m_iZBound-1; iZ++)
    {
        for (int iY = 0; iY < m_iYBound-1; iY++)
        {
            for (int iX = 0; iX < m_iXBound-1; iX++)
            {
                // get vertices on edges of box (if any)
                VETable kTable;
                int iType = GetVertices(fLevel,iX,iY,iZ,kTable);
                if ( iType != 0 )
                {
                    // get edges on faces of box
                    GetXMinEdges(iX,iY,iZ,iType,kTable);
                    GetXMaxEdges(iX,iY,iZ,iType,kTable);
                    GetYMinEdges(iX,iY,iZ,iType,kTable);
                    GetYMaxEdges(iX,iY,iZ,iType,kTable);
                    GetZMinEdges(iX,iY,iZ,iType,kTable);
                    GetZMaxEdges(iX,iY,iZ,iType,kTable);

                    // ear-clip the wireframe mesh
                    kTable.RemoveTriangles(rkVA,rkTA);
                }
            }
        }
    }
}
//----------------------------------------------------------------------------
void ExtractSurfaceCubes::MakeUnique (vector<Vector3f>& rkVA,
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
void ExtractSurfaceCubes::OrientTriangles (vector<Vector3f>& rkVA,
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
void ExtractSurfaceCubes::ComputeNormals (const vector<Vector3f>& rkVA,
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
int ExtractSurfaceCubes::GetVertices (float fLevel, int iX, int iY, int iZ,
    VETable& rkTable)
{
    int iType = 0;

    // get image values at corners of voxel
    int i000 = iX + m_iXBound*(iY + m_iYBound*iZ);
    int i100 = i000 + 1;
    int i010 = i000 + m_iXBound;
    int i110 = i010 + 1;
    int i001 = i000 + m_iXYBound;
    int i101 = i001 + 1;
    int i011 = i001 + m_iXBound;
    int i111 = i011 + 1;
    float fF000 = (float)m_aiData[i000];
    float fF100 = (float)m_aiData[i100];
    float fF010 = (float)m_aiData[i010];
    float fF110 = (float)m_aiData[i110];
    float fF001 = (float)m_aiData[i001];
    float fF101 = (float)m_aiData[i101];
    float fF011 = (float)m_aiData[i011];
    float fF111 = (float)m_aiData[i111];

    float fX0 = (float)iX, fY0 = (float)iY, fZ0 = (float)iZ;
    float fX1 = fX0+1.0f, fY1 = fY0+1.0f, fZ1 = fZ0+1.0f;

    // xmin-ymin edge
    float fDiff0 = fLevel - fF000;
    float fDiff1 = fLevel - fF001;
    if ( fDiff0*fDiff1 < 0.0f )
    {
        iType |= EB_XMIN_YMIN;
        rkTable.Insert(EI_XMIN_YMIN,
            Vector3f(fX0,fY0,fZ0+fDiff0/(fF001-fF000)));
    }

    // xmin-ymax edge
    fDiff0 = fLevel - fF010;
    fDiff1 = fLevel - fF011;
    if ( fDiff0*fDiff1 < 0.0f )
    {
        iType |= EB_XMIN_YMAX;
        rkTable.Insert(EI_XMIN_YMAX,
            Vector3f(fX0,fY1,fZ0+fDiff0/(fF011-fF010)));
    }

    // xmax-ymin edge
    fDiff0 = fLevel - fF100;
    fDiff1 = fLevel - fF101;
    if ( fDiff0*fDiff1 < 0.0f )
    {
        iType |= EB_XMAX_YMIN;
        rkTable.Insert(EI_XMAX_YMIN,
            Vector3f(fX1,fY0,fZ0+fDiff0/(fF101-fF100)));
    }

    // xmax-ymax edge
    fDiff0 = fLevel - fF110;
    fDiff1 = fLevel - fF111;
    if ( fDiff0*fDiff1 < 0.0f )
    {
        iType |= EB_XMAX_YMAX;
        rkTable.Insert(EI_XMAX_YMAX,
            Vector3f(fX1,fY1,fZ0+fDiff0/(fF111-fF110)));
    }

    // xmin-zmin edge
    fDiff0 = fLevel - fF000;
    fDiff1 = fLevel - fF010;
    if ( fDiff0*fDiff1 < 0.0f )
    {
        iType |= EB_XMIN_ZMIN;
        rkTable.Insert(EI_XMIN_ZMIN,
            Vector3f(fX0,fY0+fDiff0/(fF010-fF000),fZ0));
    }

    // xmin-zmax edge
    fDiff0 = fLevel - fF001;
    fDiff1 = fLevel - fF011;
    if ( fDiff0*fDiff1 < 0.0f )
    {
        iType |= EB_XMIN_ZMAX;
        rkTable.Insert(EI_XMIN_ZMAX,
            Vector3f(fX0,fY0+fDiff0/(fF011-fF001),fZ1));
    }

    // xmax-zmin edge
    fDiff0 = fLevel - fF100;
    fDiff1 = fLevel - fF110;
    if ( fDiff0*fDiff1 < 0.0f )
    {
        iType |= EB_XMAX_ZMIN;
        rkTable.Insert(EI_XMAX_ZMIN,
            Vector3f(fX1,fY0+fDiff0/(fF110-fF100),fZ0));
    }

    // xmax-zmax edge
    fDiff0 = fLevel - fF101;
    fDiff1 = fLevel - fF111;
    if ( fDiff0*fDiff1 < 0.0f )
    {
        iType |= EB_XMAX_ZMAX;
        rkTable.Insert(EI_XMAX_ZMAX,
            Vector3f(fX1,fY0+fDiff0/(fF111-fF101),fZ1));
    }

    // ymin-zmin edge
    fDiff0 = fLevel - fF000;
    fDiff1 = fLevel - fF100;
    if ( fDiff0*fDiff1 < 0.0f )
    {
        iType |= EB_YMIN_ZMIN;
        rkTable.Insert(EI_YMIN_ZMIN,
            Vector3f(fX0+fDiff0/(fF100-fF000),fY0,fZ0));
    }

    // ymin-zmax edge
    fDiff0 = fLevel - fF001;
    fDiff1 = fLevel - fF101;
    if ( fDiff0*fDiff1 < 0.0f )
    {
        iType |= EB_YMIN_ZMAX;
        rkTable.Insert(EI_YMIN_ZMAX,
            Vector3f(fX0+fDiff0/(fF101-fF001),fY0,fZ1));
    }

    // ymax-zmin edge
    fDiff0 = fLevel - fF010;
    fDiff1 = fLevel - fF110;
    if ( fDiff0*fDiff1 < 0.0f )
    {
        iType |= EB_YMAX_ZMIN;
        rkTable.Insert(EI_YMAX_ZMIN,
            Vector3f(fX0+fDiff0/(fF110-fF010),fY1,fZ0));
    }

    // ymax-zmax edge
    fDiff0 = fLevel - fF011;
    fDiff1 = fLevel - fF111;
    if ( fDiff0*fDiff1 < 0.0f )
    {
        iType |= EB_YMAX_ZMAX;
        rkTable.Insert(EI_YMAX_ZMAX,
            Vector3f(fX0+fDiff0/(fF111-fF011),fY1,fZ1));
    }

    return iType;
}
//----------------------------------------------------------------------------
void ExtractSurfaceCubes::GetXMinEdges (int iX, int iY, int iZ, int iType,
    VETable& rkTable)
{
    int iFaceType = 0;
    if ( iType & EB_XMIN_YMIN ) iFaceType |= 0x01;
    if ( iType & EB_XMIN_YMAX ) iFaceType |= 0x02;
    if ( iType & EB_XMIN_ZMIN ) iFaceType |= 0x04;
    if ( iType & EB_XMIN_ZMAX ) iFaceType |= 0x08;

    switch ( iFaceType )
    {
    case  0: return;
    case  3: rkTable.Insert(EI_XMIN_YMIN,EI_XMIN_YMAX); break;
    case  5: rkTable.Insert(EI_XMIN_YMIN,EI_XMIN_ZMIN); break;
    case  6: rkTable.Insert(EI_XMIN_YMAX,EI_XMIN_ZMIN); break;
    case  9: rkTable.Insert(EI_XMIN_YMIN,EI_XMIN_ZMAX); break;
    case 10: rkTable.Insert(EI_XMIN_YMAX,EI_XMIN_ZMAX); break;
    case 12: rkTable.Insert(EI_XMIN_ZMIN,EI_XMIN_ZMAX); break;
    case 15:
    {
        // four vertices, one per edge, need to disambiguate
        int i = iX + m_iXBound*(iY + m_iYBound*iZ);
        int iF00 = m_aiData[i];  // F(x,y,z)
        i += m_iXBound;
        int iF10 = m_aiData[i];  // F(x,y+1,z)
        i += m_iXYBound;
        int iF11 = m_aiData[i];  // F(x,y+1,z+1)
        i -= m_iXBound;
        int iF01 = m_aiData[i];  // F(x,y,z+1)
        int iDet = iF00*iF11 - iF01*iF10;

        if ( iDet > 0 )
        {
            // disjoint hyperbolic segments, pair <P0,P2>, <P1,P3>
            rkTable.Insert(EI_XMIN_YMIN,EI_XMIN_ZMIN);
            rkTable.Insert(EI_XMIN_YMAX,EI_XMIN_ZMAX);
        }
        else if ( iDet < 0 )
        {
            // disjoint hyperbolic segments, pair <P0,P3>, <P1,P2>
            rkTable.Insert(EI_XMIN_YMIN,EI_XMIN_ZMAX);
            rkTable.Insert(EI_XMIN_YMAX,EI_XMIN_ZMIN);
        }
        else
        {
            // plus-sign configuration, add branch point to tessellation
            rkTable.Insert(FI_XMIN,Vector3f(rkTable.GetX(EI_XMIN_ZMIN),
                rkTable.GetY(EI_XMIN_ZMIN),rkTable.GetZ(EI_XMIN_YMIN)));

            // add edges sharing the branch point
            rkTable.Insert(EI_XMIN_YMIN,FI_XMIN);
            rkTable.Insert(EI_XMIN_YMAX,FI_XMIN);
            rkTable.Insert(EI_XMIN_ZMIN,FI_XMIN);
            rkTable.Insert(EI_XMIN_ZMAX,FI_XMIN);
        }
        break;
    }
    default:  assert( false );
    }
}
//----------------------------------------------------------------------------
void ExtractSurfaceCubes::GetXMaxEdges (int iX, int iY, int iZ, int iType,
    VETable& rkTable)
{
    int iFaceType = 0;
    if ( iType & EB_XMAX_YMIN ) iFaceType |= 0x01;
    if ( iType & EB_XMAX_YMAX ) iFaceType |= 0x02;
    if ( iType & EB_XMAX_ZMIN ) iFaceType |= 0x04;
    if ( iType & EB_XMAX_ZMAX ) iFaceType |= 0x08;

    switch ( iFaceType )
    {
    case  0: return;
    case  3: rkTable.Insert(EI_XMAX_YMIN,EI_XMAX_YMAX); break;
    case  5: rkTable.Insert(EI_XMAX_YMIN,EI_XMAX_ZMIN); break;
    case  6: rkTable.Insert(EI_XMAX_YMAX,EI_XMAX_ZMIN); break;
    case  9: rkTable.Insert(EI_XMAX_YMIN,EI_XMAX_ZMAX); break;
    case 10: rkTable.Insert(EI_XMAX_YMAX,EI_XMAX_ZMAX); break;
    case 12: rkTable.Insert(EI_XMAX_ZMIN,EI_XMAX_ZMAX); break;
    case 15:
    {
        // four vertices, one per edge, need to disambiguate
        int i = (iX+1) + m_iXBound*(iY + m_iYBound*iZ);
        int iF00 = m_aiData[i];  // F(x,y,z)
        i += m_iXBound;
        int iF10 = m_aiData[i];  // F(x,y+1,z)
        i += m_iXYBound;
        int iF11 = m_aiData[i];  // F(x,y+1,z+1)
        i -= m_iXBound;
        int iF01 = m_aiData[i];  // F(x,y,z+1)
        int iDet = iF00*iF11 - iF01*iF10;

        if ( iDet > 0 )
        {
            // disjoint hyperbolic segments, pair <P0,P2>, <P1,P3>
            rkTable.Insert(EI_XMAX_YMIN,EI_XMAX_ZMIN);
            rkTable.Insert(EI_XMAX_YMAX,EI_XMAX_ZMAX);
        }
        else if ( iDet < 0 )
        {
            // disjoint hyperbolic segments, pair <P0,P3>, <P1,P2>
            rkTable.Insert(EI_XMAX_YMIN,EI_XMAX_ZMAX);
            rkTable.Insert(EI_XMAX_YMAX,EI_XMAX_ZMIN);
        }
        else
        {
            // plus-sign configuration, add branch point to tessellation
            rkTable.Insert(FI_XMAX,Vector3f(rkTable.GetX(EI_XMAX_ZMIN),
                rkTable.GetY(EI_XMAX_ZMIN),rkTable.GetZ(EI_XMAX_YMIN)));

            // add edges sharing the branch point
            rkTable.Insert(EI_XMAX_YMIN,FI_XMAX);
            rkTable.Insert(EI_XMAX_YMAX,FI_XMAX);
            rkTable.Insert(EI_XMAX_ZMIN,FI_XMAX);
            rkTable.Insert(EI_XMAX_ZMAX,FI_XMAX);
        }
        break;
    }
    default:  assert( false );
    }
}
//----------------------------------------------------------------------------
void ExtractSurfaceCubes::GetYMinEdges (int iX, int iY, int iZ, int iType,
    VETable& rkTable)
{
    int iFaceType = 0;
    if ( iType & EB_XMIN_YMIN ) iFaceType |= 0x01;
    if ( iType & EB_XMAX_YMIN ) iFaceType |= 0x02;
    if ( iType & EB_YMIN_ZMIN ) iFaceType |= 0x04;
    if ( iType & EB_YMIN_ZMAX ) iFaceType |= 0x08;

    switch ( iFaceType )
    {
    case  0: return;
    case  3: rkTable.Insert(EI_XMIN_YMIN,EI_XMAX_YMIN); break;
    case  5: rkTable.Insert(EI_XMIN_YMIN,EI_YMIN_ZMIN); break;
    case  6: rkTable.Insert(EI_XMAX_YMIN,EI_YMIN_ZMIN); break;
    case  9: rkTable.Insert(EI_XMIN_YMIN,EI_YMIN_ZMAX); break;
    case 10: rkTable.Insert(EI_XMAX_YMIN,EI_YMIN_ZMAX); break;
    case 12: rkTable.Insert(EI_YMIN_ZMIN,EI_YMIN_ZMAX); break;
    case 15:
    {
        // four vertices, one per edge, need to disambiguate
        int i = iX + m_iXBound*(iY + m_iYBound*iZ);
        int iF00 = m_aiData[i];  // F(x,y,z)
        i++;
        int iF10 = m_aiData[i];  // F(x+1,y,z)
        i += m_iXYBound;
        int iF11 = m_aiData[i];  // F(x+1,y,z+1)
        i--;
        int iF01 = m_aiData[i];  // F(x,y,z+1)
        int iDet = iF00*iF11 - iF01*iF10;

        if ( iDet > 0 )
        {
            // disjoint hyperbolic segments, pair <P0,P2>, <P1,P3>
            rkTable.Insert(EI_XMIN_YMIN,EI_YMIN_ZMIN);
            rkTable.Insert(EI_XMAX_YMIN,EI_YMIN_ZMAX);
        }
        else if ( iDet < 0 )
        {
            // disjoint hyperbolic segments, pair <P0,P3>, <P1,P2>
            rkTable.Insert(EI_XMIN_YMIN,EI_YMIN_ZMAX);
            rkTable.Insert(EI_XMAX_YMIN,EI_YMIN_ZMIN);
        }
        else
        {
            // plus-sign configuration, add branch point to tessellation
            rkTable.Insert(FI_YMIN,Vector3f(rkTable.GetX(EI_YMIN_ZMIN),
                rkTable.GetY(EI_XMIN_YMIN),rkTable.GetZ(EI_XMIN_YMIN)));

            // add edges sharing the branch point
            rkTable.Insert(EI_XMIN_YMIN,FI_YMIN);
            rkTable.Insert(EI_XMAX_YMIN,FI_YMIN);
            rkTable.Insert(EI_YMIN_ZMIN,FI_YMIN);
            rkTable.Insert(EI_YMIN_ZMAX,FI_YMIN);
        }
        break;
    }
    default:  assert( false );
    }
}
//----------------------------------------------------------------------------
void ExtractSurfaceCubes::GetYMaxEdges (int iX, int iY, int iZ, int iType,
    VETable& rkTable)
{
    int iFaceType = 0;
    if ( iType & EB_XMIN_YMAX ) iFaceType |= 0x01;
    if ( iType & EB_XMAX_YMAX ) iFaceType |= 0x02;
    if ( iType & EB_YMAX_ZMIN ) iFaceType |= 0x04;
    if ( iType & EB_YMAX_ZMAX ) iFaceType |= 0x08;

    switch ( iFaceType )
    {
    case  0: return;
    case  3: rkTable.Insert(EI_XMIN_YMAX,EI_XMAX_YMAX); break;
    case  5: rkTable.Insert(EI_XMIN_YMAX,EI_YMAX_ZMIN); break;
    case  6: rkTable.Insert(EI_XMAX_YMAX,EI_YMAX_ZMIN); break;
    case  9: rkTable.Insert(EI_XMIN_YMAX,EI_YMAX_ZMAX); break;
    case 10: rkTable.Insert(EI_XMAX_YMAX,EI_YMAX_ZMAX); break;
    case 12: rkTable.Insert(EI_YMAX_ZMIN,EI_YMAX_ZMAX); break;
    case 15:
    {
        // four vertices, one per edge, need to disambiguate
        int i = iX + m_iXBound*((iY+1) + m_iYBound*iZ);
        int iF00 = m_aiData[i];  // F(x,y,z)
        i++;
        int iF10 = m_aiData[i];  // F(x+1,y,z)
        i += m_iXYBound;
        int iF11 = m_aiData[i];  // F(x+1,y,z+1)
        i--;
        int iF01 = m_aiData[i];  // F(x,y,z+1)
        int iDet = iF00*iF11 - iF01*iF10;

        if ( iDet > 0 )
        {
            // disjoint hyperbolic segments, pair <P0,P2>, <P1,P3>
            rkTable.Insert(EI_XMIN_YMAX,EI_YMAX_ZMIN);
            rkTable.Insert(EI_XMAX_YMAX,EI_YMAX_ZMAX);
        }
        else if ( iDet < 0 )
        {
            // disjoint hyperbolic segments, pair <P0,P3>, <P1,P2>
            rkTable.Insert(EI_XMIN_YMAX,EI_YMAX_ZMAX);
            rkTable.Insert(EI_XMAX_YMAX,EI_YMAX_ZMIN);
        }
        else
        {
            // plus-sign configuration, add branch point to tessellation
            rkTable.Insert(FI_YMAX,Vector3f(rkTable.GetX(EI_YMAX_ZMIN),
                rkTable.GetY(EI_XMIN_YMAX),rkTable.GetZ(EI_XMIN_YMAX)));

            // add edges sharing the branch point
            rkTable.Insert(EI_XMIN_YMAX,FI_YMAX);
            rkTable.Insert(EI_XMAX_YMAX,FI_YMAX);
            rkTable.Insert(EI_YMAX_ZMIN,FI_YMAX);
            rkTable.Insert(EI_YMAX_ZMAX,FI_YMAX);
        }
        break;
    }
    default:  assert( false );
    }
}
//----------------------------------------------------------------------------
void ExtractSurfaceCubes::GetZMinEdges (int iX, int iY, int iZ, int iType,
    VETable& rkTable)
{
    int iFaceType = 0;
    if ( iType & EB_XMIN_ZMIN ) iFaceType |= 0x01;
    if ( iType & EB_XMAX_ZMIN ) iFaceType |= 0x02;
    if ( iType & EB_YMIN_ZMIN ) iFaceType |= 0x04;
    if ( iType & EB_YMAX_ZMIN ) iFaceType |= 0x08;

    switch ( iFaceType )
    {
    case  0: return;
    case  3: rkTable.Insert(EI_XMIN_ZMIN,EI_XMAX_ZMIN); break;
    case  5: rkTable.Insert(EI_XMIN_ZMIN,EI_YMIN_ZMIN); break;
    case  6: rkTable.Insert(EI_XMAX_ZMIN,EI_YMIN_ZMIN); break;
    case  9: rkTable.Insert(EI_XMIN_ZMIN,EI_YMAX_ZMIN); break;
    case 10: rkTable.Insert(EI_XMAX_ZMIN,EI_YMAX_ZMIN); break;
    case 12: rkTable.Insert(EI_YMIN_ZMIN,EI_YMAX_ZMIN); break;
    case 15:
    {
        // four vertices, one per edge, need to disambiguate
        int i = iX + m_iXBound*(iY + m_iYBound*iZ);
        int iF00 = m_aiData[i];  // F(x,y,z)
        i++;
        int iF10 = m_aiData[i];  // F(x+1,y,z)
        i += m_iXBound;
        int iF11 = m_aiData[i];  // F(x+1,y+1,z)
        i--;
        int iF01 = m_aiData[i];  // F(x,y+1,z)
        int iDet = iF00*iF11 - iF01*iF10;

        if ( iDet > 0 )
        {
            // disjoint hyperbolic segments, pair <P0,P2>, <P1,P3>
            rkTable.Insert(EI_XMIN_ZMIN,EI_YMIN_ZMIN);
            rkTable.Insert(EI_XMAX_ZMIN,EI_YMAX_ZMIN);
        }
        else if ( iDet < 0 )
        {
            // disjoint hyperbolic segments, pair <P0,P3>, <P1,P2>
            rkTable.Insert(EI_XMIN_ZMIN,EI_YMAX_ZMIN);
            rkTable.Insert(EI_XMAX_ZMIN,EI_YMIN_ZMIN);
        }
        else
        {
            // plus-sign configuration, add branch point to tessellation
            rkTable.Insert(FI_ZMIN,Vector3f(rkTable.GetX(EI_YMIN_ZMIN),
                rkTable.GetY(EI_XMIN_ZMIN),rkTable.GetZ(EI_XMIN_ZMIN)));

            // add edges sharing the branch point
            rkTable.Insert(EI_XMIN_ZMIN,FI_ZMIN);
            rkTable.Insert(EI_XMAX_ZMIN,FI_ZMIN);
            rkTable.Insert(EI_YMIN_ZMIN,FI_ZMIN);
            rkTable.Insert(EI_YMAX_ZMIN,FI_ZMIN);
        }
        break;
    }
    default:  assert( false );
    }
}
//----------------------------------------------------------------------------
void ExtractSurfaceCubes::GetZMaxEdges (int iX, int iY, int iZ, int iType,
    VETable& rkTable)
{
    int iFaceType = 0;
    if ( iType & EB_XMIN_ZMAX ) iFaceType |= 0x01;
    if ( iType & EB_XMAX_ZMAX ) iFaceType |= 0x02;
    if ( iType & EB_YMIN_ZMAX ) iFaceType |= 0x04;
    if ( iType & EB_YMAX_ZMAX ) iFaceType |= 0x08;

    switch ( iFaceType )
    {
    case  0: return;
    case  3: rkTable.Insert(EI_XMIN_ZMAX,EI_XMAX_ZMAX); break;
    case  5: rkTable.Insert(EI_XMIN_ZMAX,EI_YMIN_ZMAX); break;
    case  6: rkTable.Insert(EI_XMAX_ZMAX,EI_YMIN_ZMAX); break;
    case  9: rkTable.Insert(EI_XMIN_ZMAX,EI_YMAX_ZMAX); break;
    case 10: rkTable.Insert(EI_XMAX_ZMAX,EI_YMAX_ZMAX); break;
    case 12: rkTable.Insert(EI_YMIN_ZMAX,EI_YMAX_ZMAX); break;
    case 15:
    {
        // four vertices, one per edge, need to disambiguate
        int i = iX + m_iXBound*(iY + m_iYBound*(iZ+1));
        int iF00 = m_aiData[i];  // F(x,y,z)
        i++;
        int iF10 = m_aiData[i];  // F(x+1,y,z)
        i += m_iXBound;
        int iF11 = m_aiData[i];  // F(x+1,y+1,z)
        i--;
        int iF01 = m_aiData[i];  // F(x,y+1,z)
        int iDet = iF00*iF11 - iF01*iF10;

        if ( iDet > 0 )
        {
            // disjoint hyperbolic segments, pair <P0,P2>, <P1,P3>
            rkTable.Insert(EI_XMIN_ZMAX,EI_YMIN_ZMAX);
            rkTable.Insert(EI_XMAX_ZMAX,EI_YMAX_ZMAX);
        }
        else if ( iDet < 0 )
        {
            // disjoint hyperbolic segments, pair <P0,P3>, <P1,P2>
            rkTable.Insert(EI_XMIN_ZMAX,EI_YMAX_ZMAX);
            rkTable.Insert(EI_XMAX_ZMAX,EI_YMIN_ZMAX);
        }
        else
        {
            // plus-sign configuration, add branch point to tessellation
            rkTable.Insert(FI_ZMAX,Vector3f(rkTable.GetX(EI_YMIN_ZMAX),
                rkTable.GetY(EI_XMIN_ZMAX),rkTable.GetZ(EI_XMIN_ZMAX)));

            // add edges sharing the branch point
            rkTable.Insert(EI_XMIN_ZMAX,FI_ZMAX);
            rkTable.Insert(EI_XMAX_ZMAX,FI_ZMAX);
            rkTable.Insert(EI_YMIN_ZMAX,FI_ZMAX);
            rkTable.Insert(EI_YMAX_ZMAX,FI_ZMAX);
        }
        break;
    }
    default:  assert( false );
    }
}
//----------------------------------------------------------------------------
Vector3f ExtractSurfaceCubes::GetGradient (Vector3f kP)
{
    int iX = (int)kP.X();
    if ( iX < 0 || iX >= m_iXBound-1 )
        return Vector3f::ZERO;

    int iY = (int)kP.Y();
    if ( iY < 0 || iY >= m_iYBound-1 )
        return Vector3f::ZERO;

    int iZ = (int)kP.Z();
    if ( iZ < 0 || iZ >= m_iZBound-1 )
        return Vector3f::ZERO;

    // get image values at corners of voxel
    int i000 = iX + m_iXBound*(iY + m_iYBound*iZ);
    int i100 = i000 + 1;
    int i010 = i000 + m_iXBound;
    int i110 = i010 + 1;
    int i001 = i000 + m_iXYBound;
    int i101 = i001 + 1;
    int i011 = i001 + m_iXBound;
    int i111 = i011 + 1;
    float fF000 = (float)m_aiData[i000];
    float fF100 = (float)m_aiData[i100];
    float fF010 = (float)m_aiData[i010];
    float fF110 = (float)m_aiData[i110];
    float fF001 = (float)m_aiData[i001];
    float fF101 = (float)m_aiData[i101];
    float fF011 = (float)m_aiData[i011];
    float fF111 = (float)m_aiData[i111];

    kP.X() -= iX;
    kP.Y() -= iY;
    kP.Z() -= iZ;
    float fOmX = 1.0f - kP.X();
    float fOmY = 1.0f - kP.Y();
    float fOmZ = 1.0f - kP.Z();

    Vector3f kGrad;

    float fTmp0 = fOmY*(fF100-fF000) + kP.Y()*(fF110-fF010);
    float fTmp1 = fOmY*(fF101-fF001) + kP.Y()*(fF111-fF011);
    kGrad.X() = fOmZ*fTmp0 + kP.Z()*fTmp1;
    
    fTmp0 = fOmX*(fF010-fF000) + kP.X()*(fF110-fF100);
    fTmp1 = fOmX*(fF011-fF001) + kP.X()*(fF111-fF101);
    kGrad.Y() = fOmZ*fTmp0 + kP.Z()*fTmp1;
    
    fTmp0 = fOmX*(fF001-fF000) + kP.X()*(fF101-fF100);
    fTmp1 = fOmX*(fF011-fF010) + kP.X()*(fF111-fF110);
    kGrad.Z() = fOmY*fTmp0 + kP.Y()*fTmp1;

    return kGrad;
}
//----------------------------------------------------------------------------
ExtractSurfaceCubes::VETable::VETable ()
{
}
//----------------------------------------------------------------------------
void ExtractSurfaceCubes::VETable::Insert (int i, const Vector3f& rkP)
{
    assert( 0 <= i && i < 18 );
    Vertex& rkV = m_akVertex[i];
    rkV.P = rkP;
    rkV.Valid = true;
}
//----------------------------------------------------------------------------
void ExtractSurfaceCubes::VETable::Insert (int i0, int i1)
{
    assert( 0 <= i0 && i0 < 18 && 0 <= i1 && i1 < 18 );
    Vertex& rkV0 = m_akVertex[i0];
    Vertex& rkV1 = m_akVertex[i1];

    assert( rkV0.AdjQuantity < 4 && rkV1.AdjQuantity < 4 );
    rkV0.Adj[rkV0.AdjQuantity++] = i1;
    rkV1.Adj[rkV1.AdjQuantity++] = i0;
}
//----------------------------------------------------------------------------
void ExtractSurfaceCubes::VETable::RemoveVertex (int i)
{
    assert( 0 <= i && i < 18 );
    Vertex& rkV0 = m_akVertex[i];

    assert( rkV0.AdjQuantity == 2 );

    int iA0 = rkV0.Adj[0], iA1 = rkV0.Adj[1];
    Vertex& rkVA0 = m_akVertex[iA0];
    Vertex& rkVA1 = m_akVertex[iA1];

    int j;
    for (j = 0; j < rkVA0.AdjQuantity; j++)
    {
        if ( rkVA0.Adj[j] == i )
        {
            rkVA0.Adj[j] = iA1;
            break;
        }
    }
    assert( j != rkVA0.AdjQuantity );

    for (j = 0; j < rkVA1.AdjQuantity; j++)
    {
        if ( rkVA1.Adj[j] == i )
        {
            rkVA1.Adj[j] = iA0;
            break;
        }
    }
    assert( j != rkVA1.AdjQuantity );

    rkV0.Valid = false;

    if ( rkVA0.AdjQuantity == 2 )
    {
        if ( rkVA0.Adj[0] == rkVA0.Adj[1] )
            rkVA0.Valid = false;
    }

    if ( rkVA1.AdjQuantity == 2 )
    {
        if ( rkVA1.Adj[0] == rkVA1.Adj[1] )
            rkVA1.Valid = false;
    }
}
//----------------------------------------------------------------------------
bool ExtractSurfaceCubes::VETable::Remove (TriangleKey& rkTri)
{
    for (int i = 0; i < 18; i++)
    {
        Vertex& rkV = m_akVertex[i];
        if ( rkV.Valid && rkV.AdjQuantity == 2 )
        {
            rkTri.V[0] = i;
            rkTri.V[1] = rkV.Adj[0];
            rkTri.V[2] = rkV.Adj[1];
            RemoveVertex(i);
            return true;
        }
    }

    return false;
}
//----------------------------------------------------------------------------
void ExtractSurfaceCubes::VETable::RemoveTriangles (vector<Vector3f>& rkVA,
    vector<TriangleKey>& rkTA)
{
    // ear-clip the wireframe to get the triangles
    TriangleKey kTri;
    while ( Remove(kTri) )
    {
        int iV0 = (int)rkVA.size(), iV1 = iV0+1, iV2 = iV1+1;
        rkTA.push_back(TriangleKey(iV0,iV1,iV2));
        rkVA.push_back(m_akVertex[kTri.V[0]].P);
        rkVA.push_back(m_akVertex[kTri.V[1]].P);
        rkVA.push_back(m_akVertex[kTri.V[2]].P);
    }
}
//----------------------------------------------------------------------------
