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
#include "WmlTriMesh.h"
using namespace Wml;
using namespace std;

WmlImplementRTTI(TriMesh,Geometry);
WmlImplementStream(TriMesh);

//----------------------------------------------------------------------------
TriMesh::TriMesh (int iVertexQuantity, Vector3f* akVertex, Vector3f* akNormal,
    ColorRGB* akColor, Vector2f* akTexture, int iTriangleQuantity,
    int* aiConnect, Vector2f* akTexture1, Vector2f* akTexture2, 
    Vector2f* akTexture3, Vector2f* akTextureBump, 
    VertexShader* pkVertexShader, PixelShader* pkPixelShader)
    :
    Geometry(iVertexQuantity,akVertex,akNormal,akColor,akTexture,akTexture1,
        akTexture2,akTexture3,akTextureBump, pkVertexShader, pkPixelShader)
{
    m_iTriangleQuantity = iTriangleQuantity;
    m_aiConnect = aiConnect;
}
//----------------------------------------------------------------------------
TriMesh::TriMesh ()
{
    m_iTriangleQuantity = 0;
    m_aiConnect = NULL;
}
//----------------------------------------------------------------------------
TriMesh::~TriMesh ()
{
    delete[] m_aiConnect;
}
//----------------------------------------------------------------------------
void TriMesh::Reconstruct (int iVertexQuantity, int iTriangleQuantity)
{
    Geometry::Reconstruct(iVertexQuantity);

    m_iTriangleQuantity = iTriangleQuantity;
    delete[] m_aiConnect;
    if ( m_iTriangleQuantity > 0 )
        m_aiConnect = new int[3*m_iTriangleQuantity];
}
//----------------------------------------------------------------------------
void TriMesh::Reconstruct (int iVertexQuantity, Vector3f* akVertex,
    Vector3f* akNormal, ColorRGB* akColor, Vector2f* akTexture,
    int iTriangleQuantity, int* aiConnect, Vector2f* akTexture1, 
    Vector2f* akTexture2, Vector2f* akTexture3, Vector2f* akTextureBump)
{
    Geometry::Reconstruct(iVertexQuantity,akVertex,akNormal,akColor,
        akTexture,akTexture1,akTexture2,akTexture3,akTextureBump);

    if ( m_aiConnect != aiConnect )
    {
        delete[] m_aiConnect;
        m_aiConnect = aiConnect;
    }

    m_iTriangleQuantity = iTriangleQuantity;
}
//----------------------------------------------------------------------------
void TriMesh::GetTriangle (int i, int& riV0, int& riV1, int& riV2) const
{
    assert( i < m_iTriangleQuantity );

    int iBase = 3*i;
    riV0 = m_aiConnect[iBase++];
    riV1 = m_aiConnect[iBase++];
    riV2 = m_aiConnect[iBase];
}
//----------------------------------------------------------------------------
void TriMesh::GetTriangle (int i, Vector3f& rkV0, Vector3f& rkV1,
    Vector3f& rkV2) const
{
    assert( i < m_iTriangleQuantity );

    int iBase = 3*i;
    rkV0 = m_akVertex[m_aiConnect[iBase++]];
    rkV1 = m_akVertex[m_aiConnect[iBase++]];
    rkV2 = m_akVertex[m_aiConnect[iBase]];
}
//----------------------------------------------------------------------------
void TriMesh::UpdateModelNormals ()
{
    // Calculate normals from vertices by weighted averages of facet planes
    // that contain the vertices.  TO DO.  Replace by algorithm that computes
    // axis of minimum cone containing the normals.

    if ( !m_akNormal )
        m_akNormal = new Vector3f[m_iVertexQuantity];

    memset(m_akNormal,0,m_iVertexQuantity*sizeof(Vector3f));

    int* aiConnect = m_aiConnect;
    int i;
    for (i = 0; i < m_iTriangleQuantity; i++)
    {
        // get vertex indices
        int iV0 = *aiConnect++;
        int iV1 = *aiConnect++;
        int iV2 = *aiConnect++;

        // get vertices
        Vector3f& rkV0 = m_akVertex[iV0];
        Vector3f& rkV1 = m_akVertex[iV1];
        Vector3f& rkV2 = m_akVertex[iV2];

        // compute the normal (length provides the weighted sum)
        Vector3f kEdge1 = rkV1 - rkV0;
        Vector3f kEdge2 = rkV2 - rkV0;
        Vector3f kNormal = kEdge1.Cross(kEdge2);

        m_akNormal[iV0] += kNormal;
        m_akNormal[iV1] += kNormal;
        m_akNormal[iV2] += kNormal;
    }

    for (i = 0; i < m_iVertexQuantity; i++)
        m_akNormal[i].Normalize();
}
//----------------------------------------------------------------------------
void TriMesh::Draw (Renderer& rkRenderer)
{
    Geometry::Draw(rkRenderer);
    rkRenderer.Draw(*this);
}
//----------------------------------------------------------------------------
TriMesh::PickRecord::PickRecord (TriMesh* pkObject, float fRayT,
    int iTriangle, float fBary0, float fBary1, float fBary2)
    :
    Geometry::PickRecord(pkObject,fRayT)
{
    m_iTriangle = iTriangle;
    m_fBary0 = fBary0;
    m_fBary1 = fBary1;
    m_fBary2 = fBary2;
}
//----------------------------------------------------------------------------
void TriMesh::DoPick (const Vector3f& rkOrigin, const Vector3f& rkDirection,
    PickArray& rkResults)
{
    if ( m_kWorldBound.TestIntersection(rkOrigin,rkDirection) )
    {
        // convert the ray to model-space coordinates
        Vector3f kDiff = rkOrigin - m_kWorldTranslate;
        float fInvScale = 1.0f/m_fWorldScale;
        Vector3f kMOrigin = fInvScale*(kDiff*m_kWorldRotate);
        Vector3f kMDirection = fInvScale*(rkDirection*m_kWorldRotate);

        // compute intersections with the model-space triangles
        int* aiConnect = m_aiConnect;
        for (int i = 0; i < m_iTriangleQuantity; i++)
        {
            int iV0 = *aiConnect++;
            int iV1 = *aiConnect++;
            int iV2 = *aiConnect++;

            float fBary0, fBary1, fBary2, fRayT;

            if ( GetRayTriangleIntersection(kMOrigin,kMDirection,
                 m_akVertex[iV0],m_akVertex[iV1],m_akVertex[iV2],fBary0,
                 fBary1,fBary2,fRayT) )
            {
                rkResults.push_back(new PickRecord(this,fRayT,i,fBary0,fBary1,
                    fBary2));
            }
        }
    }
}
//----------------------------------------------------------------------------
bool TriMesh::GetRayTriangleIntersection (const Vector3f& rkModelOrigin,
    const Vector3f& rkModelDirection, const Vector3f& rkV0,
    const Vector3f& rkV1, const Vector3f& rkV2, float& rfBary0,
    float& rfBary1, float& rfBary2, float& rfRayT)
{
    // compute the offset origin, edges, and normal
    Vector3f kDiff = rkModelOrigin - rkV0;
    Vector3f kEdge1 = rkV1 - rkV0;
    Vector3f kEdge2 = rkV2 - rkV0;
    Vector3f kNormal = kEdge1.Cross(kEdge2);

    // Solve Q + t*D = s1*E1 + s2*E2 (Q = kDiff, D = kMDirection,
    // E1 = kEdge1, E2 = kEdge2, N = Cross(E1,E2)) by
    //   |Dot(D,N)|*s1 = sign(Dot(D,N))*Dot(D,Cross(Q,E2))
    //   |Dot(D,N)|*s2 = sign(Dot(D,N))*Dot(D,Cross(E1,Q))
    //   |Dot(D,N)|*t = -sign(Dot(D,N))*Dot(Q,N)
    const float fEpsilon = 1e-06f;
    float fDdN = rkModelDirection.Dot(kNormal);
    float fSign;
    if ( fDdN > fEpsilon )
    {
        fSign = 1.0f;
    }
    else if ( fDdN < -fEpsilon )
    {
        fSign = -1.0f;
        fDdN = -fDdN;
    }
    else
    {
        // Ray and triangle are parallel, call it a "no intersection"
        // even if the ray does intersect.
        return false;
    }

    float fDdQxE2 = fSign*rkModelDirection.Dot(kDiff.Cross(kEdge2));
    if ( fDdQxE2 >= 0.0f )
    {
        float fDdE1xQ = fSign*rkModelDirection.Dot(kEdge1.Cross(kDiff));
        if ( fDdE1xQ >= 0.0f )
        {
            if ( fDdQxE2 + fDdE1xQ <= fDdN )
            {
                // line intersects triangle, check if ray does
                float fQdN = -fSign*kDiff.Dot(kNormal);
                if ( fQdN >= 0.0f )
                {
                    // ray intersects triangle
                    float fInv = 1.0f/fDdN;
                    rfBary1 = fDdQxE2*fInv;
                    rfBary2 = fDdE1xQ*fInv;
                    rfBary0 = 1.0f - rfBary1 - rfBary2;
                    rfRayT = fQdN*fInv;
                    return true;
                }
                // else: t < 0, no intersection
            }
            // else: s1+s2 > 1, no intersection
        }
        // else: s2 < 0, no intersection
    }
    // else: s1 < 0, no intersection

    return false;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* TriMesh::Factory (Stream& rkStream)
{
    TriMesh* pkObject = new TriMesh;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void TriMesh::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Geometry::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_iTriangleQuantity);
    int iQuantity = 3*m_iTriangleQuantity;
    m_aiConnect = new int[iQuantity];
    StreamRead(rkStream,m_aiConnect,iQuantity);
}
//----------------------------------------------------------------------------
void TriMesh::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Geometry::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool TriMesh::Register (Stream& rkStream)
{
    return Geometry::Register(rkStream);
}
//----------------------------------------------------------------------------
void TriMesh::Save (Stream& rkStream)
{
    Geometry::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_iTriangleQuantity);
    StreamWrite(rkStream,m_aiConnect,3*m_iTriangleQuantity);
}
//----------------------------------------------------------------------------
StringTree* TriMesh::SaveStrings ()
{
    StringTree* pkTree = new StringTree(2,0,2,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("triangle quantity =",
        m_iTriangleQuantity));

    // children
    pkTree->SetChild(0,Geometry::SaveStrings());

    StringTree* pkTriTree = new StringTree(m_iTriangleQuantity+1,0,0,0);
    pkTriTree->SetString(0,MakeString("triangles"));
    int* piConnect = m_aiConnect;
    char acDummy[64];
    for (int i = 0; i < m_iTriangleQuantity; i++)
    {
        int iI0 = *piConnect++;
        int iI1 = *piConnect++;
        int iI2 = *piConnect++;
        sprintf(acDummy,"<%5d,%5d,%5d>",iI0,iI1,iI2);
        pkTriTree->SetString(i+1,MakeString(acDummy));
    }

    pkTree->SetChild(1,pkTriTree);

    return pkTree;
}
//----------------------------------------------------------------------------
int TriMesh::GetMemoryUsed () const
{
    int iBaseSize = sizeof(TriMesh) - sizeof(Geometry);
    int iDynaSize = 3*m_iTriangleQuantity*sizeof(int);
    int iTotalSize = iBaseSize + iDynaSize + Geometry::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int TriMesh::GetDiskUsed () const
{
    return Geometry::GetDiskUsed() +
        sizeof(m_iTriangleQuantity) +
        3*m_iTriangleQuantity*sizeof(m_aiConnect[0]);
}
//----------------------------------------------------------------------------
