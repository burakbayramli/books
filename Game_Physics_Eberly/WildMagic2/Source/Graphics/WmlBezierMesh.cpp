// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBezierCylinder.h"
#include "WmlBezierMesh.h"
#include "WmlTriMesh.h"
using namespace Wml;

WmlImplementRTTI(BezierMesh,Geometry);
WmlImplementStream(BezierMesh);

//----------------------------------------------------------------------------
BezierMesh::BezierMesh (int iCtrlQuantity, Vector3f* akCtrlVertex,
    bool bUseNormals, ColorRGB* akCtrlColor, Vector2f* akCtrlTexture,
    int iPatchQuantity, BezierPatchPtr* aspkPatch)
    :
    Geometry(iCtrlQuantity,akCtrlVertex,NULL,akCtrlColor,akCtrlTexture)
{
    m_iPatchQuantity = iPatchQuantity;
    m_aspkPatch = aspkPatch;

    // allocate space for trimesh (default level = 0)
    int iVertexQuantity, iTriangleQuantity;
    GetMeshQuantities(0,iVertexQuantity,iTriangleQuantity);

    Vector3f* akVertex = new Vector3f[iVertexQuantity];

    Vector3f* akNormal;
    if ( bUseNormals )
        akNormal = new Vector3f[iVertexQuantity];
    else
        akNormal = NULL;

    ColorRGB* akColor;
    if ( akCtrlColor )
        akColor = new ColorRGB[iVertexQuantity];
    else
        akColor = NULL;

    Vector2f* akTexture;
    if ( akCtrlTexture )
        akTexture = new Vector2f[iVertexQuantity];
    else
        akTexture = NULL;

    int* aiConnect = new int[3*iTriangleQuantity];

    m_spkMesh = new TriMesh(iVertexQuantity,akVertex,akNormal,akColor,
        akTexture,iTriangleQuantity,aiConnect);

    Tessellate(0);
}
//----------------------------------------------------------------------------
BezierMesh::BezierMesh ()
{
    m_iPatchQuantity = 0;
    m_aspkPatch = NULL;
    m_iLevel = 0;
}
//----------------------------------------------------------------------------
BezierMesh::~BezierMesh ()
{
    if ( m_aspkPatch )
    {
        for (int i = 0; i < m_iPatchQuantity; i++)
            m_aspkPatch[i] = NULL;
        delete[] m_aspkPatch;
    }
}
//----------------------------------------------------------------------------
void BezierMesh::UpdateWorldData (float fAppTime)
{
    Geometry::UpdateWorldData(fAppTime);

    // transfer state to managed mesh
    m_spkMesh->m_kWorldTranslate = m_kWorldTranslate;
    m_spkMesh->m_kWorldRotate = m_kWorldRotate;
    m_spkMesh->m_fWorldScale = m_fWorldScale;
}
//----------------------------------------------------------------------------
void BezierMesh::UpdateWorldBound ()
{
    Geometry::UpdateWorldBound();

    // transfer state to managed mesh
    m_spkMesh->m_kWorldBound = m_kWorldBound;
}
//----------------------------------------------------------------------------
void BezierMesh::UpdateModelNormals ()
{
    // TO DO.  Implement?  Control point normals don't make sense here.
}
//----------------------------------------------------------------------------
void BezierMesh::UpdateRenderState (RenderState::Stack* pkStack)
{
    // Geometry base never gets drawn, it is just a repository for control
    // points.  Transfer the render state to the trimesh that is drawn.
    m_spkMesh->UpdateRenderState(pkStack);
}
//----------------------------------------------------------------------------
void BezierMesh::Draw (Renderer& rkRenderer)
{
    // Geometry base never gets drawn, it is just a repository for control
    // points.  Draw the trimesh instead.
    m_spkMesh->Draw(rkRenderer);
}
//----------------------------------------------------------------------------
void BezierMesh::GetMeshQuantities (int iLevel, int& riVertexQuantity,
    int& riTriangleQuantity)
{
    // compute number of vertices and triangles needed for trimesh
    riVertexQuantity = 0;
    riTriangleQuantity = 0;
    for (int i = 0; i < m_iPatchQuantity; i++)
    {
        riVertexQuantity += m_aspkPatch[i]->GetVerticesPerPatch(iLevel);
        riTriangleQuantity += m_aspkPatch[i]->GetTrianglesPerPatch(iLevel);
    }
}
//----------------------------------------------------------------------------
void BezierMesh::Tessellate (int iLevel)
{
    m_iLevel = iLevel;

    // reallocate space for trimesh
    int iVertexQuantity, iTriangleQuantity;
    GetMeshQuantities(iLevel,iVertexQuantity,iTriangleQuantity);
    m_spkMesh->Reconstruct(iVertexQuantity,iTriangleQuantity);

    // fill in the trimesh with tessellated patch triangles
    iVertexQuantity = 0;
    iTriangleQuantity = 0;
    for (int i = 0; i < m_iPatchQuantity; i++)
    {
        m_aspkPatch[i]->Tessellate(iLevel,Vertices(),Colors(),Textures(),
            m_spkMesh,iVertexQuantity,iTriangleQuantity);
    }
}
//----------------------------------------------------------------------------
Object* BezierMesh::GetObjectByName (const char* acName)
{
    Object* pkFound = Geometry::GetObjectByName(acName);
    if ( pkFound )
        return pkFound;

    for (int i = 0; i < m_iPatchQuantity; i++)
    {
        pkFound = m_aspkPatch[i]->GetObjectByName(acName);
        if ( pkFound )
            return pkFound;
    }

    pkFound = m_spkMesh->GetObjectByName(acName);
    if ( pkFound )
        return pkFound;

    return NULL;
}
//----------------------------------------------------------------------------
void BezierMesh::GetAllObjectsByName (const char* acName,
    std::vector<Object*>& rkObjects)
{
    Geometry::GetAllObjectsByName(acName,rkObjects);

    for (int i = 0; i < m_iPatchQuantity; i++)
        m_aspkPatch[i]->GetAllObjectsByName(acName,rkObjects);

    m_spkMesh->GetAllObjectsByName(acName,rkObjects);
}
//----------------------------------------------------------------------------
BezierMesh::PickRecord::PickRecord (BezierMesh* pkObject, float fRayT,
    int iPatch, int iTriangle, float fBary0, float fBary1, float fBary2)
    :
    Geometry::PickRecord(pkObject,fRayT)
{
    m_iPatch = iPatch;
    m_iTriangle = iTriangle;
    m_fBary0 = fBary0;
    m_fBary1 = fBary1;
    m_fBary2 = fBary2;
}
//----------------------------------------------------------------------------
void BezierMesh::DoPick (const Vector3f& rkOrigin,
    const Vector3f& rkDirection, PickArray& rkResults)
{
    if ( m_kWorldBound.TestIntersection(rkOrigin,rkDirection) )
    {
        // convert the ray to model-space coordinates
        Vector3f kDiff = rkOrigin - m_kWorldTranslate;
        float fInvScale = 1.0f/m_fWorldScale;
        Vector3f kMOrigin = fInvScale*(kDiff*m_kWorldRotate);
        Vector3f kMDirection = fInvScale*(rkDirection*m_kWorldRotate);

        // compute intersections with the model-space triangles
        const int* aiConnect = m_spkMesh->Connectivity();
        const Vector3f* akVertex = m_spkMesh->Vertices();
        for (int iPatch = 0; iPatch < m_iPatchQuantity; iPatch++)
        {
            int iTQuantity = m_aspkPatch[iPatch]->GetTrianglesPerPatch(
                m_iLevel);
            for (int iTriangle = 0; iTriangle < iTQuantity; iTriangle++)
            {
                int iV0 = *aiConnect++;
                int iV1 = *aiConnect++;
                int iV2 = *aiConnect++;

                float fBary0, fBary1, fBary2, fRayT;

                if ( TriMesh::GetRayTriangleIntersection(kMOrigin,kMDirection,
                    akVertex[iV0],akVertex[iV1],akVertex[iV2],fBary0,fBary1,
                    fBary2,fRayT) )
                {
                    rkResults.push_back(new PickRecord(this,fRayT,iPatch,
                        iTriangle,fBary0,fBary1,fBary2));
                }
            }
        }
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* BezierMesh::Factory (Stream& rkStream)
{
    BezierMesh* pkObject = new BezierMesh;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void BezierMesh::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Geometry::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_iPatchQuantity);

    if ( rkStream.GetVersion() <= Version(1,1) )
    {
        // TO DO.  m_iTrianglePatchQuantity, m_iRectanglePatchQuantity, and
        // m_iCylinderPatchQuantity are now obsolete.  Just load the old
        // values and discard.
        int iDummy;
        StreamRead(rkStream,iDummy);  // m_iTrianglePatchQuantity
        StreamRead(rkStream,iDummy);  // m_iRectanglePatchQuantity
        StreamRead(rkStream,iDummy);  // m_iCylinderPatchQuantity
    }

    StreamRead(rkStream,m_iLevel);

    // link data
    for (int i = 0; i < m_iPatchQuantity; i++)
    {
        BezierPatch* pkPatch;
        StreamRead(rkStream,pkPatch);
        pkLink->Add(pkPatch);
    }

    TriMesh* pkMesh;
    StreamRead(rkStream,pkMesh);
    pkLink->Add(pkMesh);
}
//----------------------------------------------------------------------------
void BezierMesh::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Geometry::Link(rkStream,pkLink);

    m_aspkPatch = new BezierPatchPtr[m_iPatchQuantity];

    Object* pkLinkID;
    for (int i = 0; i < m_iPatchQuantity; i++)
    {
        pkLinkID = pkLink->GetLinkID();
        m_aspkPatch[i] = (BezierPatch*) rkStream.GetFromMap(pkLinkID);
    }

    pkLinkID = pkLink->GetLinkID();
    m_spkMesh = (TriMesh*) rkStream.GetFromMap(pkLinkID);
}
//----------------------------------------------------------------------------
bool BezierMesh::Register (Stream& rkStream)
{
    if ( !Geometry::Register(rkStream) )
        return false;

    for (int i = 0; i < m_iPatchQuantity; i++)
        m_aspkPatch[i]->Register(rkStream);

    m_spkMesh->Register(rkStream);

    return true;
}
//----------------------------------------------------------------------------
void BezierMesh::Save (Stream& rkStream)
{
    Geometry::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_iPatchQuantity);
    StreamWrite(rkStream,m_iLevel);

    // link data
    for (int i = 0; i < m_iPatchQuantity; i++)
        StreamWrite(rkStream,m_aspkPatch[i]);

    StreamWrite(rkStream,m_spkMesh);
}
//----------------------------------------------------------------------------
StringTree* BezierMesh::SaveStrings ()
{
    StringTree* pkTree = new StringTree(3,0,3,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("patch quantity =",m_iPatchQuantity));
    pkTree->SetString(2,MakeString("tessellation level =",m_iLevel));

    // children
    pkTree->SetChild(0,Geometry::SaveStrings());
    pkTree->SetChild(1,m_spkMesh->SaveStrings());

    StringTree* pkPatchTree = new StringTree(1,0,m_iPatchQuantity,0);
    pkPatchTree->SetString(0,MakeString("patches"));
    for (int i = 0; i < m_iPatchQuantity; i++)
        pkPatchTree->SetChild(i,m_aspkPatch[i]->SaveStrings());

    pkTree->SetChild(2,pkPatchTree);

    return pkTree;
}
//----------------------------------------------------------------------------
int BezierMesh::GetMemoryUsed () const
{
    int iBaseSize = sizeof(BezierMesh) - sizeof(Geometry);
    int iDynaSize = m_iPatchQuantity*sizeof(m_aspkPatch[0]);
    int iTotalSize = iBaseSize + iDynaSize + Geometry::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int BezierMesh::GetDiskUsed () const
{
    return Geometry::GetDiskUsed() +
        sizeof(m_iPatchQuantity) +
        sizeof(m_iLevel) +
        m_iPatchQuantity*sizeof(m_aspkPatch[0]) +
        sizeof(m_spkMesh);
}
//----------------------------------------------------------------------------
