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
#include "WmlPlanarShadow.h"
using namespace Wml;

WmlImplementRTTI(PlanarShadow,Node);
WmlImplementStream(PlanarShadow);

int PlanarShadow::ms_iNextFreeStencilValue = 1;

//----------------------------------------------------------------------------
PlanarShadow::PlanarShadow (Node* pkCaster, TriMesh* pkPlane, Light* pkLight)
    :
    m_spkCaster(pkCaster),
    m_spkPlane(pkPlane),
    m_spkLight(pkLight),
    m_iStencilValue(ms_iNextFreeStencilValue++)
{
    assert( pkPlane && pkLight && pkPlane->GetTriangleQuantity() > 0 );

    // NOTE.  The light is used for projected drawing purposes, not for
    // lighting the caster or plane.

    // Save the cull state of the casters and plane.  These objects will be
    // force-culled by 'this' to make sure they are not drawn by the regular
    // traversal.
    m_bForceCullCaster = pkCaster->ForceCull();
    m_bForceCullPlane = pkPlane->ForceCull();
    pkCaster->ForceCull() = true;
    pkPlane->ForceCull() = true;

    // Get a point on the plane and compute the plane normal by using the
    // first triangle in the mesh of the plane.  At render time, these need
    // to be transformed to world coordinates.
    Vector3f kV1, kV2;
    pkPlane->GetTriangle(0,m_kPointOnPlane,kV1,kV2);
    Vector3f kEdge1 = kV1 - m_kPointOnPlane;
    Vector3f kEdge2 = kV2 - m_kPointOnPlane;
    m_kNormal = kEdge1.UnitCross(kEdge2);
}
//----------------------------------------------------------------------------
PlanarShadow::PlanarShadow ()
    :
    m_kNormal(0.0f,0.0f,0.0f),
    m_kPointOnPlane(0.0f,0.0f,0.0f)
{
    m_bForceCullCaster = false;
    m_bForceCullPlane = false;
    m_iStencilValue = 0;
}
//----------------------------------------------------------------------------
PlanarShadow::~PlanarShadow ()
{
    // restore the cull state
    m_spkCaster->ForceCull() = m_bForceCullCaster;
    m_spkPlane->ForceCull() = m_bForceCullPlane;

    // release the render effect objects
    m_spkCaster = NULL;
    m_spkPlane = NULL;
    m_spkLight = NULL;
}
//----------------------------------------------------------------------------
void PlanarShadow::UpdateWorldBound ()
{
    m_kWorldBound = m_spkCaster->WorldBound();
    m_kWorldBound += m_spkPlane->WorldBound();
}
//----------------------------------------------------------------------------
void PlanarShadow::Draw (Renderer& rkRenderer)
{
    // Restore the original cull state in case the user is trying to do
    // something clever, like render an object's shadow, but not the object
    // itself.
    m_spkCaster->ForceCull() = m_bForceCullCaster;
    m_spkPlane->ForceCull() = m_bForceCullPlane;

    if ( !m_spkCaster )
    {
        // no shadow caster, just cull/draw the plane
        m_spkPlane->OnDraw(rkRenderer);
        m_spkPlane->ForceCull() = true;
        return;
    }

    // TO DO. We could add a test here for the caster not being culled in a
    // test with the light as a camera object, and the planes defined by the
    // extents of the plane.  This would avoid unnecessary shadow computations
    // and reverse shadow projection.

    if ( m_spkLight->GetType() == Light::LT_AMBIENT )
    {
        // for ambient lights, just cull and draw the objects as usual
        m_spkCaster->OnDraw(rkRenderer);
        m_spkPlane->OnDraw(rkRenderer);
        return;
    }

    rkRenderer.Draw(*this);

    // prevent drawing of the caster and plane during the regular traversal
    m_spkCaster->ForceCull() = true;
    m_spkPlane->ForceCull() = true;
}
//----------------------------------------------------------------------------
Object* PlanarShadow::GetObjectByName (const char* acName)
{
    Object* pkFound = Node::GetObjectByName(acName);
    if ( pkFound )
        return pkFound;

    // The subtree m_spkCaster is not searched to avoid the possibility of
    // infinite recursion.

    if ( m_spkPlane )
    {
        pkFound = m_spkPlane->GetObjectByName(acName);
        if ( pkFound )
            return pkFound;
    }

    if ( m_spkLight )
    {
        pkFound = m_spkLight->GetObjectByName(acName);
        if ( pkFound )
            return pkFound;
    }

    return NULL;
}
//----------------------------------------------------------------------------
void PlanarShadow::GetAllObjectsByName (const char* acName,
    std::vector<Object*>& rkObjects)
{
    Node::GetAllObjectsByName(acName,rkObjects);

    // The subtree m_spkCaster is not searched to avoid the possibility of
    // infinite recursion.

    if ( m_spkPlane )
        m_spkPlane->GetAllObjectsByName(acName,rkObjects);

    if ( m_spkLight )
        m_spkLight->GetAllObjectsByName(acName,rkObjects);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* PlanarShadow::Factory (Stream& rkStream)
{
    PlanarShadow* pkObject = new PlanarShadow;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void PlanarShadow::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Node::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_kNormal);
    StreamRead(rkStream,m_kPointOnPlane);
    StreamRead(rkStream,m_iStencilValue);

    // link data (shadow caster, plane, light, in that order)
    Spatial* pkChild;
    StreamRead(rkStream,pkChild);
    pkLink->Add(pkChild);
    StreamRead(rkStream,pkChild);
    pkLink->Add(pkChild);
    StreamRead(rkStream,pkChild);
    pkLink->Add(pkChild);
}
//----------------------------------------------------------------------------
void PlanarShadow::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Node::Link(rkStream,pkLink);

    Object* pkLinkID = pkLink->GetLinkID();
    m_spkCaster = (Node*)rkStream.GetFromMap(pkLinkID);

    pkLinkID = pkLink->GetLinkID();
    m_spkPlane = (TriMesh*)rkStream.GetFromMap(pkLinkID);

    pkLinkID = pkLink->GetLinkID();
    m_spkLight = (Light*)rkStream.GetFromMap(pkLinkID);

    // initialize the cull status for correct toggling during rendering
    m_bForceCullCaster = m_spkCaster->ForceCull();
    m_bForceCullPlane = m_spkPlane->ForceCull();
    m_spkCaster->ForceCull() = true;
    m_spkPlane->ForceCull() = true;
}
//----------------------------------------------------------------------------
bool PlanarShadow::Register (Stream& rkStream)
{
    if ( !Node::Register(rkStream) )
        return false;

    if ( m_spkCaster )
        m_spkCaster->Register(rkStream);

    if ( m_spkPlane )
        m_spkPlane->Register(rkStream);

    if ( m_spkLight )
        m_spkLight->Register(rkStream);

    return true;
}
//----------------------------------------------------------------------------
void PlanarShadow::Save (Stream& rkStream)
{
    Node::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_kNormal);
    StreamWrite(rkStream,m_kPointOnPlane);
    StreamWrite(rkStream,m_iStencilValue);

    // link data
    StreamWrite(rkStream,m_spkCaster);
    StreamWrite(rkStream,m_spkPlane);
    StreamWrite(rkStream,m_spkLight);
}
//----------------------------------------------------------------------------
StringTree* PlanarShadow::SaveStrings ()
{
    int iCQuantity = 1;
    if ( m_spkCaster )
        iCQuantity++;
    if ( m_spkPlane )
        iCQuantity++;
    if ( m_spkLight )
        iCQuantity++;

    StringTree* pkTree = new StringTree(4,0,iCQuantity,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("normal =",m_kNormal));
    pkTree->SetString(2,MakeString("point on plane =",m_kPointOnPlane));
    pkTree->SetString(3,MakeString("stencil value =",m_iStencilValue));

    // children
    pkTree->SetChild(0,Node::SaveStrings());
    int iSlot = 1;
    if ( m_spkCaster )
        pkTree->SetChild(iSlot++,m_spkCaster->SaveStrings());
    if ( m_spkPlane )
        pkTree->SetChild(iSlot++,m_spkPlane->SaveStrings());
    if ( m_spkLight )
        pkTree->SetChild(iSlot++,m_spkLight->SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int PlanarShadow::GetMemoryUsed () const
{
    int iBaseSize = sizeof(PlanarShadow) - sizeof(Node);
    int iTotalSize = iBaseSize + Node::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int PlanarShadow::GetDiskUsed () const
{
    return Node::GetDiskUsed() +
        sizeof(m_kNormal) +
        sizeof(m_kPointOnPlane) +
        sizeof(m_iStencilValue) +
        sizeof(m_spkCaster) +
        sizeof(m_spkPlane) +
        sizeof(m_spkLight);
}
//----------------------------------------------------------------------------
