// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlNode.h"
#include "WmlRenderer.h"
#include "WmlSpatial.h"
#include <algorithm>
using namespace Wml;

WmlImplementRTTI(Spatial,Object);
WmlImplementStream(Spatial);

//----------------------------------------------------------------------------
Spatial::Spatial ()
    :
    m_kRotate(Matrix3f::IDENTITY),
    m_kTranslate(Vector3f::ZERO),
    m_kWorldRotate(Matrix3f::IDENTITY),
    m_kWorldTranslate(Vector3f::ZERO)
{
    m_pkParent = NULL;
    m_fScale = 1.0f;
    m_fWorldScale = 1.0f;
    m_bForceCull = false;
    m_pkStateList = NULL;
}
//----------------------------------------------------------------------------
Spatial::~Spatial ()
{
    RemoveAllStates();
}
//----------------------------------------------------------------------------
RenderStatePtr Spatial::SetRenderState (RenderState* pkState)
{
    assert( pkState );

    // check if type of state already exists
    RenderState::List* pkList;
    for (pkList = m_pkStateList; pkList; pkList = pkList->m_pkNext)
    {
        if ( pkList->m_spkState->GetType() == pkState->GetType() )
        {
            // type of state exists, replace it
            RenderStatePtr spkSave = pkList->m_spkState;
            pkList->m_spkState = pkState;
            return spkSave;
        }
    }

    // type of state not in current list, add state
    pkList = new RenderState::List;
    pkList->m_spkState = pkState;
    pkList->m_pkNext = m_pkStateList;
    m_pkStateList = pkList;
    return NULL;
}
//----------------------------------------------------------------------------
RenderStatePtr Spatial::GetRenderState (RenderState::Type eType)
{
    // check if type of state already exists
    RenderState::List* pkList;
    for (pkList = m_pkStateList; pkList; pkList = pkList->m_pkNext)
    {
        if ( pkList->m_spkState->GetType() == eType )
        {
            // type of state exists, return it
            return pkList->m_spkState;
        }
    }

    return NULL;
}
//----------------------------------------------------------------------------
RenderStatePtr Spatial::RemoveRenderState (RenderState::Type eType)
{
    // check if type of state already exists
    RenderState::List* pkList = m_pkStateList;
    RenderState::List* pkPrev = NULL;
    for (/**/; pkList; pkPrev = pkList, pkList = pkList->m_pkNext)
    {
        if ( pkList->m_spkState->GetType() == eType )
        {
            // type of state exists, remove it
            RenderStatePtr spkSave = pkList->m_spkState;

            if ( pkPrev )
            {
                // render state not at front of list
                pkPrev->m_pkNext = pkList->m_pkNext;
            }
            else
            {
                // render state at front of list
                assert( pkList == m_pkStateList );
                m_pkStateList = pkList->m_pkNext;
            }
            pkList->m_pkNext = NULL;
            delete pkList;

            return spkSave;
        }
    }

    // type of state not in current list
    return NULL;
}
//----------------------------------------------------------------------------
void Spatial::RemoveAllStates ()
{
    while ( m_pkStateList )
    {
        m_pkStateList->m_spkState = NULL;
        RenderState::List* pkSave = m_pkStateList->m_pkNext;
        delete m_pkStateList;
        m_pkStateList = pkSave;
    }
}
//----------------------------------------------------------------------------
void Spatial::UpdateWorldData (float fAppTime)
{
    // update render state controllers
    RenderState::List* pkSList;
    Controller* pkControl;
    for (pkSList = m_pkStateList; pkSList; pkSList = pkSList->m_pkNext)
    {
        RenderState* pkState = pkSList->m_spkState;
        pkControl = pkState->GetControllers();
        while ( pkControl )
        {
            pkControl->Update(fAppTime);
            pkControl = pkControl->GetNext();
        }
    }

    // update spatial controllers
    int iComputesWorldTransform = 0;
    pkControl = GetControllers();
    while ( pkControl )
    {
        if ( pkControl->Update(fAppTime) )
            iComputesWorldTransform++;
        pkControl = pkControl->GetNext();
    }

    // If two controllers set the world transforms, there is most likely a
    // problem (one controller is unaware that the other is also changing
    // transforms).
    assert( iComputesWorldTransform <= 1 );

    // update world transforms
    if ( iComputesWorldTransform == 0 )
    {
        if ( m_pkParent )
        {
            m_fWorldScale = m_pkParent->m_fWorldScale*m_fScale;
            m_kWorldRotate = m_pkParent->m_kWorldRotate*m_kRotate;
            m_kWorldTranslate = m_pkParent->m_kWorldTranslate +
                m_pkParent->m_fWorldScale*(m_pkParent->m_kWorldRotate *
                m_kTranslate);
        }
        else
        {
            m_fWorldScale = m_fScale;
            m_kWorldRotate = m_kRotate;
            m_kWorldTranslate = m_kTranslate;
        }
    }
}
//----------------------------------------------------------------------------
void Spatial::PropagateBoundToRoot ()
{
    if ( m_pkParent )
    {
        m_pkParent->UpdateWorldBound();
        m_pkParent->PropagateBoundToRoot();
    }
}
//----------------------------------------------------------------------------
void Spatial::UpdateGS (float fAppTime, bool bInitiator)
{
    UpdateWorldData(fAppTime);
    UpdateWorldBound();
    if ( bInitiator )
        PropagateBoundToRoot();
}
//----------------------------------------------------------------------------
void Spatial::PropagateStateFromRoot (RenderState::Stack* pkStack)
{
    // traverse to root to allow downward state propagation
    if ( m_pkParent )
        m_pkParent->PropagateStateFromRoot(pkStack);

    // push states onto current render state stack
    RenderState::List* pkList;
    for (pkList = m_pkStateList; pkList; pkList = pkList->m_pkNext)
        pkStack->Push(pkList->m_spkState);
}
//----------------------------------------------------------------------------
void Spatial::RestoreStateToRoot (RenderState::Stack* pkStack)
{
    // pop states from current render state stack
    RenderState::List* pkList;
    for (pkList = m_pkStateList; pkList; pkList = pkList->m_pkNext)
        pkStack->Pop(pkList->m_spkState);

    // traverse to root
    if ( m_pkParent )
        m_pkParent->RestoreStateToRoot(pkStack);
}
//----------------------------------------------------------------------------
void Spatial::UpdateRS (RenderState::Stack* pkStack)
{
    bool bInitiator = (pkStack == NULL);
    RenderState::List* pkList;

    // update previous state by current state
    if ( bInitiator )
    {
        // traverse to root and push states from root to this node
        pkStack = new RenderState::Stack;
        PropagateStateFromRoot(pkStack);
    }
    else
    {
        // push states at this node
        for (pkList = m_pkStateList; pkList; pkList = pkList->m_pkNext)
            pkStack->Push(pkList->m_spkState);
    }

    // propagate the new state to the subtree rooted here
    UpdateRenderState(pkStack);

    // restore previous state
    if ( bInitiator )
    {
        // traverse to root and pop states along the way
        RestoreStateToRoot(pkStack);
        delete pkStack;
    }
    else
    {
        // pop states at this node
        for (pkList = m_pkStateList; pkList; pkList = pkList->m_pkNext)
            pkStack->Pop(pkList->m_spkState);
    }
}
//----------------------------------------------------------------------------
void Spatial::OnDraw (Renderer& rkRenderer)
{
    if ( m_bForceCull )
        return;

    CameraPtr spCamera = rkRenderer.GetCamera();
    unsigned int uiState = spCamera->GetPlaneState();

    if ( !spCamera->Culled(m_kWorldBound) )
        Draw(rkRenderer);

    spCamera->SetPlaneState(uiState);
}
//----------------------------------------------------------------------------
Object* Spatial::GetObjectByName (const char* acName)
{
    Object* pkFound = Object::GetObjectByName(acName);
    if ( pkFound )
        return pkFound;

    RenderState::List* pkList;
    for (pkList = m_pkStateList; pkList; pkList = pkList->m_pkNext)
    {
        pkFound = pkList->m_spkState->GetObjectByName(acName);
        if ( pkFound )
            return pkFound;
    }

    // To avoid cycles in a recursive search of a scene graph for an object
    // with the specified name, the Node m_pkParent is not checked.
    return NULL;
}
//----------------------------------------------------------------------------
void Spatial::GetAllObjectsByName (const char* acName,
    std::vector<Object*>& rkObjects)
{
    Object::GetAllObjectsByName(acName,rkObjects);

    RenderState::List* pkList;
    for (pkList = m_pkStateList; pkList; pkList = pkList->m_pkNext)
        pkList->m_spkState->GetAllObjectsByName(acName,rkObjects);

    // To avoid cycles in a recursive search of a scene graph for an object
    // with the specified name, the Node m_pkParent is not checked.
}
//----------------------------------------------------------------------------
Spatial::PickRecord::PickRecord (Spatial* pkObject, float fRayT)
    :
    m_spkObject(pkObject)
{
    m_fRayT = fRayT;
}
//----------------------------------------------------------------------------
Spatial::PickRecord::~PickRecord ()
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
void Spatial::DoPick (const Vector3f&, const Vector3f&, PickArray&)
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
void Spatial::Sort (PickArray& rkResults)
{
    // sort in ascending order
    std::sort(rkResults.begin(),rkResults.end());
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* Spatial::Factory (Stream&)
{
    // Spatial is abstract, Factory never called
    return NULL;
}
//----------------------------------------------------------------------------
void Spatial::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_kRotate);
    StreamRead(rkStream,m_kTranslate);
    StreamRead(rkStream,m_fScale);

    // world transforms and world bound are derived, no need to save

    // link data
    int iQuantity;
    StreamRead(rkStream,iQuantity);
    m_pkStateList = 0;
    for (int i = 0; i < iQuantity; i++)
    {
        RenderState* pkState;
        StreamRead(rkStream,pkState);
        pkLink->Add(pkState);

        // build render state list, to be filled in by Link
        RenderState::List* pkList = new RenderState::List;
        pkList->m_spkState = NULL;
        pkList->m_pkNext = m_pkStateList;
        m_pkStateList = pkList;
    }
}
//----------------------------------------------------------------------------
void Spatial::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Link(rkStream,pkLink);

    RenderState::List* pkList;
    for (pkList = m_pkStateList; pkList; pkList = pkList->m_pkNext)
    {
        Object* pkLinkID = pkLink->GetLinkID();
        pkList->m_spkState = (RenderState*)rkStream.GetFromMap(pkLinkID);
    }
}
//----------------------------------------------------------------------------
bool Spatial::Register (Stream& rkStream)
{
    if ( !Object::Register(rkStream) )
        return false;

    // m_pkParent need not be registered since the parent itself must have
    // initiated the Register call to its children, 'this' being one of them.

    RenderState::List* pkList;
    for (pkList = m_pkStateList; pkList; pkList = pkList->m_pkNext)
    {
        RenderState* pkState = pkList->m_spkState;
        if ( pkState )
            pkState->Register(rkStream);
    }

    return true;
}
//----------------------------------------------------------------------------
void Spatial::Save (Stream& rkStream)
{
    Object::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_kRotate);
    StreamWrite(rkStream,m_kTranslate);
    StreamWrite(rkStream,m_fScale);

    // world transforms and world bound are derived, no need to save

    // link data

    // m_pkParent need not be saved since 'this' will be attached as a child
    // in Node::Link.

    RenderState::List* pkList;
    int iQuantity = 0;
    for (pkList = m_pkStateList; pkList; pkList = pkList->m_pkNext)
        iQuantity++;

    StreamWrite(rkStream,iQuantity);

    for (pkList = m_pkStateList; pkList; pkList = pkList->m_pkNext)
        StreamWrite(rkStream,pkList->m_spkState);
}
//----------------------------------------------------------------------------
StringTree* Spatial::SaveStrings ()
{
    int iChildQuantity = 1;
    RenderState::List* pkList;
    for (pkList = m_pkStateList; pkList; pkList = pkList->m_pkNext)
    {
        RenderState* pkState = pkList->m_spkState;
        if ( pkState )
            iChildQuantity++;
    }

    StringTree* pkTree = new StringTree(9,0,iChildQuantity,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("local rotate =",m_kRotate));
    pkTree->SetString(2,MakeString("local translate =",m_kTranslate));
    pkTree->SetString(3,MakeString("local scale =",m_fScale));
    pkTree->SetString(4,MakeString("world rotate =",m_kWorldRotate));
    pkTree->SetString(5,MakeString("world translate =",m_kWorldTranslate));
    pkTree->SetString(6,MakeString("world scale =",m_fWorldScale));
    pkTree->SetString(7,MakeString("world bound =",m_kWorldBound));
    pkTree->SetString(8,MakeString("force cull =",m_bForceCull));

    // children
    pkTree->SetChild(0,Object::SaveStrings());
    int iSlot = 1;
    for (pkList = m_pkStateList; pkList; pkList = pkList->m_pkNext)
    {
        RenderState* pkState = pkList->m_spkState;
        if ( pkState )
            pkTree->SetChild(iSlot++,pkState->SaveStrings());
    }

    return pkTree;
}
//----------------------------------------------------------------------------
int Spatial::GetMemoryUsed () const
{
    int iBaseSize = sizeof(Spatial) - sizeof(Object);

    int iDynaSize = 0;
    RenderState::List* pkList = m_pkStateList;
    while ( pkList )
    {
        iDynaSize += sizeof(RenderState::List);
        pkList = pkList->m_pkNext;
    }

    int iTotalSize = iBaseSize + iDynaSize + Object::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int Spatial::GetDiskUsed () const
{
    int iSize = Object::GetDiskUsed() +
        sizeof(m_kRotate) +
        sizeof(m_kTranslate) +
        sizeof(m_fScale);

    RenderState::List* pkList;
    int iQuantity = 0;
    for (pkList = m_pkStateList; pkList; pkList = pkList->m_pkNext)
        iQuantity++;

    // List of states stored as 4-byte integer (quantity of state) and
    // array of pointers to states.
    iSize += sizeof(int) + iQuantity*sizeof(RenderStatePtr);

    return iSize;
}
//----------------------------------------------------------------------------


