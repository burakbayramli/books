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
using namespace Wml;

WmlImplementRTTI(Node,Spatial);
WmlImplementStream(Node);

//----------------------------------------------------------------------------
Node::Node (int iQuantity, int iGrowBy)
    :
    m_kChild(iQuantity)
{
    m_iGrowBy = iGrowBy;
    m_iUsed = 0;
}
//----------------------------------------------------------------------------
Node::~Node ()
{
    for (int i = 0; i < (int)m_kChild.size(); i++)
    {
        SpatialPtr spkChild = DetachChildAt(i);
        spkChild = NULL;
    }
}
//----------------------------------------------------------------------------
int Node::AttachChild (Spatial* pkChild)
{
    // Some folks are of the impression that a node can have multiple parents,
    // the scene graph therefore being a DAG.  That is not the case.  The
    // parent-child relationships form a tree.  This assertion is to let folks
    // know this and to warn them that a child is being kidnapped from another
    // parent.  To be safe, you should really call DetachChild before you
    // reattach somewhere else with AttachChild or SetChild.
    assert( pkChild && !pkChild->GetParent() );

    pkChild->SetParent(this);
    m_iUsed++;

    // attach child in first available slot (if any)
    int iQuantity = (int)m_kChild.size();
    for (int i = 0; i < iQuantity; i++)
    {
        if ( m_kChild[i] == 0 )
        {
            m_kChild[i] = pkChild;
            return i;
        }
    }

    // all slots used, increase array size
    m_kChild.resize(iQuantity + m_iGrowBy);
    m_kChild[iQuantity] = pkChild;
    return iQuantity;
}
//----------------------------------------------------------------------------
int Node::DetachChild (Spatial* pkChild)
{
    if ( pkChild )
    {
        // search to see if child exists
        for (int i = 0; i < (int)m_kChild.size(); i++)
        {
            if ( m_kChild[i] == pkChild )
            {
                // child found, detach it
                pkChild->SetParent(NULL);
                m_kChild[i] = NULL;
                m_iUsed--;
                return i;
            }
        }
    }

    return -1;
}
//----------------------------------------------------------------------------
SpatialPtr Node::DetachChildAt (int i)
{
    if ( i < (int)m_kChild.size() )
    {
        SpatialPtr spkChild = m_kChild[i];
        if ( spkChild )
        {
            // child exists in slot, detach it
            spkChild->SetParent(NULL);
            m_kChild[i] = NULL;
            m_iUsed--;
        }
        return spkChild;
    }
    else
    {
        return NULL;
    }
}
//----------------------------------------------------------------------------
SpatialPtr Node::SetChild (int i, Spatial* pkChild)
{
    // Some folks are of the impression that a node can have multiple parents,
    // the scene graph therefore being a DAG.  That is not the case.  The
    // parent-child relationships form a tree.  This assertion is to let folks
    // know this and to warn them that a child is being kidnapped from another
    // parent.  To be safe, you should really call DetachChild before you
    // reattach somewhere else with AttachChild or SetChild.
    if ( pkChild )
    {
        assert( !pkChild->GetParent() );
    }

    if ( i < (int)m_kChild.size() )
    {
        // detach child currently in slot
        SpatialPtr spkPreviousChild = m_kChild[i];
        if ( spkPreviousChild )
        {
            spkPreviousChild->SetParent(NULL);
            m_iUsed--;
        }

        // attach new child to slot
        if ( pkChild )
        {
            pkChild->SetParent(this);
            m_iUsed++;
        }

        m_kChild[i] = pkChild;
        return spkPreviousChild;
    }
    else
    {
        // index out of range, increase array size and attach new child
        pkChild->SetParent(this);
        m_kChild.resize(i + m_iGrowBy);
        m_kChild[i] = pkChild;
        m_iUsed++;
        return NULL;
    }
}
//----------------------------------------------------------------------------
SpatialPtr Node::GetChild (int i)
{
    if ( i < (int)m_kChild.size() )
        return m_kChild[i];
    else
        return NULL;
}
//----------------------------------------------------------------------------
void Node::UpdateWorldData (float fAppTime)
{
    Spatial::UpdateWorldData(fAppTime);

    for (int i = 0; i < (int)m_kChild.size(); i++)
    {
        Spatial* pkChild = m_kChild[i];
        if ( pkChild )
            pkChild->UpdateGS(fAppTime,false);
    }
}
//----------------------------------------------------------------------------
void Node::UpdateWorldBound ()
{
    bool bFoundFirstBound = false;
    for (int i = 0; i < (int)m_kChild.size(); i++)
    {
        Spatial* pkChild = m_kChild[i];
        if ( pkChild )
        {
            if ( bFoundFirstBound )
            {
                // merge current world bound with child world bound
                m_kWorldBound += pkChild->WorldBound();
            }
            else
            {
                // set world bound to first non-null child world bound
                bFoundFirstBound = true;
                m_kWorldBound = pkChild->WorldBound();
            }
        }
    }
}
//----------------------------------------------------------------------------
void Node::UpdateRenderState (RenderState::Stack* pkStack)
{
    for (int i = 0; i < (int)m_kChild.size(); i++)
    {
        Spatial* pkChild = m_kChild[i];
        if ( pkChild )
            pkChild->UpdateRS(pkStack);
    }
}
//----------------------------------------------------------------------------
void Node::Draw (Renderer& rkRenderer)
{
    for (int i = 0; i < (int)m_kChild.size(); i++)
    {
        Spatial* pkChild = m_kChild[i];
        if ( pkChild )
            pkChild->OnDraw(rkRenderer);
    }
}
//----------------------------------------------------------------------------
Object* Node::GetObjectByName (const char* acName)
{
    Object* pkFound = Spatial::GetObjectByName(acName);
    if ( pkFound )
        return pkFound;

    for (int i = 0; i < (int)m_kChild.size(); i++)
    {
        Spatial* pkChild = m_kChild[i];
        if ( pkChild )
        {
            pkFound = pkChild->GetObjectByName(acName);
            if ( pkFound )
                return pkFound;
        }
    }

    return NULL;
}
//----------------------------------------------------------------------------
void Node::GetAllObjectsByName (const char* acName,
    std::vector<Object*>& rkObjects)
{
    Spatial::GetAllObjectsByName(acName,rkObjects);

    for (int i = 0; i < (int)m_kChild.size(); i++)
    {
        Spatial* pkChild = m_kChild[i];
        if ( pkChild )
            pkChild->GetAllObjectsByName(acName,rkObjects);
    }
}
//----------------------------------------------------------------------------
void Node::DoPick (const Vector3f& rkOrigin, const Vector3f& rkDirection,
    PickArray& rkResults)
{
    if ( m_kWorldBound.TestIntersection(rkOrigin,rkDirection) )
    {
        for (int i = 0; i < (int)m_kChild.size(); i++)
        {
            Spatial* pkChild = m_kChild[i];
            if ( pkChild )
                pkChild->DoPick(rkOrigin,rkDirection,rkResults);
        }
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* Node::Factory (Stream& rkStream)
{
    Node* pkObject = new Node;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void Node::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Spatial::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_iGrowBy);

    if ( rkStream.GetVersion() == Version(1,0) )
    {
        // The value m_iUsed is really a derived quantity that gets set in the
        // Link call when children are added.  It was streamed in Version
        // 1.00, but does not need to be (and is not in later versions).
        StreamRead(rkStream,m_iUsed);
        m_iUsed = 0;
    }

    // link data
    int iQuantity;
    StreamRead(rkStream,iQuantity);
    for (int i = 0; i < iQuantity; i++)
    {
        Spatial* pkChild;
        StreamRead(rkStream,pkChild);
        pkLink->Add(pkChild);
    }

    // set array quantity, array filled in by Link
    m_kChild.resize(iQuantity);
}
//----------------------------------------------------------------------------
void Node::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Spatial::Link(rkStream,pkLink);

    for (int i = 0; i < (int)m_kChild.size(); i++)
    {
        Object* pkLinkID = pkLink->GetLinkID();
        Spatial* pkChild = (Spatial*)rkStream.GetFromMap(pkLinkID);
        SetChild(i,pkChild);
    }
}
//----------------------------------------------------------------------------
bool Node::Register (Stream& rkStream)
{
    if ( !Spatial::Register(rkStream) )
        return false;

    for (int i = 0; i < (int)m_kChild.size(); i++)
    {
        if ( m_kChild[i] )
            m_kChild[i]->Register(rkStream);
    }

    return true;
}
//----------------------------------------------------------------------------
void Node::Save (Stream& rkStream)
{
    Spatial::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_iGrowBy);

    // link data
    StreamWrite(rkStream,m_kChild.size());
    for (int i = 0; i < (int)m_kChild.size(); i++)
        StreamWrite(rkStream,m_kChild[i]);
}
//----------------------------------------------------------------------------
StringTree* Node::SaveStrings ()
{
    StringTree* pkTree = new StringTree(4,0,m_iUsed+1,0);

    // strings
    int iCQuantity = (int)m_kChild.size();
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("quantity =",iCQuantity));
    pkTree->SetString(2,MakeString("used =",m_iUsed));
    pkTree->SetString(3,MakeString("grow by =",m_iGrowBy));

    // children
    pkTree->SetChild(0,Spatial::SaveStrings());
    int iSlot = 1;
    for (int i = 0; i < iCQuantity; i++)
    {
        Spatial* pkChild = m_kChild[i];
        if ( pkChild )
            pkTree->SetChild(iSlot++,pkChild->SaveStrings());
    }

    return pkTree;
}
//----------------------------------------------------------------------------
int Node::GetMemoryUsed () const
{
    int iBaseSize = sizeof(Node) - sizeof(Spatial);
    int iDynaSize = ((int)m_kChild.size())*sizeof(m_kChild[0]);
    int iTotalSize = iBaseSize + iDynaSize + Spatial::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int Node::GetDiskUsed () const
{
    return Spatial::GetDiskUsed() +
        sizeof(m_iGrowBy) +
        sizeof(m_kChild.size()) +
        ((int)m_kChild.size())*sizeof(m_kChild[0]);
}
//----------------------------------------------------------------------------
