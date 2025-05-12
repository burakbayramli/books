// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlSwitchNode.h"
using namespace Wml;
using namespace std;

WmlImplementRTTI(SwitchNode,Node);
WmlImplementStream(SwitchNode);

//----------------------------------------------------------------------------
SwitchNode::SwitchNode (int iQuantity, int iGrowBy)
    :
    Node(iQuantity,iGrowBy)
{
    m_iActiveChild = SN_INVALID_CHILD;
}
//----------------------------------------------------------------------------
void SwitchNode::SetActiveChild (int iActiveChild)
{
    assert(iActiveChild == SN_INVALID_CHILD || iActiveChild < GetQuantity());
    m_iActiveChild = iActiveChild;
}
//----------------------------------------------------------------------------
void SwitchNode::Draw (Renderer& rkRenderer)
{
    if ( m_iActiveChild != SN_INVALID_CHILD )
    {
        Spatial* pkChild = m_kChild[m_iActiveChild];
        if ( pkChild )
            pkChild->OnDraw(rkRenderer);
    }
}
//----------------------------------------------------------------------------
void SwitchNode::DoPick (const Vector3f& rkOrigin,
    const Vector3f& rkDirection, PickArray& rkResults)
{
    if ( m_iActiveChild != SN_INVALID_CHILD )
    {
        if ( m_kWorldBound.TestIntersection(rkOrigin,rkDirection) )
        {
            Spatial* pkChild = m_kChild[m_iActiveChild];
            if ( pkChild )
                pkChild->DoPick(rkOrigin,rkDirection,rkResults);
        }
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* SwitchNode::Factory (Stream& rkStream)
{
    SwitchNode* pkObject = new SwitchNode;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void SwitchNode::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Node::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_iActiveChild);
}
//----------------------------------------------------------------------------
void SwitchNode::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Node::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool SwitchNode::Register (Stream& rkStream)
{
    return Node::Register(rkStream);
}
//----------------------------------------------------------------------------
void SwitchNode::Save (Stream& rkStream)
{
    Node::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_iActiveChild);
}
//----------------------------------------------------------------------------
StringTree* SwitchNode::SaveStrings ()
{
    // TO DO.  Finish implementation.
    StringTree* pkTree = new StringTree(1,0,1,0);
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetChild(0,Node::SaveStrings());
    return pkTree;
}
//----------------------------------------------------------------------------
int SwitchNode::GetMemoryUsed () const
{
    int iBaseSize = sizeof(SwitchNode) - sizeof(Node);
    int iTotalSize = iBaseSize + Node::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int SwitchNode::GetDiskUsed () const
{
    return Node::GetDiskUsed() +
        sizeof(m_iActiveChild);
}
//----------------------------------------------------------------------------
