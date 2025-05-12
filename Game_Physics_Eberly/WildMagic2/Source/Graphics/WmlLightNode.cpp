// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlLightNode.h"
#include "WmlDirectionalLight.h"
#include "WmlPointLight.h"
#include "WmlSpotLight.h"
using namespace Wml;

WmlImplementRTTI(LightNode,Node);
WmlImplementStream(LightNode);

//----------------------------------------------------------------------------
LightNode::LightNode (Light* pkLight, int iQuantity, int iGrowBy)
    :
    Node(iQuantity,iGrowBy),
    m_spkLight(pkLight)
{
}
//----------------------------------------------------------------------------
void LightNode::UpdateWorldData (float fAppTime)
{
    Node::UpdateWorldData(fAppTime);

    if ( m_spkLight )
    {
        switch ( m_spkLight->GetType() )
        {
        case Light::LT_DIRECTIONAL:
        {
            DirectionalLight* pkDLight =
                WmlStaticCast(DirectionalLight,m_spkLight);
            pkDLight->Direction() = m_kWorldRotate.GetColumn(2);
            break;
        }
        case Light::LT_POINT:
        {
            PointLight* pkPLight = WmlStaticCast(PointLight,m_spkLight);
            pkPLight->Location() = m_kWorldTranslate;
            break;
        }
        case Light::LT_SPOT:
        {
            SpotLight* pkSLight = WmlStaticCast(SpotLight,m_spkLight);
            pkSLight->Location() = m_kWorldTranslate;
            pkSLight->Direction() = m_kWorldRotate.GetColumn(2);
            break;
        }
        default:  // Light::LT_AMBIENT, Light::LT_QUANTITY
            break;
        }
    }
}
//----------------------------------------------------------------------------
Object* LightNode::GetObjectByName (const char* acName)
{
    Object* pkFound = Node::GetObjectByName(acName);
    if ( pkFound )
        return pkFound;

    if ( m_spkLight )
    {
        pkFound = m_spkLight->GetObjectByName(acName);
        if ( pkFound )
            return pkFound;
    }

    return NULL;
}
//----------------------------------------------------------------------------
void LightNode::GetAllObjectsByName (const char* acName,
    std::vector<Object*>& rkObjects)
{
    Node::GetAllObjectsByName(acName,rkObjects);

    if ( m_spkLight )
        m_spkLight->GetAllObjectsByName(acName,rkObjects);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* LightNode::Factory (Stream& rkStream)
{
    LightNode* pkObject = new LightNode;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void LightNode::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Node::Load(rkStream,pkLink);

    // link data
    Light* pkLight;
    StreamRead(rkStream,pkLight);
    pkLink->Add(pkLight);
}
//----------------------------------------------------------------------------
void LightNode::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Node::Link(rkStream,pkLink);

    Object* pkLinkID = pkLink->GetLinkID();
    m_spkLight = (Light*)rkStream.GetFromMap(pkLinkID);
}
//----------------------------------------------------------------------------
bool LightNode::Register (Stream& rkStream)
{
    if ( !Node::Register(rkStream) )
        return false;

    if ( m_spkLight )
        m_spkLight->Register(rkStream);

    return true;
}
//----------------------------------------------------------------------------
void LightNode::Save (Stream& rkStream)
{
    Node::Save(rkStream);

    // link data
    StreamWrite(rkStream,m_spkLight);
}
//----------------------------------------------------------------------------
StringTree* LightNode::SaveStrings ()
{
    int iCQuantity = ( m_spkLight ? 2 : 1 );
    StringTree* pkTree = new StringTree(1,0,iCQuantity,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));

    // children
    pkTree->SetChild(0,Spatial::SaveStrings());

    if ( m_spkLight )
        pkTree->SetChild(1,m_spkLight->SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int LightNode::GetMemoryUsed () const
{
    int iBaseSize = sizeof(LightNode) - sizeof(Node);
    int iTotalSize = iBaseSize + Node::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int LightNode::GetDiskUsed () const
{
    return Node::GetDiskUsed() +
        sizeof(m_spkLight);
}
//----------------------------------------------------------------------------
