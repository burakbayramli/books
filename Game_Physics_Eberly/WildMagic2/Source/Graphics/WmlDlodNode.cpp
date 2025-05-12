// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDlodNode.h"
#include "WmlRenderer.h"
using namespace Wml;

WmlImplementRTTI(DlodNode,SwitchNode);
WmlImplementStream(DlodNode);

//----------------------------------------------------------------------------
DlodNode::DlodNode (int iQuantity, int iGrowBy)
    :
    SwitchNode(iQuantity,iGrowBy),
    m_afModelMinSqrDist(iQuantity),
    m_afModelMaxSqrDist(iQuantity),
    m_afWorldMinSqrDist(iQuantity),
    m_afWorldMaxSqrDist(iQuantity)
{
    m_fLastUpdateTime = -FLT_MAX;
}
//----------------------------------------------------------------------------
void DlodNode::SetModelMinSqrDistance (int i, float fMinSqrDist)
{
    int iQuantity = (int)m_afModelMinSqrDist.size();
    if ( i >= iQuantity )
    {
        m_afModelMinSqrDist.resize(iQuantity+m_iGrowBy);
        m_afWorldMinSqrDist.resize(iQuantity+m_iGrowBy);
    }

    m_afModelMinSqrDist[i] = fMinSqrDist;
}
//----------------------------------------------------------------------------
void DlodNode::SetModelMaxSqrDistance (int i, float fMaxSqrDist)
{
    int iQuantity = (int)m_afModelMaxSqrDist.size();
    if ( i >= iQuantity )
    {
        m_afModelMaxSqrDist.resize(iQuantity+m_iGrowBy);
        m_afWorldMaxSqrDist.resize(iQuantity+m_iGrowBy);
    }

    m_afModelMaxSqrDist[i] = fMaxSqrDist;
}
//----------------------------------------------------------------------------
void DlodNode::SetModelSqrDistance (int i, float fMinSqrDist,
    float fMaxSqrDist)
{
    int iQuantity = (int)m_afModelMinSqrDist.size();
    if ( i >= iQuantity )
    {
        m_afModelMinSqrDist.resize(iQuantity+m_iGrowBy);
        m_afWorldMinSqrDist.resize(iQuantity+m_iGrowBy);
    }

    iQuantity = (int)m_afModelMaxSqrDist.size();
    if ( i >= iQuantity )
    {
        m_afModelMaxSqrDist.resize(iQuantity+m_iGrowBy);
        m_afWorldMaxSqrDist.resize(iQuantity+m_iGrowBy);
    }

    m_afModelMinSqrDist[i] = fMinSqrDist;
    m_afModelMaxSqrDist[i] = fMaxSqrDist;
}
//----------------------------------------------------------------------------
void DlodNode::SelectLevelOfDetail (const Camera* pkCamera)
{
    // ASSERT:  The child array of an DlodNode is compacted, that is,
    // there are no empty slots in the array and the number of children
    // is GetQuantity().  Moreover, it is assumed that all model squared
    // distance values were set for these children.

    SwitchNode::UpdateWorldData(m_fLastUpdateTime);

    // compute world LOD center
    m_kWorldLodCenter = m_kWorldTranslate + m_fWorldScale*(
        m_kWorldRotate*m_kModelLodCenter);

    // compute world squared distance intervals
    float fWorldSqrScale = m_fWorldScale*m_fWorldScale;
    int i;
    for (i = 0; i < (int)m_kChild.size(); i++)
    {
        float fValue = m_afModelMinSqrDist[i];
        m_afWorldMinSqrDist[i] = fWorldSqrScale*fValue*fValue;

        fValue = m_afModelMaxSqrDist[i];
        m_afWorldMaxSqrDist[i] = fWorldSqrScale*fValue*fValue;
    }

    // select the LOD child
    SetActiveChild(SN_INVALID_CHILD);
    if ( m_kChild.size() > 0 )
    {
        Vector3f kDiff = m_kWorldLodCenter - pkCamera->GetLocation();
        float fSqrDist = kDiff.SquaredLength();

        for (i = 0; i < (int)m_kChild.size(); i++) 
        {
            if ( m_afWorldMinSqrDist[i] <= fSqrDist
            &&   fSqrDist < m_afWorldMaxSqrDist[i] )
            {
                SetActiveChild(i);
                break;
            }
        }
    }
}
//----------------------------------------------------------------------------
void DlodNode::UpdateWorldData (float fAppTime)
{
    // Save the update time.  The update is deferred until Draw so that the
    // selected LOD child is visible .  At this time the update time is
    // needed to pass to the UpdateGS call.
    m_fLastUpdateTime = fAppTime;

    // The deferred update means that none of the children world data is
    // computed by a recursive traversal.  Just compute the world bound.
    UpdateWorldBound();
}
//----------------------------------------------------------------------------
void DlodNode::Draw (Renderer& rkRenderer)
{
    SelectLevelOfDetail(rkRenderer.GetCamera());
    SwitchNode::Draw(rkRenderer);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* DlodNode::Factory (Stream& rkStream)
{
    DlodNode* pkObject = new DlodNode;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void DlodNode::Load (Stream& rkStream, Stream::Link* pkLink)
{
    SwitchNode::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_kModelLodCenter);

    int i, iQuantity;
    StreamRead(rkStream,iQuantity);
    m_afModelMinSqrDist.resize(iQuantity);
    for (i = 0; i < iQuantity; i++)
        StreamRead(rkStream,m_afModelMinSqrDist[i]);

    StreamRead(rkStream,iQuantity);
    m_afModelMaxSqrDist.resize(iQuantity);
    for (i = 0; i < iQuantity; i++)
        StreamRead(rkStream,m_afModelMaxSqrDist[i]);
}
//----------------------------------------------------------------------------
void DlodNode::Link (Stream& rkStream, Stream::Link* pkLink)
{
    SwitchNode::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool DlodNode::Register (Stream& rkStream)
{
    return SwitchNode::Register(rkStream);
}
//----------------------------------------------------------------------------
void DlodNode::Save (Stream& rkStream)
{
    SwitchNode::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_kModelLodCenter);

    int i, iQuantity = (int)m_afModelMinSqrDist.size();
    StreamWrite(rkStream,iQuantity);
    for (i = 0; i < iQuantity; i++)
        StreamWrite(rkStream,m_afModelMinSqrDist[i]);

    iQuantity = (int)m_afModelMaxSqrDist.size();
    StreamWrite(rkStream,iQuantity);
    for (i = 0; i < iQuantity; i++)
        StreamWrite(rkStream,m_afModelMaxSqrDist[i]);

    // The world LOD center is computed from the model LOD center in
    // SelectLevelOfDetail.  The world distance extremes are also computed
    // from the model distance extremes in SelectLevelOfDetail.  These
    // world quantities do not need to be saved.
}
//----------------------------------------------------------------------------
StringTree* DlodNode::SaveStrings ()
{
    // TO DO.  Finish implementation.
    StringTree* pkTree = new StringTree(1,0,1,0);
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetChild(0,SwitchNode::SaveStrings());
    return pkTree;
}
//----------------------------------------------------------------------------
int DlodNode::GetMemoryUsed () const
{
    int iBaseSize = sizeof(DlodNode) - sizeof(SwitchNode);
    int iDynaSize = ((int)m_afModelMinSqrDist.size())*(4*sizeof(float));
    int iTotalSize = iBaseSize + iDynaSize + SwitchNode::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int DlodNode::GetDiskUsed () const
{
    int iSize = SwitchNode::GetDiskUsed() +
        sizeof(m_kModelLodCenter);

    int iQuantity = (int)m_afModelMinSqrDist.size();
    iSize += sizeof(iQuantity) + iQuantity*sizeof(m_afModelMinSqrDist[0]);
    iQuantity = (int)m_afModelMaxSqrDist.size();
    iSize += sizeof(iQuantity) + iQuantity*sizeof(m_afModelMaxSqrDist[0]);

    return iSize;
}
//----------------------------------------------------------------------------
