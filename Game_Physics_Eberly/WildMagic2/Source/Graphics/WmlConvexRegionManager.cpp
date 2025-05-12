// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlConvexRegionManager.h"
#include "WmlConvexRegion.h"
#include "WmlRenderer.h"
using namespace Wml;

WmlImplementRTTI(ConvexRegionManager,BspNode);
WmlImplementStream(ConvexRegionManager);

//----------------------------------------------------------------------------
ConvexRegionManager::ConvexRegionManager (bool bUseEyePlusNear)
{
    m_bUseEyePlusNear = bUseEyePlusNear;
}
//----------------------------------------------------------------------------
SpatialPtr ConvexRegionManager::AttachOutside (Spatial* pkOutside)
{
    return SetChild(1,pkOutside);
}
//----------------------------------------------------------------------------
SpatialPtr ConvexRegionManager::DetachOutside ()
{
    return DetachChildAt(1);
}
//----------------------------------------------------------------------------
SpatialPtr ConvexRegionManager::GetOutside ()
{
    return GetChild(1);
}
//----------------------------------------------------------------------------
ConvexRegion* ConvexRegionManager::GetContainingRegion (
    const Vector3f& rkPoint)
{
    return WmlDynamicCast(ConvexRegion,GetContainingNode(rkPoint));
}
//----------------------------------------------------------------------------
void ConvexRegionManager::Draw (Renderer& rkRenderer)
{
    CameraPtr spCamera = rkRenderer.GetCamera();
    Vector3f kEye = spCamera->GetLocation();
    if ( m_bUseEyePlusNear )
        kEye += spCamera->GetFrustumNear()*spCamera->GetDirection();

    ConvexRegion* pkRegion = GetContainingRegion(kEye);

    if ( pkRegion )
    {
        // inside the set of regions, start drawing with region of camera
        pkRegion->Draw(rkRenderer);
    }
    else
    {
        // outside the set of regions, draw the outside scene (if it exists)
        if ( GetOutside() )
            GetOutside()->Draw(rkRenderer);
    }

}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* ConvexRegionManager::Factory (Stream& rkStream)
{
    ConvexRegionManager* pkObject = new ConvexRegionManager;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void ConvexRegionManager::Load (Stream& rkStream,
    Stream::Link* pkLink)
{
    BspNode::Load(rkStream,pkLink);

    // native data
    StreamReadBool(rkStream,m_bUseEyePlusNear);
}
//----------------------------------------------------------------------------
void ConvexRegionManager::Link (Stream& rkStream,
    Stream::Link* pkLink)
{
    BspNode::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool ConvexRegionManager::Register (Stream& rkStream)
{
    return BspNode::Register(rkStream);
}
//----------------------------------------------------------------------------
void ConvexRegionManager::Save (Stream& rkStream)
{
    BspNode::Save(rkStream);

    // native data
    StreamWriteBool(rkStream,m_bUseEyePlusNear);
}
//----------------------------------------------------------------------------
StringTree* ConvexRegionManager::SaveStrings ()
{
    // TO DO.  Finish implementation.
    StringTree* pkTree = new StringTree(1,0,1,0);
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetChild(0,BspNode::SaveStrings());
    return pkTree;
}
//----------------------------------------------------------------------------
int ConvexRegionManager::GetMemoryUsed () const
{
    int iBaseSize = sizeof(ConvexRegionManager) - sizeof(BspNode);
    int iTotalSize = iBaseSize + BspNode::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int ConvexRegionManager::GetDiskUsed () const
{
    return BspNode::GetDiskUsed() +
        StreamBytesBool(m_bUseEyePlusNear);
}
//----------------------------------------------------------------------------
