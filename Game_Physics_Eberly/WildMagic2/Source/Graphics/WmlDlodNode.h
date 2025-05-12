// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDLODNODE_H
#define WMLDLODNODE_H

#include "WmlSwitchNode.h"
#include <vector>

namespace Wml
{

class Camera;

class WML_ITEM DlodNode : public SwitchNode
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // construction
    DlodNode (int iQuantity = 1, int iGrowBy = 1);

    // center for level of detail
    Vector3f& ModelCenter ();
    const Vector3f& ModelCenter () const;
    const Vector3f& WorldCenter () const;

    // distance intervals for children
    void SetModelMinSqrDistance (int i, float fMinSqrDist);
    void SetModelMaxSqrDistance (int i, float fMaxSqrDist);
    void SetModelSqrDistance (int i, float fMinSqrDist, float fMaxSqrDist);

    float GetModelMinSqrDistance (int i) const;
    float GetModelMaxSqrDistance (int i) const;
    void GetModelSqrDistance (int i, float& rfMinSqrDist,
        float& rfMaxSqrDist) const;

    float GetWorldMinSqrDistance (int i) const;
    float GetWorldMaxSqrDistance (int i) const;
    void GetWorldSqrDistance (int i, float& rfMinSqrDist,
        float& rfMaxSqrDist) const;

protected:
    // geometric updates
    void SelectLevelOfDetail (const Camera* pkCamera);
    virtual void UpdateWorldData (float fAppTime);

    // drawing
    virtual void Draw (Renderer& rkRenderer);

    // for deferred updates
    float m_fLastUpdateTime;

    // point whose distance to camera determines correct child
    Vector3f m_kModelLodCenter;
    Vector3f m_kWorldLodCenter;

    // squared distances for each LOD interval
    std::vector<float> m_afModelMinSqrDist;
    std::vector<float> m_afModelMaxSqrDist;
    std::vector<float> m_afWorldMinSqrDist;
    std::vector<float> m_afWorldMaxSqrDist;
};

WmlSmartPointer(DlodNode);
WmlRegisterStream(DlodNode);
#include "WmlDlodNode.inl"

}

#endif
