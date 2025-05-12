// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCLODMESH_H
#define WMLCLODMESH_H

#include "WmlCollapseRecord.h"
#include "WmlTriMesh.h"

namespace Wml
{

class WML_ITEM ClodMesh : public TriMesh
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // Construction and destruction.  ClodMesh accepts responsibility for
    // deleting the input arrays.
    ClodMesh (int iVertexQuantity, Vector3f* akVertex, Vector3f* akNormal,
        ColorRGB* akColor, Vector2f* akTexture, int iTriangleQuantity,
        int* aiConnect, int iRecordQuantity = 0,
        CollapseRecord* akRecord = NULL);

    virtual ~ClodMesh ();

    // LOD selection is based on manual selection by the application.  To
    // use distance from camera or screen space coverage, derive a class
    // from WmlClodMesh and override 'GetAutomatedTargetRecord'.
    int GetRecordQuantity () const;
    int& TargetRecord ();
    virtual int GetAutomatedTargetRecord ();

    // Geometric updates.  The Draw method will call this update and adjust
    // the TriMesh quantities according to the current value of the target
    // record.  You can call this manually in an application that does not
    // need to display the mesh.
    void SelectLevelOfDetail ();

protected:
    ClodMesh ();

    // drawing
    virtual void Draw (Renderer& rkRenderer);

    // selection of LOD
    int m_iCurrentRecord, m_iTargetRecord;
    int m_iRecordQuantity;
    CollapseRecord* m_akRecord;
};

WmlSmartPointer(ClodMesh);
WmlRegisterStream(ClodMesh);
#include "WmlClodMesh.inl"

}

#endif
