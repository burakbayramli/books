// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCOLLAPSERECORD_H
#define WMLCOLLAPSERECORD_H

// Representation of an edge collapse.  The current implementation
// collapses an edge <V0,V1> by replacing V1 by V0.  No new vertices or
// surface attributes are introduced.  Moreover, the vertex array and
// connectivity array are permuted so that vertices and triangles that
// are removed first in the collapse process occur at the ends of their
// arrays.  This supports sending the vertices as static geometry to the
// hardware renderer.  The level of detail selection only needs to (1)
// change the active vertex and triangle counts and (2) update the
// connectivity array based on the m_auiIndex values.
//
// Some lighting artifacts occur if normal vectors are used.  The
// artifacts can be reduced if the normals are recalculated from the
// connectivity array each time the target record changes.  In this case
// the normal array becomes dynamic and you have to tell the renderer to
// repack each frame.
//
// The Record class can be modified to store new vertices and attributes
// for incremental updates.  For example, you might want to replace V0
// and V1 by (V0+V1)/2 and average the corresponding attributes.  In this
// case the vertex array becomes dynamic and you have to tell the
// renderer to repack each frame.

#include "WmlStream.h"

namespace Wml
{

class WML_ITEM CollapseRecord
{
public:
    CollapseRecord (int iVKeep = -1, int iVThrow = -1, int iVQuantity = 0,
        int iTQuantity = 0)
    {
        m_iVKeep = iVKeep;
        m_iVThrow = iVThrow;
        m_iVQuantity = iVQuantity;
        m_iTQuantity = iTQuantity;
        m_iIQuantity = 0;
        m_aiIndex = NULL;
    }

    ~CollapseRecord ()
    {
        delete[] m_aiIndex;
    }

    void Read (Stream& rkStream);
    void Write (Stream& rkStream);
    int GetMemoryUsed () const;
    int GetDiskUsed () const;

    // edge <VKeep,VThrow> collapses so that VThrow is replaced by VKeep
    int m_iVKeep, m_iVThrow;

    // number of vertices after edge collapse
    int m_iVQuantity;

    // number of triangles after edge collapse
    int m_iTQuantity;

    // connectivity array indices in [0..TQ-1] that contain VThrow
    int m_iIQuantity;
    int* m_aiIndex;
};

}

#endif
