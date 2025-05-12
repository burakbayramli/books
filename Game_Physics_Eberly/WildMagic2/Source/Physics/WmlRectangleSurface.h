// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef WMLRECTANGLESURFACE_H
#define WMLRECTANGLESURFACE_H

#include "WmlParametricSurface.h"
#include "WmlTriMesh.h"

namespace Wml
{

class WML_ITEM RectangleSurface : public TriMesh
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // Construction and destruction.  The input surface must be rectangular,
    // not triangular.  RectangleSurface accepts responsibility for deleting
    // the input surface.  If bWantColors is 'true', the vertex colors are
    // allocated and set to black.  The application needs to assign colors as
    // needed.  If either of pkTextureMin or pkTextureMax is not null, both
    // must be not null.  In this case, texture coordinates are generated for
    // the surface.
    RectangleSurface (ParametricSurfacef* pkSurface, int iUSamples,
        int iVSamples, bool bWantNormals, bool bWantColors, bool bDoubleSided,
        const Vector2f* pkTextureMin, const Vector2f* pkTextureMax);

    virtual ~RectangleSurface ();

    // member access
    ParametricSurfacef*& Surface ();
    const ParametricSurfacef* GetSurface () const;
    int GetUSamples () const;
    int GetVSamples () const;

    // If the surface is modified, for example if it is control point
    // based and the control points are modified, then you should call this
    // update function to recompute the rectangle surface geometry.
    void UpdateSurface ();

protected:
    RectangleSurface ();

    ParametricSurfacef* m_pkSurface;
    int m_iUSamples, m_iVSamples;
};

WmlSmartPointer(RectangleSurface);
WmlRegisterStream(RectangleSurface);
#include "WmlRectangleSurface.inl"

}

#endif
