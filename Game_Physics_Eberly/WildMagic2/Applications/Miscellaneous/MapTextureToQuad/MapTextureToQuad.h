// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef MAPTEXTURETOQUAD_H
#define MAPTEXTURETOQUAD_H

#include "WmlApplication2.h"
#include "WmlQuadToQuadTransforms.h"

class MapTextureToQuad : public Application2
{
public:
    MapTextureToQuad ();
    virtual ~MapTextureToQuad ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnDisplay ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

    // allows user to drag vertices of convex quadrilateral
    virtual void OnMouseClick (int iButton, int iState, int iX, int iY,
        unsigned int uiModifiers);
    virtual void OnMotion (int iX, int iY, unsigned int uiModifiers);

protected:
    // the image to perspectively draw onto the convex quadrilateral.
    Image* m_pkImage;

    // The four vertices of the convex quadrilateral in counterclockwise
    // order:  Q00 = V[0], Q10 = V[1], Q11 = V[2], Q01 = V[3].
    void CreateMapping ();
    Vector2f m_akVertex[4];
    HmQuadToSqrf* m_pkMap;

    // for dragging the quadrilateral vertices
    void SelectVertex (const Vector2f& rkPos);
    void UpdateQuadrilateral (const Vector2f& rkPos);
    bool m_bMouseDown;
    int m_iSelected;
};

#endif
