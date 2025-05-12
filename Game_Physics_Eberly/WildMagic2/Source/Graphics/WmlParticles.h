// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLPARTICLES_H
#define WMLPARTICLES_H

#include "WmlTriMesh.h"

namespace Wml
{

class Camera;

class WML_ITEM Particles : public Geometry
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // Construction and destruction.  Particles accepts responsibility for
    // deleting the input arrays.
    Particles (int iPQuantity, Vector3f* akPCenter, Vector3f* akPNormal,
        ColorRGB* akPColor, float* afSize, bool bUseTexture0,
        bool bUseTexture1 = false, bool bUseTexture2 = false,
        bool bUseTexture3 = false, bool bUseTextureBump = false);

    virtual ~Particles ();

    // member access
    void SetActiveQuantity (int iActiveQuantity);
    int GetActiveQuantity () const;
    float* Sizes ();
    const float* Sizes () const;

    // The particle sizes are considered to be relative measurements.  The
    // sizes must be adjusted to allow particles to appear large or small
    // depending on the application's needs.  The initial value is 1.0.
    float& SizeAdjust ();
    const float& SizeAdjust () const;

    const TriMeshPtr GetMesh () const;

    // To dynamically change vertex normals and/or colors, you will need to
    // inform the object that these have changed.  The Particles::Geometry
    // base class stores the "particle" attributes.  The Particles class
    // generates quadrilaterals to represent the particles and the TriMesh
    // member stores the "quad" attributes--these are derived from the
    // "particle" attributes.
    void ColorsHaveChanged ();
    void NormalsHaveChanged ();

protected:
    Particles ();
    void CreateMesh ();

    // drawing
    void GenerateParticles (const Camera* pkCamera);
    virtual void Draw (Renderer& rkRenderer);

    // Allow application to specify fewer than the maximum number of vertices
    // to draw.
    int m_iActiveQuantity;

    float* m_afSize;
    float m_fSizeAdjust;

    // triangle mesh that represents the particles (derived quantity)
    TriMeshPtr m_spkMesh;

    // flags indicating whether or not texture channels are used
    bool m_bUseTexture0;
    bool m_bUseTexture1;
    bool m_bUseTexture2;
    bool m_bUseTexture3;
    bool m_bUseTextureBump;
};

WmlSmartPointer(Particles);
WmlRegisterStream(Particles);
#include "WmlParticles.inl"

}

#endif

