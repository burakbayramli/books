// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLGEOMETRY_H
#define WMLGEOMETRY_H

#include "WmlSpatial.h"
#include "WmlColorRGB.h"
#include "WmlVector2.h"
#include "WmlPixelShader.h"
#include "WmlVertexShader.h"
#include "WmlShaderConstants.h"

namespace Wml
{

class WML_ITEM Geometry : public Spatial
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    virtual ~Geometry ();

    // vertex access
    int GetVertexQuantity () const;
    Vector3f& Vertex (int i);
    const Vector3f& Vertex (int i) const;
    Vector3f* Vertices ();
    const Vector3f* Vertices () const;

    // normal vector access
    Vector3f& Normal (int i);
    const Vector3f& Normal (int i) const;
    Vector3f* Normals ();
    const Vector3f* Normals () const;

    // color access
    ColorRGB& Color (int i);
    const ColorRGB& Color (int i) const;
    ColorRGB* Colors ();
    const ColorRGB* Colors () const;

    // base texture coordinate access
    Vector2f& Texture (int i);
    const Vector2f& Texture (int i) const;
    Vector2f* Textures ();
    const Vector2f* Textures () const;

    // additional texture coordinates to support multitexture
    Vector2f& Texture1 (int i);
    const Vector2f& Texture1 (int i) const;
    Vector2f* Textures1 ();
    const Vector2f* Textures1 () const;

    Vector2f& Texture2 (int i);
    const Vector2f& Texture2 (int i) const;
    Vector2f* Textures2 ();
    const Vector2f* Textures2 () const;

    Vector2f& Texture3 (int i);
    const Vector2f& Texture3 (int i) const;
    Vector2f* Textures3 ();
    const Vector2f* Textures3 () const;

    // support for bump mapping
    Vector2f& TextureBump (int i);
    const Vector2f& TextureBump (int i) const;
    Vector2f* TexturesBump ();
    const Vector2f* TexturesBump () const;
    ColorRGB*& LightVectors ();
    const ColorRGB* LightVectors () const;

    // bounding sphere access
    Bound& ModelBound ();
    const Bound& ModelBound () const;

    // render state access
    const RenderStatePtr* GetRenderStateArray () const;

    // shader state/object access (NULL, if none)
    void SetVertexShader (VertexShader* pkVertexShader);
    void SetPixelShader (PixelShader* pkPixelShader);

    // shader getters (should really only be used by Renderer)
    VertexShaderPtr GetVertexShader () const;
    PixelShaderPtr GetPixelShader () const;
    ShaderConstants* GetVertexShaderConstants () const;
    ShaderConstants* GetPixelShaderConstants () const;

    // helper function for constants
    ShaderConst* GetVertexConst (const char* acName);
    ShaderConst* GetPixelConst (const char* acName);

    // geometric updates
    virtual void UpdateModelBound ();
    virtual void UpdateWorldBound ();
    virtual void UpdateModelNormals ();

    // reallocate any nonnull arrays
    void Reconstruct (int iVertexQuantity);

    // The reconstruction allows you to change only individual arrays.  You
    // do this by passing in the current array pointers for any arrays you
    // want to preserve.
    void Reconstruct (int iVertexQuantity, Vector3f* akVertex,
        Vector3f* akNormal, ColorRGB* akColor, Vector2f* akTexture,
        Vector2f* akTexture1 = NULL, Vector2f* akTexture2 = NULL,
        Vector2f* akTexture3 = NULL, Vector2f* akTextureBump = NULL);

    // Picking support. This nested class exists only to maintain the
    // class-derivation chain that is parallel to the one whose base is
    // Spatial.
    class WML_ITEM PickRecord : public Spatial::PickRecord
    {
    protected:
        PickRecord (Geometry* pkObject, float fRayT);
    };

protected:
    // Construction and destruction.  Geometry accepts responsibility for
    // deleting the input arrays.
    Geometry (int iVertexQuantity, Vector3f* akVertex, Vector3f* akNormal,
        ColorRGB* akColor, Vector2f* akTexture, Vector2f* akTexture1 = NULL,
        Vector2f* akTexture2 = NULL, Vector2f* akTexture3 = NULL,
        Vector2f* akTextureBump = NULL, VertexShader* pkVertexShader = NULL,
        PixelShader* pkPixelShader = NULL);

    Geometry ();

    // render state update
    virtual void UpdateRenderState (RenderState::Stack* pkStack);

    // drawing
    virtual void Draw (Renderer& rkRenderer);

    // vertices and attributes
    int m_iVertexQuantity;
    Vector3f* m_akVertex;
    Vector3f* m_akNormal;

    // TO DO.  Vertex colors will become part of a VertexColor render effect
    // object and will be removed from Geometry.  The VertexColor object will
    // be attachable to a Geometry object.
    ColorRGB* m_akColor;

    // TO DO.  Texture coordinates will become part of Texture render effect
    // objects and will be removed from Geometry.  Each Texture object will
    // be attachable to a Geometry object.
    //
    // The extra texture coordinates support multitexturing.  For now, only
    // four texture images are allowed per Geometry object, limited by the
    // minimum of TextureState::MAX_TEXTURES (currently 4).
    Vector2f* m_akTexture;
    Vector2f* m_akTexture1;
    Vector2f* m_akTexture2;
    Vector2f* m_akTexture3;

    // TO DO.  The texture coordinates will become part of BumpMap render
    // effect objects and will be removed from Geometry.  The light vectors
    // are stored in m_akColor, but will become a separate array in the
    // BumpMap render effect object;
    Vector2f* m_akTextureBump;

    // model bound
    Bound m_kBound;

    // render state
    RenderStatePtr m_aspkState[RenderState::RS_MAX_STATE];

    // shader state/objects
    // The shaders can be shared between multiple objects and so they will
    // be smart pointers.
    VertexShaderPtr m_pkVertexShader;
    PixelShaderPtr m_pkPixelShader;
    // The shader constants are local to this object and so the pointers will
    // be handled explicitly.
    ShaderConstants* m_pkVertexShaderConsts;
    ShaderConstants* m_pkPixelShaderConsts;
};

WmlSmartPointer(Geometry);
WmlRegisterStream(Geometry);
#include "WmlGeometry.inl"

} // namespace Wml

#endif
