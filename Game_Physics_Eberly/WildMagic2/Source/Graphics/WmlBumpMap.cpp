// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBumpMap.h"
#include "WmlDirectionalLight.h"
#include "WmlPointLight.h"
#include "WmlRenderer.h"
#include "WmlTriMesh.h"
using namespace Wml;

WmlImplementRTTI(BumpMap,Node);
WmlImplementStream(BumpMap);

//----------------------------------------------------------------------------
BumpMap::BumpMap (Node* pkObjects, Texture* pkNormalMap, Light* pkLight,
    bool bModulate)
    :
    m_spkObjects(pkObjects),
    m_spkNormalMap(pkNormalMap), 
    m_spkLight(pkLight)
{
    assert( pkObjects && pkNormalMap && pkLight );

    m_bModulate = bModulate;

    // Save the cull state of the objects.  These objects will be force-culled
    // by 'this' to make sure they are not drawn by the regular traversal.
    m_bForceCullObjects = pkObjects->ForceCull();
    pkObjects->ForceCull() = true;
    
    // create texture states
    m_spkTextureState = new TextureState;
    m_spkTextureStateModulated = new TextureState;

    // set apply mode for normal map
    m_spkNormalMap->Apply() = Texture::AM_COMBINE;
    m_spkNormalMap->CombineFuncRGB() = Texture::ACF_DOT3_RGB;
    m_spkNormalMap->CombineSrc0RGB() = Texture::ACS_TEXTURE;
    m_spkNormalMap->CombineOp0RGB() = Texture::ACO_SRC_COLOR;
    m_spkNormalMap->CombineSrc1RGB() = Texture::ACS_PRIMARY_COLOR;
    m_spkNormalMap->CombineOp1RGB() = Texture::ACO_SRC_COLOR;

    // set up the modulated and unmodulated texture states
    m_spkTextureState->Set(0,m_spkNormalMap);
    m_spkTextureStateModulated->Set(0,m_spkNormalMap);
    
    // The blend color can be modified by the application using the function
    // SetCurrentDiffuseMaterial.
    Texture* pkTexture = new Texture;
    pkTexture->Apply() = Texture::AM_COMBINE;
    pkTexture->CombineFuncRGB() = Texture::ACF_MODULATE;
    pkTexture->CombineSrc0RGB() = Texture::ACS_CONSTANT;
    pkTexture->CombineOp0RGB() = Texture::ACO_SRC_COLOR;
    pkTexture->CombineSrc1RGB() = Texture::ACS_PREVIOUS;
    pkTexture->CombineOp1RGB() = Texture::ACO_SRC_COLOR;
    pkTexture->BlendColor() = pkLight->Diffuse();
    m_spkTextureStateModulated->Set(1,pkTexture);

    // The blend color can be modified by the application using the function
    // SetCurrentAmbientMaterial.
    pkTexture = new Texture;
    pkTexture->Apply() = Texture::AM_COMBINE;
    pkTexture->CombineFuncRGB() = Texture::ACF_ADD;
    pkTexture->CombineSrc0RGB() = Texture::ACS_PREVIOUS;
    pkTexture->CombineOp0RGB() = Texture::ACO_SRC_COLOR;
    pkTexture->CombineSrc1RGB() = Texture::ACS_CONSTANT;
    pkTexture->CombineOp1RGB() = Texture::ACO_SRC_COLOR;
    pkTexture->BlendColor() = pkLight->Ambient();
    m_spkTextureStateModulated->Set(2,pkTexture);

    // create an alpha blending state
    m_spkAlphaState = new AlphaState;
    m_spkAlphaState->BlendEnabled() = true;
    m_spkAlphaState->TestEnabled() = false;
    m_spkAlphaState->SrcBlend() = AlphaState::SBF_DST_COLOR;
    m_spkAlphaState->DstBlend() = AlphaState::DBF_ZERO;
}
//----------------------------------------------------------------------------
BumpMap::BumpMap ()
{
    m_bModulate = false;
    m_bForceCullObjects = false;
}
//----------------------------------------------------------------------------
BumpMap::~BumpMap()
{
    // restore the cull state
    m_spkObjects->ForceCull() = m_bForceCullObjects;

    // release the render effect objects
    m_spkObjects = NULL;
    m_spkNormalMap = NULL;
    m_spkTextureState = NULL;
    m_spkTextureStateModulated = NULL;
    m_spkAlphaState = NULL;
    m_spkLight = NULL;
}
//----------------------------------------------------------------------------
void BumpMap::ComputeLightVectors (TriMesh& rkMesh)
{
    // Generate light direction vectors in the surface local space and store
    // them in the trimesh for interpolation via the rasterizer.  This assumes
    // the user has provided normalized normals.
    if ( !rkMesh.Normals() || !rkMesh.Textures() )
        return;

    assert( rkMesh.Colors() );

    // get the world light vector
    Vector3f kWLight;
    switch ( m_spkLight->GetType() )
    {
    case Light::LT_DIRECTIONAL:
        kWLight = -WmlStaticCast(DirectionalLight,m_spkLight)->Direction();
        break;
    case Light::LT_POINT:
    case Light::LT_SPOT:
        kWLight = WmlStaticCast(PointLight,m_spkLight)->Location();
        break;
    default:
        // ambient light, nothing we can do to handle this
        return;
    }

    // transform the world light vector into model space
    Vector3f kMLight = kWLight - rkMesh.WorldTranslate();
    kMLight = kMLight*rkMesh.WorldRotate();
    kMLight /= rkMesh.WorldScale();

    // The surface local space information is computed for all the vertices.
    // We iterate over the triangles because we need to know the connectivity
    // information.  The end result is assignment of the light vectors.
    Vector3f* akVertex = rkMesh.Vertices();
    Vector3f* akNormal = rkMesh.Normals();
    Vector2f* akUV = rkMesh.Textures();

    // Set the light vectors to (0,0,0) as a flag that the quantity has not
    // yet been computed.  The probability that a light vector is actually
    // (0,0,0) should be small, so the flag system should save computation
    // time overall.
    ColorRGB* akLVec = rkMesh.Colors();
    memset(akLVec,0,rkMesh.GetVertexQuantity()*sizeof(ColorRGB));

    for (int iT = 0; iT < rkMesh.GetTriangleQuantity(); iT++)
    {
        // get the triangle vertices and attributes
        int aiIndex[3];
        rkMesh.GetTriangle(iT,aiIndex[0],aiIndex[1],aiIndex[2]);
        
        Vector3f* apkV[3] =
        {
            &akVertex[aiIndex[0]],
            &akVertex[aiIndex[1]],
            &akVertex[aiIndex[2]]
        };

        Vector3f* apkN[3] =
        {
            &akNormal[aiIndex[0]],
            &akNormal[aiIndex[1]],
            &akNormal[aiIndex[2]]
        };

        Vector2f* apkUV[3] =
        {
            &akUV[aiIndex[0]],
            &akUV[aiIndex[1]],
            &akUV[aiIndex[2]]
        };

        for (int i = 0; i < 3; i++)
        {
            ColorRGB& rkColor = akLVec[aiIndex[i]];
            if ( rkColor != ColorRGB::BLACK )
                continue;

            // Compute the surface local space at each vertex.
            //
            // TO DO.  If the geometry is static in model space, then we
            // should precompute the surface local space and store it.
            //
            // The normal vector N is the surface normal.  The vertex normal
            // is used as an approximation to N.
            //
            // The tangent vector T is computed by thinking of the surface in
            // parametric form P(u,v) for some scalar variables u and v.  In
            // this case, a tangent is the partial derivative T = dP/du.  This
            // quantity is estimated from the triangle attributes.  The
            // texture coordinates (u,v) are used as an approximation to the
            // parametric quantities.
            //
            // The binormal vector is B = Cross(N,T).  If the estimation of
            // T is ill-posed, we try BLAH BLAH BLAH.
            
            //The tangent T is defined
            // by dSurf/dS, and the binormal B = Cross(N,T).  We assume the
            // normals are normalized,  and we assume the model is
            // parametrized so that the square patch assumption holds.
            //
            // We need to compute the tangent vector at the current vertex.
            // If we think of the surface as being a vector-valued function
            // P(u,v), then we want to set T to be dP/du.  We try to compute
            // T, but if the parametrization is such that our direct
            // definition of T is not applicable, then we try for the binormal
            // directly and then back out a reasonable T.  If there is no
            // useful parametrization information, we just assume that the
            // light vector is in the same direction as the surface normal.

            int iP = (i == 0) ? 2 : i - 1;
            int iN = (i + 1) % 3;

            // compute edge vectors at the specified vertex
            Vector3f kDP1 = *apkV[iN] - *apkV[i];
            Vector3f kDP2 = *apkV[iP] - *apkV[i];

            // estimate a tangent surface vector
            Vector3f kTangent;
            bool fDegenerate = false;
            const float fEpsilon = 1e-05f;

            if ( Mathf::FAbs(kDP1.Length()) < fEpsilon
            ||   Mathf::FAbs(kDP2.Length()) < fEpsilon )
            {
                // The triangle is very small, call it degenerate.
                fDegenerate = true;
            }
            else
            {
                // difference of surface parameters along triangle edge
                float fDU1 = apkUV[iN]->X() - apkUV[i]->X();
                float fDV1 = apkUV[iN]->Y() - apkUV[i]->Y();

                if ( Mathf::FAbs(fDV1) < fEpsilon )
                {
                    // The triangle effectively has no variation in the v
                    // texture coordinate.
                    if ( Mathf::FAbs(fDU1) < fEpsilon )
                    {
                        // The triangle effectively has no variation in the u
                        // coordinate.  Since the texture coordinates do not
                        // effectively vary on this triangle, treat it as a
                        // degenerate parametric surface.
                        fDegenerate = true;
                    }
                    else
                    {
                        // The variation is effectively all in u, so set the
                        // tangent T = dP/du.
                        kTangent = kDP1/fDU1;
                    }
                }
                else
                {
                    // difference of surface parameters along triangle edge
                    float fDU2 = apkUV[iP]->X() - apkUV[i]->X();
                    float fDV2 = apkUV[iP]->Y() - apkUV[i]->Y();
                    float fDet = fDV1*fDU2 - fDV2*fDU1;

                    if ( Mathf::FAbs(fDet) >= fEpsilon )
                    {
                        // The triangle vertices form three collinear points
                        // in parameter space, so
                        //   dP/du = (dv1*dP2-dv2*dP1)/(dv1*du2-dv2*du1)
                        kTangent = (fDV1*kDP2-fDV2*kDP1)/fDet;
                    }
                    else
                    {
                        // The triangle vertices are collinear in parameter
                        // space.
                        fDegenerate = true;
                    }
                }
            }

            if ( fDegenerate )
            {
                // The texture coordinate mapping is not properly defined for
                // this.  Just say that the tangent space light vector points
                // in the same direction as the surface normal.
                rkColor.r = 0.0f;
                rkColor.g = 0.0f;
                rkColor.b = 1.0f;
                continue;
            }

            // Project T into the tangent plane by projecting out the surface
            // normal, then make it unit length.
            kTangent -= apkN[i]->Dot(kTangent)*(*apkN[i]);
            kTangent.Normalize();

            // compute the binormal B, another tangent perpendicular to T
            Vector3f kBinormal = apkN[i]->UnitCross(kTangent);

            // When generating bump/normal maps, folks usually work in a
            // left-handed screen space with the origin at the upper right,
            // u to the right, and v down, while we apply the textures with
            // the origin at the lower left, u right, v up, so we need to flip
            // the binormal (v-axis) to get a proper transformation to the
            // surface local texture space.
            Matrix3f kRotTS(
                kTangent.X(),   kTangent.Y(),   kTangent.Z(),
              -kBinormal.X(), -kBinormal.Y(), -kBinormal.Z(),
                apkN[i]->X(),   apkN[i]->Y(),   apkN[i]->Z());

            // Compute the tangent space light vector.  Conceptually
            //   kTSLight = kRotTS*(kMLight-Vertex[i])/|kMLight-Vertex[i]|
            Vector3f kTSLight = kMLight;

            // Subtract off the vertex position if we have a positional light.
            if ( m_spkLight->GetType() != Light::LT_DIRECTIONAL )
                kTSLight -= *apkV[i];

            kTSLight.Normalize();
            kTSLight = kRotTS*kTSLight;

            // Transform the light vector into [0,1]^3 to make it a valid
            // ColorRGB object.
            rkColor.r = 0.5f*(kTSLight.X() + 1.0f);
            rkColor.g = 0.5f*(kTSLight.Y() + 1.0f);
            rkColor.b = 0.5f*(kTSLight.Z() + 1.0f);
        }
    }
}
//----------------------------------------------------------------------------
void BumpMap::UpdateWorldBound()
{
    m_kWorldBound = m_spkObjects->WorldBound();
}
//----------------------------------------------------------------------------
void BumpMap::Draw (Renderer& rkRenderer)
{
    m_spkObjects->ForceCull() = m_bForceCullObjects;
    rkRenderer.Draw(*this);
    m_spkObjects->ForceCull() = true;
}
//----------------------------------------------------------------------------
void BumpMap::SetCurrentAmbientMaterial (const ColorRGB& rkColor)
{
    // This is used to capture material changes for bump mapping.  Materials
    // are implemented by using additional texture units with a constant color
    // as one of the inputs for a texture add.
    m_spkTextureStateModulated->Get(2)->BlendColor() =
        rkColor*m_spkLight->Ambient();
}
//----------------------------------------------------------------------------
void BumpMap::SetCurrentDiffuseMaterial (const ColorRGB& rkColor,
    int iMaxTextureUnits)
{
    // This is used to capture material changes for bump mapping.  Materials
    // are implemented by using additional texture units with a constant color
    // as one of the inputs for a texture modulate.  If there are enough
    // texture units, then ambient is added.  If not, then it gets modulated
    // by L.N.
    if ( iMaxTextureUnits > 2 )
    {
        m_spkTextureStateModulated->Get(1)->BlendColor() = rkColor * 
            m_spkLight->Diffuse();
    }
    else
    {
        // If we only have 2 texture units, then ambient gets modulated by L.N
        // along with diffuse.  Ambient must be set before diffuse for this to
        // work transparently.  This is done automatically inside
        // WmlLightState.cpp.
        m_spkTextureStateModulated->Get(1)->BlendColor() =
            rkColor*m_spkLight->Diffuse() +
            m_spkTextureStateModulated->Get(2)->BlendColor();
    }       
}
//----------------------------------------------------------------------------
Object* BumpMap::GetObjectByName (const char* acName)
{
    Object* pkFound = Node::GetObjectByName(acName);
    if ( pkFound )
        return pkFound;

    // The subtree m_spkObjects is not searched to avoid the possibility of
    // infinite recursion.

    if ( m_spkNormalMap )
    {
        pkFound = m_spkNormalMap->GetObjectByName(acName);
        if ( pkFound )
            return pkFound;
    }

    if ( m_spkTextureState )
    {
        pkFound = m_spkTextureState->GetObjectByName(acName);
        if ( pkFound )
            return pkFound;
    }

    if ( m_spkTextureStateModulated )
    {
        pkFound = m_spkTextureStateModulated->GetObjectByName(acName);
        if ( pkFound )
            return pkFound;
    }

    if ( m_spkLight )
    {
        pkFound = m_spkLight->GetObjectByName(acName);
        if ( pkFound )
            return pkFound;
    }

    if ( m_spkAlphaState )
    {
        pkFound = m_spkAlphaState->GetObjectByName(acName);
        if ( pkFound )
            return pkFound;
    }

    return NULL;
}
//----------------------------------------------------------------------------
void BumpMap::GetAllObjectsByName (const char* acName,
    std::vector<Object*>& rkObjects)
{
    Node::GetAllObjectsByName(acName,rkObjects);

    // The subtree m_spkObjects is not searched to avoid the possibility of
    // infinite recursion.

    if ( m_spkNormalMap )
        m_spkNormalMap->GetAllObjectsByName(acName,rkObjects);

    if ( m_spkTextureState )
        m_spkTextureState->GetAllObjectsByName(acName,rkObjects);

    if ( m_spkTextureStateModulated )
        m_spkTextureStateModulated->GetAllObjectsByName(acName,rkObjects);

    if ( m_spkLight )
        m_spkLight->GetAllObjectsByName(acName,rkObjects);

    if ( m_spkAlphaState )
        m_spkAlphaState->GetAllObjectsByName(acName,rkObjects);
}
//----------------------------------------------------------------------------


//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* BumpMap::Factory (Stream& rkStream)
{
    BumpMap* pkObject = new BumpMap;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void BumpMap::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Node::Load(rkStream,pkLink);

    // native data
    StreamReadBool(rkStream,m_bModulate);

    // link data (objects, normal map, texture state, modulated texture
    // state, light state, alpha state, in that order)
    Spatial* pkChild;
    StreamRead(rkStream,pkChild);
    pkLink->Add(pkChild);
    StreamRead(rkStream,pkChild);
    pkLink->Add(pkChild);
    StreamRead(rkStream,pkChild);
    pkLink->Add(pkChild);
    StreamRead(rkStream,pkChild);
    pkLink->Add(pkChild);
    StreamRead(rkStream,pkChild);
    pkLink->Add(pkChild);
    StreamRead(rkStream,pkChild);
    pkLink->Add(pkChild);
}
//----------------------------------------------------------------------------
void BumpMap::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Node::Link(rkStream,pkLink);

    Object* pkLinkID = pkLink->GetLinkID();
    m_spkObjects = (Node*)rkStream.GetFromMap(pkLinkID);

    pkLinkID = pkLink->GetLinkID();
    m_spkNormalMap = (Texture*)rkStream.GetFromMap(pkLinkID);

    pkLinkID = pkLink->GetLinkID();
    m_spkTextureState = (TextureState*)rkStream.GetFromMap(pkLinkID);

    pkLinkID = pkLink->GetLinkID();
    m_spkTextureStateModulated = (TextureState*)rkStream.GetFromMap(pkLinkID);

    pkLinkID = pkLink->GetLinkID();
    m_spkLight = (Light*)rkStream.GetFromMap(pkLinkID);

    pkLinkID = pkLink->GetLinkID();
    m_spkAlphaState = (AlphaState*)rkStream.GetFromMap(pkLinkID);

    // initialize the cull status for correct toggling during rendering
    m_bForceCullObjects = m_spkObjects->ForceCull();
    m_spkObjects->ForceCull() = true;
}
//----------------------------------------------------------------------------
bool BumpMap::Register (Stream& rkStream)
{
    if ( !Node::Register(rkStream) )
        return false;

    if ( m_spkObjects )
        m_spkObjects->Register(rkStream);

    if ( m_spkNormalMap )
        m_spkNormalMap->Register(rkStream);

    if ( m_spkTextureState )
        m_spkTextureState->Register(rkStream);

    if ( m_spkTextureStateModulated )
        m_spkTextureStateModulated->Register(rkStream);

    if ( m_spkLight )
        m_spkLight->Register(rkStream);

    if ( m_spkAlphaState )
        m_spkAlphaState->Register(rkStream);

    return true;
}
//----------------------------------------------------------------------------
void BumpMap::Save (Stream& rkStream)
{
    Node::Save(rkStream);

    // native data
    StreamWriteBool(rkStream,m_bModulate);

    // link data
    StreamWrite(rkStream,m_spkObjects);
    StreamWrite(rkStream,m_spkNormalMap);
    StreamWrite(rkStream,m_spkTextureState);
    StreamWrite(rkStream,m_spkTextureStateModulated);
    StreamWrite(rkStream,m_spkLight);
    StreamWrite(rkStream,m_spkAlphaState);
}
//----------------------------------------------------------------------------
StringTree* BumpMap::SaveStrings ()
{
    int iCQuantity = 1;
    if ( m_spkObjects )
        iCQuantity++;
    if ( m_spkNormalMap )
        iCQuantity++;
    if ( m_spkTextureState )
        iCQuantity++;
    if ( m_spkTextureStateModulated )
        iCQuantity++;
    if ( m_spkLight )
        iCQuantity++;
    if ( m_spkAlphaState )
        iCQuantity++;

    StringTree* pkTree = new StringTree(2,0,iCQuantity,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("modulated =",m_bModulate));

    // children
    pkTree->SetChild(0,Node::SaveStrings());
    int iSlot = 1;
    if ( m_spkObjects )
        pkTree->SetChild(iSlot++,m_spkObjects->SaveStrings());
    if ( m_spkNormalMap )
        pkTree->SetChild(iSlot++,m_spkNormalMap->SaveStrings());
    if ( m_spkTextureState )
        pkTree->SetChild(iSlot++,m_spkTextureState->SaveStrings());
    if ( m_spkTextureStateModulated )
        pkTree->SetChild(iSlot++,m_spkTextureStateModulated->SaveStrings());
    if ( m_spkLight )
        pkTree->SetChild(iSlot++,m_spkLight->SaveStrings());
    if ( m_spkAlphaState )
        pkTree->SetChild(iSlot++,m_spkAlphaState->SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int BumpMap::GetMemoryUsed () const
{
    int iBaseSize = sizeof(BumpMap) - sizeof(Node);
    int iTotalSize = iBaseSize + Node::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int BumpMap::GetDiskUsed () const
{
    return Node::GetDiskUsed() +
        sizeof(m_bModulate) +
        sizeof(m_spkObjects) +
        sizeof(m_spkNormalMap) +
        sizeof(m_spkTextureState) +
        sizeof(m_spkTextureStateModulated) +
        sizeof(m_spkLight) +
        sizeof(m_spkAlphaState);
}
//----------------------------------------------------------------------------
