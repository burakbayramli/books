// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlRenderer.h"
#include "WmlNode.h"
#include "WmlPointLight.h"
#include "WmlSpotLight.h"
#include "WmlDirectionalLight.h"
#include "WmlScreenPolygon.h"
using namespace Wml;

WmlImplementRTTI(Renderer,Object);
WmlImplementStream(Renderer);

Renderer::List* Renderer::ms_pkRendererList = NULL;

//----------------------------------------------------------------------------
Renderer::Renderer (int iWidth, int iHeight)
    :
    m_kBackgroundColor(ColorRGB::WHITE)
{
    m_iX = 0;
    m_iY = 0;
    m_iWidth = iWidth;
    m_iHeight = iHeight;
    m_iQuantity = m_iWidth*m_iHeight;

    m_bCapMultitexture = false;
    m_bCapSpecularAfterTexture = false;
    m_bCapPlanarReflection = false;
    m_bCapPlanarShadow = false;
    m_bCapTextureClampToBorder = false;
    m_bCapTextureApplyAdd = false;
    m_bCapTextureApplyCombine = false;
    m_bCapTextureApplyCombineDot3 = false;
    m_bCapDot3BumpMapping = false;
    m_iCapVertShaderVersion = Shader::UNSUPPORTED;
    m_iCapPixShaderVersion = Shader::UNSUPPORTED;

    m_bOverrideState = false;
    m_bReverseCullState = false;
    m_bDrawingReflected = false;
    m_bOverrideAlphaState = false;
    m_bOverrideTextureState = false;
    m_eOverrideLightingMode = OLM_NONE;

    m_pkCurrentBumpMap = NULL;
    m_pkCurPixelShader = NULL;
    m_pkCurVertexShader = NULL;

    m_iTextureUnitRequested = 0;
    m_iMaxTextureUnits = 0;

    // add renderer to global list of renderers
    List* pkItem = new List;
    pkItem->m_pkRenderer = this;
    pkItem->m_pkNext = ms_pkRendererList;
    ms_pkRendererList = pkItem;
}
//----------------------------------------------------------------------------
Renderer::Renderer ()
    :
    m_kBackgroundColor(ColorRGB::WHITE)
{
    m_iX = 0;
    m_iY = 0;
    m_iWidth = 0;
    m_iHeight = 0;
    m_iQuantity = 0;

    m_pkCurPixelShader = NULL;
    m_pkCurVertexShader = NULL;

    // add renderer to global list of renderers
    List* pkItem = new List;
    pkItem->m_pkRenderer = this;
    pkItem->m_pkNext = ms_pkRendererList;
    ms_pkRendererList = pkItem;
}
//----------------------------------------------------------------------------
Renderer::~Renderer ()
{
    m_spkCamera = NULL;

    // remove renderer from global list of renderers
    List* pkList = ms_pkRendererList;
    List* pkPrev = NULL;
    for (/**/; pkList; pkPrev = pkList, pkList = pkList->m_pkNext)
    {
        if ( pkList->m_pkRenderer == this )
        {
            if ( pkPrev )
            {
                // renderer not at front of list
                pkPrev->m_pkNext = pkList->m_pkNext;
            }
            else
            {
                // render state at front of list
                assert( pkList == ms_pkRendererList );
                ms_pkRendererList = pkList->m_pkNext;
            }
            pkList->m_pkNext = NULL;
            delete pkList;
            break;
        }
    }
}
//----------------------------------------------------------------------------
void Renderer::Move (int iXPos, int iYPos)
{
    if ( iXPos >= 0 && iYPos >= 0 )
    {
        m_iX = iXPos;
        m_iY = iYPos;
    }
}
//----------------------------------------------------------------------------
void Renderer::Resize (int iWidth, int iHeight)
{
    if ( iWidth > 0 && iHeight > 0 )
    {
        m_iWidth = iWidth;
        m_iHeight = iHeight;
        m_iQuantity = m_iWidth*m_iHeight;
        if ( m_spkCamera )
            m_spkCamera->OnResize(iWidth,iHeight);
    }
}
//----------------------------------------------------------------------------
void Renderer::Reshape (int iStartX, int iStartY, int iEndX, int iEndY)
{
    int iWidth = iEndX - iStartX;
    int iHeight = iEndY - iStartY;
    if ( iWidth > 0 && iHeight > 0 )
    {
        m_iX = iStartX;
        m_iY = iStartY;
        m_iWidth = iWidth;
        m_iHeight = iHeight;
        m_iQuantity = m_iWidth*m_iHeight;
        if ( m_spkCamera )
            m_spkCamera->OnResize(iWidth,iHeight);
    }
}
//----------------------------------------------------------------------------
void Renderer::Activate ()
{
}
//----------------------------------------------------------------------------
void Renderer::Suspend ()
{
}
//----------------------------------------------------------------------------
void Renderer::Resume ()
{
}
//----------------------------------------------------------------------------
CameraPtr Renderer::SetCamera (Camera* pkCamera)
{
    CameraPtr spkSave = m_spkCamera;
    if ( spkSave )
        spkSave->m_bActive = false;
    m_spkCamera = pkCamera;
    if ( m_spkCamera )
        m_spkCamera->m_bActive = true;
    return spkSave;
}
//----------------------------------------------------------------------------
void Renderer::Draw (Node* pkScene)
{
    if ( pkScene )
        pkScene->OnDraw(*this);
}
//----------------------------------------------------------------------------
void Renderer::Draw (ScreenPolygon* pkPolygon)
{
    if ( pkPolygon )
        pkPolygon->OnDraw(*this);
}
//----------------------------------------------------------------------------
void Renderer::InitializeState ()
{
    if ( m_bOverrideState )
        return;

    RenderState** apkState = RenderState::GetDefaultStates();

    RenderState* pkState;

    if ( !m_bOverrideAlphaState )
    {
        pkState = apkState[RenderState::RS_ALPHA];
        SetAlphaState((AlphaState*)pkState);
    }

    pkState = apkState[RenderState::RS_CULL];
    SetCullState((CullState*)pkState);

    pkState = apkState[RenderState::RS_DITHER];
    SetDitherState((DitherState*)pkState);

    pkState = apkState[RenderState::RS_FOG];
    SetFogState((FogState*)pkState);

    pkState = apkState[RenderState::RS_LIGHT];
    SetLightState((LightState*)pkState);

    pkState = apkState[RenderState::RS_MATERIAL];
    SetMaterialState((MaterialState*)pkState);

    pkState = apkState[RenderState::RS_POLYGONOFFSET];
    SetPolygonOffsetState((PolygonOffsetState*)pkState);

    pkState = apkState[RenderState::RS_SHADE];
    SetShadeState((ShadeState*)pkState);

    if ( !m_bOverrideTextureState )
    {
        pkState = apkState[RenderState::RS_TEXTURE];
        SetTextureState((TextureState*)pkState);
    }

    pkState = apkState[RenderState::RS_VERTEXCOLOR];
    SetVertexColorState((VertexColorState*)pkState);

    pkState = apkState[RenderState::RS_WIREFRAME];
    SetWireframeState((WireframeState*)pkState);

    pkState = apkState[RenderState::RS_ZBUFFER];
    SetZBufferState((ZBufferState*)pkState);
}
//----------------------------------------------------------------------------
void Renderer::SetState (const RenderStatePtr aspkState[])
{
    if ( m_bOverrideState )
        return;

    RenderState* pkState;

    if ( !m_bOverrideAlphaState )
    {
        pkState = aspkState[RenderState::RS_ALPHA];
        SetAlphaState((AlphaState*)pkState);
    }

    pkState = aspkState[RenderState::RS_DITHER];
    SetDitherState((DitherState*)pkState);

    pkState = aspkState[RenderState::RS_CULL];
    SetCullState((CullState*)pkState);

    pkState = aspkState[RenderState::RS_FOG];
    SetFogState((FogState*)pkState);

    pkState = aspkState[RenderState::RS_LIGHT];
    SetLightState((LightState*)pkState);

    pkState = aspkState[RenderState::RS_MATERIAL];
    SetMaterialState((MaterialState*)pkState);

    pkState = aspkState[RenderState::RS_POLYGONOFFSET];
    SetPolygonOffsetState((PolygonOffsetState*)pkState);

    pkState = aspkState[RenderState::RS_SHADE];
    SetShadeState((ShadeState*)pkState);

    if ( !m_bOverrideTextureState )
    {
        pkState = aspkState[RenderState::RS_TEXTURE];
        SetTextureState((TextureState*)pkState);
    }

    pkState = aspkState[RenderState::RS_VERTEXCOLOR];
    SetVertexColorState((VertexColorState*)pkState);

    pkState = aspkState[RenderState::RS_WIREFRAME];
    SetWireframeState((WireframeState*)pkState);

    pkState = aspkState[RenderState::RS_ZBUFFER];
    SetZBufferState((ZBufferState*)pkState);
}
//----------------------------------------------------------------------------
Object* Renderer::GetObjectByName (const char* acName)
{
    Object* pkFound = Object::GetObjectByName(acName);
    if ( pkFound )
        return pkFound;

    if ( m_spkCamera )
    {
        pkFound = m_spkCamera->GetObjectByName(acName);
        if ( pkFound )
            return pkFound;
    }

    return 0;
}
//----------------------------------------------------------------------------
void Renderer::GetAllObjectsByName (const char* acName,
    std::vector<Object*>& rkObjects)
{
    Object::GetAllObjectsByName(acName,rkObjects);

    if ( m_spkCamera )
        m_spkCamera->GetAllObjectsByName(acName,rkObjects);
}
//----------------------------------------------------------------------------
int Renderer::RequestTextureUnit (int iUnit)
{
    if ( iUnit <= -1 )
    {
        // Tries to grab a texture unit for a RenderEffect, starting 
        // with the highest unit so it does not conflict with
        // model textures.
        iUnit = m_iMaxTextureUnits - 1;
        if ( iUnit >= TextureState::MAX_TEXTURES )
        {
            // TO DO.  The current MAX_TEXTURES is 4.  Trap this block of code
            // to see if we should increase the supported number.
            iUnit = TextureState::MAX_TEXTURES - 1;
        }

        // TO DO.  Render effects who are children will override parents, if
        // necessary, when all texture units are in use by the render effect.
        // However, if a render effect ends up requesting units that are used
        // by models that it renders, the render effect will not show up.
        // That behavior could be modified later by changing the "requested
        // bit vector" to a "reserved bit vector" and changing the appropriate
        // code in Wml<Renderer>TextureState.cpp.
        while ( iUnit > -1 && (m_iTextureUnitRequested & (1 << iUnit)) )
        {
            // try a lower unit
            iUnit--;
        }

        if ( iUnit > -1 )
        {
            // grab the unit
            m_iTextureUnitRequested |= (1 << iUnit);
        }
    }
    else
    {
        if ( iUnit >= TextureState::MAX_TEXTURES
        ||  (m_iTextureUnitRequested & (1 << iUnit)) )
        {
            // texture unit invalid or already in use
            return -1;
        }

        // grab the unit
        m_iTextureUnitRequested |= (1 << iUnit);
    }

    return iUnit;
}
//----------------------------------------------------------------------------
bool Renderer::TextureUnitRequested (int iUnit)
{
    // Check if a texture unit is requested (or used) to avoid performance
    // warning.
    return (m_iTextureUnitRequested & (1 << iUnit)) != 0;
}
//----------------------------------------------------------------------------
void Renderer::ReleaseTextureUnit (int iUnit)
{
    m_iTextureUnitRequested &= ~(1 << iUnit);
}
//----------------------------------------------------------------------------
void Renderer::OnDestroyTexture (Texture* pkTexture)
{
    List* pkItem = ms_pkRendererList;
    while ( pkItem )
    {
        // tell renderer to release the texture and associated resources
        pkItem->m_pkRenderer->ReleaseTexture(pkTexture);
        pkItem = pkItem->m_pkNext;
    }
}
//----------------------------------------------------------------------------
void Renderer::OnDestroyShader (Shader* pkShader)
{
    List* pkItem = ms_pkRendererList;
    while ( pkItem )
    {
        // tell renderer to release the shader and associated resources
        pkItem->m_pkRenderer->ReleaseShader(pkShader);
        pkItem = pkItem->m_pkNext;
    }    
}
//----------------------------------------------------------------------------
void Renderer::ReleaseTextures (Spatial* pkScene)
{
    TextureState* pkTS = WmlStaticCast(TextureState,
        pkScene->GetRenderState(RenderState::RS_TEXTURE));
    if ( pkTS )
    {
        for (int i = 0; i < pkTS->GetQuantity(); i++)
        {
            Texture* pkTexture = pkTS->Get(i);
            if ( pkTexture )
                ReleaseTexture(pkTexture);
        }
    }

    Node* pkNode = WmlDynamicCast(Node,pkScene);
    if ( pkNode )
    {
        for (int i = 0; i < pkNode->GetQuantity(); i++)
        {
            Spatial* pkChild = pkNode->GetChild(i);
            if ( pkChild )
                ReleaseTextures(pkChild);
        }
    }
}
//----------------------------------------------------------------------------
void Renderer::ReleaseShaders (Spatial* pkScene)
{
    Node* pkNode = WmlDynamicCast(Node,pkScene);

    if ( pkNode )
    {
       Geometry* pkGeom = WmlDynamicCast(Geometry, pkScene);
       if ( pkGeom )
        {
            if ( pkGeom->GetVertexShader() )
            {
                ReleaseShader( pkGeom->GetVertexShader() );
            }

            if ( pkGeom->GetPixelShader() )
            {
                ReleaseShader( pkGeom->GetPixelShader() );
            }
        }

        for (int i = 0; i < pkNode->GetQuantity(); i++)
        {
            Spatial* pkChild = pkNode->GetChild(i);
            if ( pkChild )
            {
                ReleaseShaders( pkChild );
            }
        }
    }
}
//----------------------------------------------------------------------------
void Renderer::SetStateConst (float* afData, const Geometry& rkGeom,
    StateConstantType iType, int iTypeNum)
{
    // This handles state that is not specific to the specific renderer.
    // *Renderer should call this function in their own SetStateConst function
    // to handle the rest.

    // If the user asks for some light which does not exist, then
    // it will throw an assert in debug mode and quietly fail with reasonable
    // data (black for colors, the origin for positions, and 
    // UNIT_X for directions, etc...) which shouldn't break any shaders.

    RenderState* pkState;
    Light* pkLight;

    switch( iType )
    {
        // things not handled here, but in the specific renderer
        case RENDERER_MODVIEW:
        case RENDERER_PROJ:
        case RENDERER_MODVIEWPROJ:
        case RENDERER_MOD:

        // Camera State
        case CAMERA_POSITION:
            memcpy(afData, &m_spkCamera->GetLocation(), 3*sizeof(float));
            afData[3] = 1.0f;
            break;
        case CAMERA_UP:
            memcpy(afData, &m_spkCamera->GetUp(), 3*sizeof(float));
            afData[3] = 0.0f;
            break;
        case CAMERA_LEFT:
            memcpy(afData, &m_spkCamera->GetLeft(), 3*sizeof(float));
            afData[3] = 0.0f;
            break;

        // Fog State
        case FOG_COLOR:
            pkState = rkGeom.GetRenderStateArray()[RenderState::RS_FOG];
            memcpy(afData,((FogState*)pkState)->Color(),3*sizeof(float));
            afData[3] = 1.0f;
            break;
        case FOG_PARAMS:
            pkState = rkGeom.GetRenderStateArray()[RenderState::RS_FOG];
            afData[0] = ((FogState*)pkState)->Start();
            afData[1] = ((FogState*)pkState)->End();
            afData[2] = ((FogState*)pkState)->Density();
            afData[3] = (float)((FogState*)pkState)->Enabled();
            break;

        // Material State
        case MATERIAL_EMISSIVE:
            pkState = rkGeom.GetRenderStateArray()[RenderState::RS_MATERIAL];
            memcpy(afData,((MaterialState*)pkState)->Emissive(),3*
                sizeof(float) );
            break;
        case MATERIAL_AMBIENT:
            pkState = rkGeom.GetRenderStateArray()[RenderState::RS_MATERIAL];
            memcpy(afData,((MaterialState*)pkState)->Ambient(),3*
                sizeof(float) );
            break;
        case MATERIAL_DIFFUSE:
            pkState = rkGeom.GetRenderStateArray()[RenderState::RS_MATERIAL];
            memcpy(afData,((MaterialState*)pkState)->Diffuse(),3*
                sizeof(float) );
            break;
        case MATERIAL_SPECULAR:
            pkState = rkGeom.GetRenderStateArray()[RenderState::RS_MATERIAL];
            memcpy(afData,((MaterialState*)pkState)->Specular(),3*
                sizeof(float) );
            break;
        case MATERIAL_SHININESS:
            pkState = rkGeom.GetRenderStateArray()[RenderState::RS_MATERIAL];
            afData[0] = ((MaterialState*)pkState)->Shininess();
            break;

        // Lights
        // Because it's hard (right now) to do conditional statements in
        // shader code, lights that are off will be supported by setting their
        // color to (0,0,0,1), so that they do not emit any light.  If the
        // shader code uses light colors unconventionally, this may
        // cause problems.  Otherwise, you can use the same shader code with
        // arbitrary lights that turn on and off.
        case LIGHT_POSITION:
            pkState = rkGeom.GetRenderStateArray()[RenderState::RS_LIGHT];
            pkLight = ((LightState*)pkState)->Get(iTypeNum);
            if ( !pkLight )
            {
                assert( !"No light to send position from" );
                // For release mode, quietly fail with reasonable data
                memcpy(afData,Vector3f::ZERO,3*sizeof(float));
                afData[3] = 1.0f;
            }
            else if ( pkLight->GetType() == Light::LT_POINT )
            {
                memcpy(afData,((PointLight*)pkLight)->Location(),3*
                    sizeof(float));
                afData[3] = 1.0f;
            }
            else if ( pkLight->GetType() == Light::LT_SPOT )
            {
                memcpy(afData,((SpotLight*)pkLight)->Location(),3*
                    sizeof(float));
                afData[3] = 1.0f;
            }
            else
            {
                assert( !"Light type does not have a position." );
                // For release mode, quietly fail with reasonable data
                memcpy(afData,Vector3f::ZERO,3*sizeof(float));
                afData[3] = 1.0f;
            }
            break;
        case LIGHT_DIRECTION:
            pkState = rkGeom.GetRenderStateArray()[RenderState::RS_LIGHT];
            pkLight = ((LightState*)pkState)->Get(iTypeNum);
            if ( !pkLight )
            {
                assert( !"No light to send direction from" );
                // For release mode, quietly fail with reasonable data
                memcpy(&afData[1],Vector3f::ZERO,3*sizeof(float));
                afData[0] = 1.0f;         
            }
            else if ( pkLight->GetType() == Light::LT_SPOT )
            {
                memcpy(afData,((SpotLight*)pkLight)->Direction(),3*
                    sizeof(float));
                afData[3] = 0.0f;
            }
            else if ( pkLight->GetType() == Light::LT_DIRECTIONAL )
            {
                 memcpy(afData,((DirectionalLight*)pkLight)->Direction(),3*
                    sizeof(float));
                 afData[3] = 0.0f;
            }
            else
            {
                assert( !"Light type does not have a direction." );
                // For release mode, quietly fail with reasonable data
                memcpy(&afData[1],Vector3f::ZERO,3*sizeof(float));
                afData[0] = 1.0f;  
            }
            break;
        case LIGHT_AMBIENT:
            pkState = rkGeom.GetRenderStateArray()[RenderState::RS_LIGHT];
            pkLight = ((LightState*)pkState)->Get(iTypeNum);
            if ( !pkLight )
            {
                assert( !"Light doesn't exist to send ambient from." );
                // In release mode, quietly fail with no color
                memcpy(afData,&ColorRGB::BLACK,3*sizeof(float) );
                afData[3] = 1.0f;
            }
            else if ( pkLight->On() )
            {
                memcpy(afData,pkLight->Ambient(),3*sizeof(float) );
                afData[3] = 1.0f;
            }
            else
            {
                memcpy(afData,&ColorRGB::BLACK,3*sizeof(float) );
                afData[3] = 1.0f;
            }
            break;
        case LIGHT_DIFFUSE:
            pkState = rkGeom.GetRenderStateArray()[RenderState::RS_LIGHT];
            pkLight = ((LightState*)pkState)->Get(iTypeNum);
            if ( !pkLight )
            {
                assert( !"Light doesn't exist to send diffuse from." );
                // In release mode, quietly fail with no color
                memcpy(afData,&ColorRGB::BLACK,3*sizeof(float) );
                afData[3] = 1.0f;
            }
            else if ( pkLight->On() )
            {
                memcpy(afData,pkLight->Diffuse(),3*sizeof(float) );
                afData[3] = 1.0f;
            }
            else
            {
                memcpy(afData,&ColorRGB::BLACK,3*sizeof(float) );
                afData[3] = 1.0f;
            }
            break;
        case LIGHT_SPECULAR:
            pkState = rkGeom.GetRenderStateArray()[RenderState::RS_LIGHT];
            pkLight = ((LightState*)pkState)->Get(iTypeNum);
            if ( !pkLight )
            {
                assert( !"Light doesn't exist to send specular from." );
                // In release mode, quietly fail with no color
                memcpy(afData,&ColorRGB::BLACK,3*sizeof(float) );
                afData[3] = 1.0f;
            }
            else if ( pkLight->On() )
            {
                memcpy(afData,pkLight->Specular(),3*sizeof(float) );
                afData[3] = 1.0f;
            }
            else
            {
                memcpy(afData,&ColorRGB::BLACK,3*sizeof(float) );
                afData[3] = 1.0f;
            }
            break;
        case LIGHT_SPOTCUTOFF:
            pkState = rkGeom.GetRenderStateArray()[RenderState::RS_LIGHT];
            pkLight = ((LightState*)pkState)->Get(iTypeNum);
            if ( !pkLight )
            {
                assert( !"Light doesn't exist to send cut-off from." );
                // Release mode reasonable data
                afData[0] = 0.0f;
                afData[1] = 1.0f;
                afData[2] = 0.0f;
                afData[3] = 1.0f;
            }
            if ( pkLight->GetType() == Light::LT_SPOT )
            {
                afData[0] = ((SpotLight*)pkLight)->GetAngle();
                afData[1] = Mathf::Cos( afData[0] );
                afData[2] = Mathf::Sin( afData[1] );
                afData[3] = ((SpotLight*)pkLight)->Exponent();
            }
            else
            {
                assert( !"Not a spotlight!" );
                // Release mode reasonable data
                afData[0] = 0.0f;
                afData[1] = 1.0f;
                afData[2] = 0.0f;
                afData[3] = 1.0f;
            }
            break;
        case LIGHT_ATTENPARAMS:
            pkState = rkGeom.GetRenderStateArray()[RenderState::RS_LIGHT];
            pkLight = ((LightState*)pkState)->Get(iTypeNum);
            if ( !pkLight )
            {
                afData[0] = 0.0f;
                afData[1] = 0.0f;
                afData[2] = 0.0f;
                afData[3] = 0.0f;
            }
            else
            {
                afData[0] = pkLight->Constant();
                afData[1] = pkLight->Linear();
                afData[2] = pkLight->Quadratic();
                afData[3] = pkLight->Intensity();
            }
            break;

        case USER_DEFINED:
        default:
            // do nothing
            break;
    };
}
//----------------------------------------------------------------------------
void Renderer::SetShaderState (const Geometry* pkGeom)
{
    if ( !pkGeom )
    {
        SetCurrentVertexShader(NULL);
        SetCurrentPixelShader(NULL);
        return;
    }
    
    SetCurrentVertexShader(pkGeom->GetVertexShader());
    SetCurrentPixelShader(pkGeom->GetPixelShader());
}
//----------------------------------------------------------------------------
void Renderer::SetShaderConstants (const Geometry* pkGeom)
{
    if ( !pkGeom )
        return;

    if ( m_pkCurVertexShader != pkGeom->GetVertexShader() )
    {
        // Your graphics card does not support vertex shaders.
        return;
    }

    if ( m_pkCurPixelShader != pkGeom->GetPixelShader() )
    {
        // Your graphics card does not support pixel shaders.
        return;
    }

    if ( m_pkCurVertexShader )
        SetVertexShaderConst(pkGeom->GetVertexShaderConstants(),*pkGeom);

    if ( m_pkCurPixelShader )
        SetPixelShaderConst(pkGeom->GetPixelShaderConstants(),*pkGeom);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* Renderer::Factory (Stream&)
{
    // Renderer is abstract, Factory never called.  Renderers cannot be
    // streamed anyway...
    return NULL;
}
//----------------------------------------------------------------------------
void Renderer::Load (Stream&, Stream::Link*)
{
    // Renderers cannot be streamed.
    assert( false );
}
//----------------------------------------------------------------------------
void Renderer::Link (Stream&, Stream::Link*)
{
    // Renderers cannot be streamed.
    assert( false );
}
//----------------------------------------------------------------------------
bool Renderer::Register (Stream&)
{
    // Renderers cannot be streamed.
    assert( false );
    return false;
}
//----------------------------------------------------------------------------
void Renderer::Save (Stream&)
{
    // Renderers cannot be streamed.
    assert( false );
}
//----------------------------------------------------------------------------
StringTree* Renderer::SaveStrings ()
{
    StringTree* pkTree = new StringTree(4,0,2,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("width =",m_iWidth));
    pkTree->SetString(2,MakeString("height =",m_iHeight));
    pkTree->SetString(3,MakeString("background color =",
        m_kBackgroundColor));

    // children
    pkTree->SetChild(0,Object::SaveStrings());
    pkTree->SetChild(1,m_spkCamera->SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int Renderer::GetMemoryUsed () const
{
    int iBaseSize = sizeof(Renderer) - sizeof(Object);
    int iTotalSize = iBaseSize + Object::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int Renderer::GetDiskUsed () const
{
    // Renderers cannot be streamed.
    assert( false );
    return 0;
}
//----------------------------------------------------------------------------
