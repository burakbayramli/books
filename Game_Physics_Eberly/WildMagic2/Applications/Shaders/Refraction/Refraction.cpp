// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "Refraction.h"
#include "WmlVector3.h"

Refraction g_kTheApp;

//----------------------------------------------------------------------------
Refraction::Refraction ()
    :
    Application("Refraction",0,0,480,480,ColorRGB(0.5f,0.0f,1.0f))
{
    m_bInitialized = false;
}
//----------------------------------------------------------------------------
TriMesh* Refraction::CreateSquare (float fSize, float fDepth)
{
    Vector3f* akVertex = new Vector3f[4];
    akVertex[0] = Vector3f(fDepth, -fSize, -fSize);
    akVertex[1] = Vector3f(fDepth, -fSize, fSize);
    akVertex[2] = Vector3f(fDepth, fSize, fSize);
    akVertex[3] = Vector3f(fDepth, fSize, -fSize);

    Vector3f* akNormal = new Vector3f[4];
    akNormal[0] = -Vector3f::UNIT_X;
    akNormal[1] = -Vector3f::UNIT_X;
    akNormal[2] = -Vector3f::UNIT_X;
    akNormal[3] = -Vector3f::UNIT_X;

    Vector2f* akUV0 = new Vector2f[4];
    akUV0[0] = Vector2f(1.0f,1.0f);
    akUV0[1] = Vector2f(1.0f,0.0f);
    akUV0[2] = Vector2f(0.0f,0.0f);
    akUV0[3] = Vector2f(0.0f,1.0f);

    int* aiConnect = new int[6];
    aiConnect[0] = 0;  aiConnect[1] = 1; aiConnect[2] = 3;
    aiConnect[3] = 3;  aiConnect[4] = 1; aiConnect[5] = 2;

    return new TriMesh(4,akVertex,akNormal,NULL,akUV0,2,aiConnect,NULL,NULL,
        NULL,NULL);
}
//----------------------------------------------------------------------------
bool Refraction::Setup ()
{
    Stream kStream;
    bool bLoaded = kStream.Load("Face.mgc");
    if ( !bLoaded )
        return false;

    m_spkScene = (Node*) kStream.GetObjectAt(0);
    m_spkTriMesh = WmlSmartPointerCast(TriMesh,
        m_spkScene->GetChild(0));

    Image* pkEnvMap = Image::Load("tiles2-sphere.mif");
    if ( !pkEnvMap )
        return false;

    Texture* pkEnvTexture = new Texture;
    pkEnvTexture->SetImage(pkEnvMap);
    pkEnvTexture->Filter() = Texture::FM_LINEAR;
    pkEnvTexture->Mipmap() = Texture::MM_NONE;
    pkEnvTexture->Apply() = Texture::AM_DECAL;
    pkEnvTexture->Envmap() = Texture::EM_SPHERE;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkEnvTexture);

    Image* pkReflectMap = Image::Load("tiles-sphere.mif");
    if ( !pkReflectMap )
        return false;

    Texture* pkRefTexture = new Texture;
    pkRefTexture->SetImage(pkReflectMap);
    pkRefTexture->Filter() = Texture::FM_LINEAR;
    pkRefTexture->Mipmap() = Texture::MM_NONE;
    pkRefTexture->Apply() = Texture::AM_DECAL;
    pkTS->Set(1,pkRefTexture);

    m_spkTriMesh->SetVertexShader(m_spkVertShader);
    m_spkTriMesh->SetRenderState( pkTS );
    m_spkTriMesh->SetPixelShader(m_spkPixShader);

    Image* pkBackground = Image::Load("tiles2.mif");
    if ( !pkBackground )
        return false;

    Texture* pkBackTex = new Texture;
    pkBackTex->SetImage(pkBackground);
    pkBackTex->Filter() = Texture::FM_LINEAR;
    pkBackTex->Mipmap() = Texture::MM_NONE;
    pkBackTex->Apply() = Texture::AM_DECAL;
    TextureState* pkTS2 = new TextureState;
    pkTS2->Set(0,pkBackTex);

    // 1000 is the depth that I want this created at.  The 0.4125 comes
    // from the near plane halfwidth (which is a square).  Thus this square
    // should fill the viewplane (assuming no camera movement) and will sit
    // behind the face.
    TriMesh* pkBackMesh = CreateSquare( 0.4125f*5000, 5000 );
    pkBackMesh->SetRenderState( pkTS2 );

    m_spkScene->AttachChild( pkBackMesh );

    // So, a negative index isn't really physically correct, but I think it
    // makes the object appear to refract what's underneath it.
    m_fRIndex = -0.4f;
    m_spkTriMesh->GetVertexConst( "IndexOfRefraction" )->SetData( 
        m_fRIndex );

    m_bReflection = true;
    m_spkTriMesh->GetVertexConst( "FresnelConstants" )->SetData( 0.1f,
        0.1f, 0.2f );

    m_fRFactor = 0.8f;
 
    return true;
}
//----------------------------------------------------------------------------
bool Refraction::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    m_spkVertShader = VertexShader::Load("refraction.wvs");
    m_spkPixShader = PixelShader::Load("refraction.wps");

    if ( !m_spkVertShader || !m_spkPixShader || !Setup() )
        return true;

    ms_spkCamera->SetFrustum(1.0f,10000.0f,-0.4125f,0.4125f,0.4125f,-0.4125f);
    Vector3f kCLoc(0.0f,0.0f,0.0f);
    Vector3f kCLeft(0.0f,0.0f,-1.0f);
    Vector3f kCUp(0.0f,1.0f,0.0f);
    Vector3f kCDir(1.0f,0.0f,0.0f);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_spkMotionObject = m_spkTriMesh;

    m_bTurretActive = true;

    m_bInitialized = true;
    return true;
}
//----------------------------------------------------------------------------
void Refraction::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkTrnNode = NULL;
    m_spkModel = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void Refraction::OnIdle ()
{
    MeasureTime();
    //MoveCamera();

    if ( MoveObject() )
        m_spkScene->UpdateGS(0.0f);

    ms_spkRenderer->ClearBuffers();
    if ( ms_spkRenderer->BeginScene() )
    {
        if ( m_bInitialized )
        {
            ms_spkRenderer->Draw(m_spkScene);
            DrawFrameRate(8,GetHeight()-8,ColorRGB::WHITE);
        }
        else
        {
            ms_spkRenderer->Draw(8,32,ColorRGB::WHITE,
                "Load of refraction.{wvs,wps}, face.mgc, tiles2-sphere.mif");
            ms_spkRenderer->Draw(8,48,ColorRGB::WHITE,
                "tiles-sphere.mif or tiles2.mif failed.  ");
            ms_spkRenderer->Draw(8,64,ColorRGB::WHITE,
                "Make sure these files are in the same directory as the "
                "executable.");
        }
        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();

    UpdateClicks();
}
//----------------------------------------------------------------------------
void Refraction::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }
    else if ( ucKey == ' ' )
    {
        m_bReflection = !m_bReflection;
        if ( m_bReflection )
        {
            m_spkTriMesh->GetVertexConst( "FresnelConstants" )->SetData( 0.1f,
                0.1f, 0.2f );
        }
        else
        {
            m_spkTriMesh->GetVertexConst( "FresnelConstants" )->SetData( 0.0f,
                0.0f, 0.0f );
        }
    }
    else if ( ucKey == '+' )
    {
        m_fRIndex += 0.1f;
        m_spkTriMesh->GetVertexConst( "IndexOfRefraction" )->SetData( 
            m_fRIndex );
    }
    else if ( ucKey == '-' )
    {
        m_fRIndex -= 0.1f;
        m_spkTriMesh->GetVertexConst( "IndexOfRefraction" )->SetData( 
            m_fRIndex );
    }
}
//----------------------------------------------------------------------------
