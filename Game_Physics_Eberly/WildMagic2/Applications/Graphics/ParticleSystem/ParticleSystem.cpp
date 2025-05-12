// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "ParticleSystem.h"
#include "BloodCellController.h"

ParticleSystem g_kTheApp;

//----------------------------------------------------------------------------
ParticleSystem::ParticleSystem ()
    :
    Application("ParticleSystem",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
}
//----------------------------------------------------------------------------
bool ParticleSystem::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // set up camera
    ms_spkCamera->SetFrustum(1.0f,1000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLoc(4.0f,0.0f,0.0f);
    Vector3f kCLeft(0.0f,-1.0f,0.0f);
    Vector3f kCUp(0.0f,0.0f,1.0f);
    Vector3f kCDir(-1.0f,0.0f,0.0f);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // set up scene
    m_spkScene = new Node(1);

    m_spkWireframe = new WireframeState;
    m_spkWireframe->Enabled() = false;
    m_spkScene->SetRenderState(m_spkWireframe);

    // create an image with transparency
    int iWidth = 32, iHeight = 32;
    float fFactor = 1.0f/(iWidth*iWidth + iHeight*iHeight);
    unsigned char* aucData = new unsigned char[4*iWidth*iHeight];
    int i = 0;
    for (int iY = 0; iY < iHeight; iY++)
    {
        for (int iX = 0; iX < iWidth; iX++)
        {
            aucData[i++] = 255;
            aucData[i++] = 0;
            aucData[i++] = 0;

            int iDX = 2*iX - iWidth;
            int iDY = 2*iY - iHeight;
            float fValue = fFactor*(iDX*iDX + iDY*iDY);
            assert( 0.0f <= fValue && fValue <= 1.0f );
            if ( fValue < 0.125f )
                fValue = Mathf::Cos(4.0f*Mathf::PI*fValue);
            else
                fValue = 0.0f;
            aucData[i++] = (unsigned char)(255.0f*fValue);
        }
    }

    Image* pkImage = new Image(Image::IT_RGBA8888,iWidth,iHeight,aucData,
        "BloodCell.mif");

    Texture* pkTexture = new Texture;
    pkTexture->SetImage(pkImage);
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR;
    pkTexture->Wrap() = Texture::WM_CLAMP_S_CLAMP_T;
    TextureState* pkTextureState = new TextureState;
    pkTextureState->Set(0,pkTexture);

    AlphaState* pkAlphaState = new AlphaState;
    pkAlphaState->BlendEnabled() = true;

    int iVertexQuantity = 32;
    Vector3f* akVertex = new Vector3f[iVertexQuantity];
    float* afSize = new float[iVertexQuantity];
    for (i = 0; i < iVertexQuantity; i++)
    {
        akVertex[i].X() = Mathf::SymmetricRandom();
        akVertex[i].Y() = Mathf::SymmetricRandom();
        akVertex[i].Z() = Mathf::SymmetricRandom();
        afSize[i] = 0.25f*Mathf::UnitRandom();
    }

    m_spkParticles = new Particles(iVertexQuantity,akVertex,NULL,NULL,afSize,
        true);
    m_spkParticles->SetRenderState(pkTextureState);
    m_spkParticles->SetRenderState(pkAlphaState);

    m_spkParticles->AttachControl(new BloodCellController);

    m_spkScene->AttachChild(m_spkParticles);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_bTurretActive = true;
    SetTurretAxes();
    m_fTrnSpeed = 0.01f;
    m_fRotSpeed = 0.01f;

    return true;
}
//----------------------------------------------------------------------------
void ParticleSystem::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkParticles = NULL;
    m_spkWireframe = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void ParticleSystem::OnIdle ()
{
    MeasureTime();

    MoveCamera();

    m_spkParticles->UpdateGS(0.0f);

    ms_spkRenderer->ClearBuffers();
    if ( ms_spkRenderer->BeginScene() )
    {
        ms_spkRenderer->Draw(m_spkScene);
        DrawFrameRate(8,GetHeight()-8,ColorRGB::WHITE);
        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();

    UpdateClicks();
}
//----------------------------------------------------------------------------
void ParticleSystem::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    switch ( ucKey )
    {
    case '0':  // reset frame rate measurements
        ResetTime();
        return;
    case 'w':
        m_spkWireframe->Enabled() = !m_spkWireframe->Enabled();
        return;
    }
}
//----------------------------------------------------------------------------
