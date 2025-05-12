// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "RipplingOcean.h"
#include "WmlStandardMesh.h"

RipplingOcean g_kTheApp;

//----------------------------------------------------------------------------
RipplingOcean::RipplingOcean ()
    :
    Application("RipplingOcean",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
    m_bInitialized = false;
}
//----------------------------------------------------------------------------
void RipplingOcean::HeightToNormalMap (Image*& pkImage)
{
    // Simple 4 sample nearest neighbor kernel filter

    // It only uses the red component to determine the height (which is fine
    // for all monochrome height maps).
 
    // It also assumes that the image will wrap, and so when it goes off one
    // edge it will reuse the data on the other side.

    // It also assumes that positive Z is the direction of the height map.

    int iWidth = pkImage->GetWidth();
    int iHeight = pkImage->GetHeight();
    int iFac = pkImage->GetBytesPerPixel();

    float fScale = 100.0f;

    fScale *= 1.0f/255.0f;

    unsigned char* aucNormal = new unsigned char[iWidth*iHeight*4];

    for (int iX = 0; iX < iWidth; iX++)
    {
        for (int iY = 0; iY < iHeight; iY++)
        {
            int iNegX = (int)(pkImage->GetData()[iFac*((iX-1+iWidth)%iWidth + 
                iY*iWidth)]);
            int iPosX = (int)(pkImage->GetData()[iFac*((iX+1)%iWidth + 
                iY*iWidth)]);;

            int iNegY = (int)(pkImage->GetData()[iFac*(iX+((iY-1+iHeight)%
                iHeight)*iWidth)]);
            int iPosY = (int)(pkImage->GetData()[iFac*(iX+((iY+1)%iHeight)*
                iWidth)]);

            Vector3f kCross(fScale*(iNegX-iPosX)/2.0f, 
                fScale*(iNegY-iPosY)/2.0f, 1.0f);
            kCross.Normalize();

            kCross = 0.5f * kCross + Vector3f(0.5f,0.5f,0.5f);

            unsigned char* aucColor = &aucNormal[4*(iX+iY*iWidth)];
            aucColor[0] = (unsigned char)((int)(kCross.X()*255));
            aucColor[1] = (unsigned char)((int)(kCross.Y()*255));
            aucColor[2] = (unsigned char)((int)(kCross.Z()*255));
            aucColor[3] = (unsigned char)255;
        }
    }

    delete pkImage;
    pkImage = new Image(Image::IT_RGBA8888, iWidth, iHeight, aucNormal);
}
//----------------------------------------------------------------------------
void RipplingOcean::CreateRectangleMesh (TriMesh*& rpkMesh,
    const Vector3f& rkCenter, const Vector3f& rkU, const Vector3f& rkV,
    const Vector3f& rkAxis, float fUExtent, float fVExtent,
    int iUSamples, int iVSamples, bool bWantNormals, bool bWantColors, 
    bool bWantUVs)
{
    assert( iUSamples >=2 );
    assert( iVSamples >=2 );

    // allocate vertices
    int iVQuantity = iUSamples*iVSamples;
    Vector3f* akVertex = new Vector3f[iVQuantity];

    // allocate normals if requested
    Vector3f* akNormal = NULL;
    if ( bWantNormals )
        akNormal = new Vector3f[iVQuantity];

    // allocate colors if requested
    ColorRGB* akColor = NULL;
    if ( bWantColors )
    {
        akColor = new ColorRGB[iVQuantity];
        //memset(akColor,0,iVQuantity*sizeof(ColorRGB));

        // We want tangent data here.
        // rkV better be normalized!
        ColorRGB kColor( rkV.X()*0.5f+0.5f, rkV.Y()*0.5f+0.5f, 
            rkV.Z()*0.5f+0.5f );        

        for (int i = 0; i < iVQuantity; i++)
        {
            akColor[i] = kColor;
        }
    }

    // allocate texture coordinates if requested
    Vector2f* akUV = NULL;
    if ( bWantUVs )
        akUV = new Vector2f[iVQuantity];

    // allocate connectivity
    int iTQuantity = (iUSamples-1)*(iVSamples-1)*2;
    int* aiConnect = new int[3*iTQuantity];

    int iVNum = 0;

    // generate geometry
    float fInvU = 1.0f/(float)(iUSamples-1);
    float fInvV = 1.0f/(float)(iVSamples-1);

    float fCurU, fCurV;
    int iU, iV;
    for (iU = 0, fCurU = 0.0f; iU < iUSamples; iU++)
    {
        for (iV = 0, fCurV = 0.0f; iV < iVSamples; iV++)
        {
            akVertex[iVNum] = rkCenter + fUExtent*rkU*(fCurU-0.5f)
                + fVExtent*rkV*(fCurV-0.5f);

            if ( bWantUVs )
            {
                akUV[iVNum].X() = fCurU;
                akUV[iVNum].Y() = fCurV;
            }

            iVNum++;
            fCurV += fInvV;
        }
        fCurU += fInvU;
    }

    if ( bWantNormals )
    {
        for (int i = 0; i < iVQuantity; i++)
            akNormal[i] = rkAxis;
    }

    assert( iVNum == iVQuantity );

    int iC = 0;

    // generate connectivity
    for (iU = 0; iU < iUSamples-1; iU++)
    {
        for (iV = 0; iV < iVSamples-1; iV++)
        {
            int iV0, iV1, iV2, iV3;
            iV0 = iV + iVSamples*iU;
            iV1 = iV0 + 1;
            iV2 = iV0 + iVSamples;
            iV3 = iV2 + 1;

            aiConnect[iC++] = iV0;
            aiConnect[iC++] = iV1;
            aiConnect[iC++] = iV2;

            aiConnect[iC++] = iV2;
            aiConnect[iC++] = iV1;
            aiConnect[iC++] = iV3;
        }
    }

    assert( iC == iTQuantity*3 );

    if ( rpkMesh )
    {
        rpkMesh->Reconstruct(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
    else
    {
        rpkMesh = new TriMesh(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
}
//----------------------------------------------------------------------------
void RipplingOcean::SetupShaders ()
{
    // Re-set all the constants

    // AvgDuDxDvDy, Ambient, TexRepeat, fTime
    float fTime;

    if ( m_bStopped )
    {
        fTime = m_fStopTime;
    }
    else
    {
        fTime = GetTimeInSeconds();
    }
    m_spkTriMesh->GetVertexConst("u_f4Constants")->SetData(
        1/24.0f, m_fAmbient, m_fTexRepeat, fTime );

    Vector3f kLightDir(0.0f,1.0f,1.0f);
    kLightDir.Normalize();
    m_spkTriMesh->GetVertexConst("u_f3LightDir")->SetData( kLightDir );
    m_spkTriMesh->GetVertexConst("u_f4WaveDirX")->SetData(0.25f, 0.0f, -0.7f,
        -0.8f);
    m_spkTriMesh->GetVertexConst("u_f4WaveDirY")->SetData(0, 0.15f, -0.7f, 
        0.1f);
    m_spkTriMesh->GetVertexConst("u_f4WaveSpeed")->SetData(
        0.2f*m_fWaveSpeedFactor, 0.15f*m_fWaveSpeedFactor,
        0.4f*m_fWaveSpeedFactor, 0.4f*m_fWaveSpeedFactor);
    m_spkTriMesh->GetVertexConst("u_f4WaveOffset")->SetData(0, 0.2f, 0.3f,
        -0.2f);
    m_spkTriMesh->GetVertexConst("u_f4WaveHeight")->SetData(
        16.0f*m_fWaveHeightFactor, 10.0f*m_fWaveHeightFactor,
        5.8f*m_fWaveHeightFactor, 8.5f*m_fWaveHeightFactor);
    m_spkTriMesh->GetVertexConst("u_f4BumpSpeed")->SetData(
        0.031f*m_fRippleSpeedFactor, 0.04f*m_fRippleSpeedFactor,
        -0.03f*m_fRippleSpeedFactor, 0.02f*m_fRippleSpeedFactor );
}
//----------------------------------------------------------------------------
bool RipplingOcean::Setup ()
{
    m_bStopped = false;
    m_fStopTime = GetTimeInSeconds();

    m_spkScene = new Node(1);
    m_spkTrnNode = new Node(1);
    m_spkScene->AttachChild(m_spkTrnNode);

    // Root node for a scene graph that contains a bump-mapped triangle mesh
    // square.
    m_spkModel = new Node(1);

    // create the triangle mesh surface
    TriMesh* pkMesh = NULL;
    CreateRectangleMesh(pkMesh, Vector3f::ZERO,
        Vector3f::UNIT_X, Vector3f::UNIT_Y, -Vector3f::UNIT_Z,
        1400.0f, 1200.0f, 50, 50, true, true, true);

    m_spkTriMesh = pkMesh;

    m_spkTriMesh->SetVertexShader(m_spkVertShader);
    m_spkTriMesh->SetPixelShader(m_spkPixShader);
    SetupShaders();

    Image* pkNormal = Image::Load("plasma.mif");
    if ( !pkNormal )
        return false;

    HeightToNormalMap( pkNormal );

    Texture* pkNormalTex = new Texture;
    pkNormalTex->SetImage(pkNormal);
    pkNormalTex->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkNormalTex->Filter() = Texture::FM_LINEAR;
    pkNormalTex->Apply() = Texture::AM_DECAL;
    pkNormalTex->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkNormalTex);

    Image* pkWater = Image::Load("watergradient.mif");
    if (!pkWater)
        return false;

    Texture* pkWaterTex = new Texture;
    pkWaterTex->SetImage(pkWater);
    pkWaterTex->Apply() = Texture::AM_DECAL;
    pkWaterTex->Wrap() = Texture::WM_CLAMP_S_CLAMP_T;
    pkTS->Set(1,pkWaterTex);

    Image* pkSkySphere = Image::Load("sky.mif");
    if (!pkSkySphere)
        return false;

    Texture* pkSkySphereTex = new Texture;
    pkSkySphereTex->SetImage(pkSkySphere);
    pkTS->Set(2,pkSkySphereTex);

    m_spkTriMesh->SetRenderState(pkTS);

    m_spkModel->AttachChild(m_spkTriMesh);
    m_spkTrnNode->AttachChild(m_spkModel);

    // I'll admit that this is kind of a hack, but it puts the sun
    // a smidge higher in the sky.  It makes it look nicest to start.  =)
    Matrix3f kIncr;
    kIncr.FromAxisAngle(Vector3f::UNIT_X, -0.08f);
    m_spkTrnNode->Rotate() = kIncr; 

    return true;
}
//----------------------------------------------------------------------------
bool RipplingOcean::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    m_bVertexShader = true;
    m_spkVertShader = VertexShader::Load("RipplingOcean.wvs");
    m_spkPixShader = PixelShader::Load("RipplingOcean.wps");

    m_fAmbient = 0.3f;
    m_fTexRepeat = 6.0f;
    m_fWaveSpeedFactor = 1.0f;
    m_fWaveHeightFactor = 1.0f;
    m_fRippleSpeedFactor = 1.0f;

    if ( !m_spkVertShader || !m_spkPixShader || !Setup() )
        return true;

    SetupShaders();

    m_spkScene->UpdateGS(0.0f);

    ms_spkCamera->SetFrustum(1.0f,10000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLeft(1.0f,0.0f,0.0f);
    Vector3f kCUp(0.0f,1.0f,0.0f); 

    Vector3f kCDir(0.0f,1.0f,0.5f);
    kCDir.Normalize();
    Vector3f kCLoc(0.0f, -600.0f, -100.0f);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_spkMotionObject = m_spkScene;
    m_fTrnSpeed = 2.0f;
    m_fRotSpeed = 0.001f;
    m_bTurretActive = true;
    SetTurretAxes();

    m_bInitialized = true;
    return true;
}
//----------------------------------------------------------------------------
void RipplingOcean::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkTrnNode = NULL;
    m_spkModel = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void RipplingOcean::OnIdle ()
{
    MeasureTime();
    MoveCamera();

    if ( m_bVertexShader )
    {
        SetupShaders();
    }

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
                "Load of ripple.{wvs,wps}, sky.mif, watergradient.mif"\
                " or plasa.mif failed.  ");
            ms_spkRenderer->Draw(8,48,ColorRGB::WHITE,
                "Make sure these files are in the same directory as the "
                "executable.");
        }
        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();

    UpdateClicks();
}
//----------------------------------------------------------------------------
void RipplingOcean::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }
    else if ( ucKey == 'w' )
    {
        m_fWaveHeightFactor -= 0.1f;
        if ( m_fWaveHeightFactor < 0 )
            m_fWaveHeightFactor = 0;
    }
    else if ( ucKey == 'W' )
    {
        m_fWaveHeightFactor += 0.1f;
    }
    else if ( ucKey == 's' )
    {
        m_fWaveSpeedFactor -= 0.1f;
        if ( m_fWaveSpeedFactor < 0 )
            m_fWaveSpeedFactor = 0;
    }
    else if ( ucKey == 'S' )
    {
        m_fWaveSpeedFactor += 0.1f;
    }
    else if ( ucKey == 'a' )
    {
        m_fAmbient -= 0.05f;
        if ( m_fAmbient < 0 )
            m_fAmbient = 0;
    }
    else if ( ucKey == 'A' )
    {
        m_fAmbient += 0.05f;
        if ( m_fAmbient > 1 )
            m_fAmbient = 1;
    }
    else if ( ucKey == 'r' )
    {
        m_fRippleSpeedFactor -= 0.1f;
        if ( m_fRippleSpeedFactor < 0 )
            m_fRippleSpeedFactor = 0;
    }
    else if ( ucKey == 'R' )
    {
        m_fRippleSpeedFactor += 0.1f;
    }
    else if ( ucKey == 'T' )
    {
        m_fTexRepeat += 0.1f;
    }
    else if ( ucKey == 't' )
    {
        m_fTexRepeat -= 0.1f;
        if ( m_fTexRepeat < 0 )
            m_fTexRepeat = 0;
    }
    else if ( ucKey == ' ' )
    {
        m_bStopped = !m_bStopped;
        m_fStopTime = GetTimeInSeconds();
    }
}
//----------------------------------------------------------------------------
