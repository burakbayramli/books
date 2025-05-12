// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "Charcoal.h"
#include "WmlDirectionalLight.h"

Charcoal g_kTheApp;

//----------------------------------------------------------------------------
Charcoal::Charcoal ()
    :
    Application("Charcoal",0,0,640,640,ColorRGB(1.0f,1.0f,1.0f))
{
    m_bInitialized = false;
}
//----------------------------------------------------------------------------
Image* Charcoal::ContrastImage(int iWidth, int iHeight, double dNoiseDensity,
    double dContrastExponent)
{
    int iConWidth = 2*iWidth;
    int iConHeight = 2*iHeight;

	// Create an image that is double the requested width and height 
    // (double to get a blurring effect on the noise)
    unsigned char* aucImageData = new unsigned char[4*iConWidth*iConHeight];
    unsigned char* aucColor;

    // First make the entire texture completely white, then we will 
    // sprinkle black dots all over it
	
	for (int x = 0 ; x < 4*iConWidth*iConHeight; x++)
	{
        aucImageData[x] = (unsigned char)255;
	}

    // seed random
    Mathd::UnitRandom(GetTimeInSeconds());

	int iNumNoiseTexels = (int)(dNoiseDensity * (double)(iConWidth*
        iConHeight));

	for (int i = 0 ; i < iNumNoiseTexels ; i++)
	{
			// Generate x and y texture coordinates randomly between 0 and 1
            double dX = Mathd::UnitRandom();
			double dY = Mathd::UnitRandom();
            dY = Mathd::Pow(dY, dContrastExponent);

			int iTexelX = (int)(dX * (double)(iConWidth - 1.0));
			int iTexelY = (int)(dY * (double)(iConHeight - 1.0));

            aucColor = &aucImageData[4*(iTexelX+iConWidth*iTexelY)];
            aucColor[0] = (unsigned char)0;
            aucColor[1] = (unsigned char)0;
            aucColor[2] = (unsigned char)0;
	}

    return( new Image(Image::IT_RGBA8888, iConWidth, iConHeight,
        aucImageData, "ConstrastImage" ) );
}
//----------------------------------------------------------------------------
Image* Charcoal::RandomImage(int iWidth, int iHeight)
{
	// Create an image and insert random data into it.
    unsigned char* aucImageData = new unsigned char[4*iWidth*iHeight];

    // fill image with random data.
	for (int x = 0 ; x < 4*iWidth*iHeight; x++)
	{
        aucImageData[x] = (unsigned char)((int)Mathf::IntervalRandom(0,256));
	}
 
    return( new Image(Image::IT_RGBA8888, iWidth, iHeight,
        aucImageData, "RandomImage" ) );
}
//----------------------------------------------------------------------------
TriMesh* Charcoal::CreateSquare (float fSize, float fDepth)
{
    Vector3f* akVertex = new Vector3f[4];
    akVertex[0] = Vector3f(-fSize, -fSize, fDepth);
    akVertex[1] = Vector3f(fSize, -fSize, fDepth);
    akVertex[2] = Vector3f(fSize, fSize, fDepth);
    akVertex[3] = Vector3f(-fSize, fSize, fDepth);

    Vector3f* akNormal = new Vector3f[4];
    akNormal[0] = Vector3f::UNIT_X;
    akNormal[1] = Vector3f::UNIT_X;
    akNormal[2] = Vector3f::UNIT_X;
    akNormal[3] = Vector3f::UNIT_X;

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
void Charcoal::AttachShader(Node* pkNode)
{
    // Recursively attach shaders, add texture coordinates, and set shader
    // constants.

    for( int i = 0; i < pkNode->GetUsed(); i++ )
    {
        Node* pkChild = WmlDynamicCast(Node, pkNode->GetChild(i));

        if ( pkChild )
        {
            AttachShader( pkChild );
        }
        else
        {
            Geometry* pkGeom = WmlDynamicCast(Geometry, pkNode->GetChild(i));
            if ( pkGeom )
            {
                pkGeom->SetVertexShader( m_spkVertShader );
                pkGeom->SetPixelShader( m_spkPixShader );

                // Let's set some constants, too!
                pkGeom->GetVertexConst("AmbientIntensity")->SetData(0.2f);
                pkGeom->GetVertexConst("ContrastExponent")->SetData(
                    (float)m_dContrastExponent);

                // This shader needs some texture coordinates too.  Let's
                // just attach those here too.  Luckily it doens't matter
                // what they are.  They just need to be varied enough to
                // look up into the random texture map.
                if ( !pkGeom->Textures() )
                {
                    int iQuantity = pkGeom->GetVertexQuantity();

                    Vector2f* afTexture = new Vector2f[iQuantity];
                    for( int j = 0; j < iQuantity; j++ )
                    {
                        afTexture[j].X() = Mathf::UnitRandom();
                        afTexture[j].Y() = Mathf::UnitRandom();
                    }

                    pkGeom->Reconstruct(iQuantity, pkGeom->Vertices(),
                        pkGeom->Normals(), pkGeom->Colors(), afTexture);
                }
            }
        }
    }
}
//----------------------------------------------------------------------------
void Charcoal::UpdateConstants(Node* pkNode)
{
    // Update shader constants
    for( int i = 0; i < pkNode->GetUsed(); i++ )
    {
        Node* pkChild = WmlDynamicCast(Node, pkNode->GetChild(i));

        if ( pkChild )
        {
            UpdateConstants( pkChild );
        }
        else
        {
            Geometry* pkGeom = WmlDynamicCast(Geometry, pkNode->GetChild(i));
            if ( pkGeom )
            {
                if ( pkGeom->GetPixelShader() == m_spkPixShader )
                {
                    // Smudge:
                    // 0.5 = normal smudge
                    // 0.0 = no lighting smudging
                    // 1.0 = no contrast map, only diffuse lighting
                    float fSmudgeFactor;

                    if ( m_bDisplayLighting )
                    {
                        if ( m_bSmudge )
                        {
                            fSmudgeFactor = 0.5f;
                        }
                        else
                        {
                            fSmudgeFactor = 1.0f;
                        }
                    }
                    else
                    {
                        fSmudgeFactor = 0.0f;
                    }

                    // Paper:
                    // 0.0 = display paper
                    // 1.0 = no paper
                    float fPaperFactor = ( m_bDisplayPaper ? 0.0f : 1.0f );

                    pkGeom->GetPixelConst("Constants")->SetData(
                        fSmudgeFactor, fPaperFactor);
                }
           }
        }
    }
}
//----------------------------------------------------------------------------
bool Charcoal::Setup ()
{
    // load scene graph
    Stream kStream;
    bool bLoaded = kStream.Load("SkinnedBiped.mgc");
    if ( !bLoaded )
        return true;

    m_spkScene = (Node*) kStream.GetObjectAt(0);

    // recursively attach the shader
    AttachShader( m_spkScene );
    UpdateConstants( m_spkScene );
    
    Image* pkContrast = ContrastImage(512,512,2.0,m_dContrastExponent);
    if ( !pkContrast )
        return false;

    Texture* pkConTexture = new Texture;
    pkConTexture->SetImage(pkContrast);
    pkConTexture->Mipmap() = Texture::MM_NEAREST;
    pkConTexture->Filter() = Texture::FM_LINEAR;
    pkConTexture->Apply() = Texture::AM_DECAL;
    pkConTexture->Wrap() = Texture::WM_CLAMP_S_CLAMP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkConTexture);

    Image* pkRandom = RandomImage(1024, 1024);
    if ( !pkRandom )
        return false;

    Texture* pkRanTexture = new Texture;
    pkRanTexture->SetImage(pkRandom);
    pkRanTexture->Mipmap() = Texture::MM_NONE;
    pkRanTexture->Apply() = Texture::AM_DECAL;
    pkTS->Set(1,pkRanTexture);

    Image* pkPaper = Image::Load("paper.mif");
    if ( !pkPaper )
        return false;

    Texture* pkPapTexture = new Texture;
    pkPapTexture->SetImage(pkPaper);
    pkPapTexture->Mipmap() = Texture::MM_NONE;
    pkPapTexture->Apply() = Texture::AM_DECAL;
    pkTS->Set(2,pkPapTexture);

    DirectionalLight* pkLight1 = new DirectionalLight;
    pkLight1->On() = true;
    Vector3f kDir = Vector3f( -1.0f, 0.3f, -0.8f );
    kDir.Normalize();
    pkLight1->Direction() = kDir;

    m_pkLight = new DirectionalLight;
    m_pkLight->On() = false;
    // If you want another light, uncomment this
    // Vector3f kDir2 = Vector3f( 0.4f, -1.0f, -0.5f );
    // kDir2.Normalize();
    Vector3f kDir2 = Vector3f(0.0f, 0.0f, 0.0f );
    m_pkLight->Direction() = kDir2;

    LightState* pkLightState = new LightState();
    pkLightState->Attach( pkLight1 );
    pkLightState->Attach( m_pkLight );

    m_spkScene->SetRenderState(pkTS);
    m_spkScene->SetRenderState(pkLightState);

    TriMesh* pkBackMesh = CreateSquare( 1.0f, 1.0f );
    
    Image* pkBackground = Image::Load("paper.mif");
    if ( !pkBackground )
        return false;

    Texture* pkBackTex = new Texture;
    pkBackTex->SetImage(pkBackground);
    pkBackTex->Apply() = Texture::AM_DECAL;
    TextureState* pkTSBack = new TextureState;
    pkTSBack->Set(0,pkBackTex);

    pkBackMesh->SetRenderState( pkTSBack );

    VertexShader* pkBackV = VertexShader::Load("Background.wvs");
    PixelShader* pkBackP = PixelShader::Load("Background.wps");

    if (!pkBackV || !pkBackP)
        return false;

    pkBackMesh->SetVertexShader(pkBackV);
    pkBackMesh->SetPixelShader(pkBackP);

    m_spkScene->AttachChild(pkBackMesh);

    return true;
}
//----------------------------------------------------------------------------
bool Charcoal::OnInitialize ()
{
    // arbitrary!
    m_fCycle = 0.89f;

    m_dContrastExponent = 3.5;
    m_bRunning = false;
    m_fStopTime = 0.0f;
    m_bDisplayLighting = true;
    m_bDisplayPaper = true;
    m_bSmudge = true;

    if ( !Application::OnInitialize() )
        return false;

    m_spkVertShader = VertexShader::Load("Charcoal.wvs");
    m_spkPixShader = PixelShader::Load("Charcoal.wps");

    if ( !m_spkVertShader || !m_spkPixShader || !Setup() )
        return true;

    // set up camera
    ms_spkCamera->SetFrustum(1.0f,1000.0f,-0.4125f,0.4125f,0.4125f,-0.4125f);
    Vector3f kCLoc(80.0f,0.0f,23.0f);
    Vector3f kCLeft(0.0f,-1.0f,0.0f);
    Vector3f kCUp(0.0f,0.0f,1.0f);
    Vector3f kCDir(-1.0f,0.0f,0.0f);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_spkMotionObject = m_spkScene;
    m_fTrnSpeed = 0.1f;
    m_fRotSpeed = 0.001f;
    m_bTurretActive = true;
    SetTurretAxes();

    m_bInitialized = true;
    return true;
}
//----------------------------------------------------------------------------
void Charcoal::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkTrnNode = NULL;
    m_spkModel = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void Charcoal::OnIdle ()
{
    MeasureTime();
    MoveCamera(); 
    
    float fTime = GetTimeInSeconds();
    
    // Light rotating (looks kind of goofy.  Maybe with a higher poly count
    // model it would look fine.)
    /*
    float fLightCycle = fTime*Mathf::PI/10;
    m_pkLight->Direction() = Vector3f( Mathf::Sin(fLightCycle), 
        Mathf::Cos(fLightCycle), -0.5f);
    */

    if (m_bRunning)
    {
        fTime -= m_fCycle*((int)(fTime/m_fCycle));
        m_spkScene->UpdateGS(fTime);
    }
    else
    {
        fTime = m_fStopTime;
        fTime -= m_fCycle*((int)(fTime/m_fCycle));
        m_spkScene->UpdateGS(fTime);
    }

    ms_spkRenderer->ClearBuffers();
    if ( ms_spkRenderer->BeginScene() )
    {
        if ( m_bInitialized )
        {
            ms_spkRenderer->Draw(m_spkScene);
            //DrawFrameRate(8,GetHeight()-8,ColorRGB::BLACK);
        }
        else
        {
            ms_spkRenderer->Draw(8,32,ColorRGB::WHITE,
                "Load of (one of) Charcoal.{wvs,wps}, SkinnedBiped.mgc"\
                ", paper.mif,");
            ms_spkRenderer->Draw(8,48,ColorRGB::WHITE,
                "or Background.{wvs,wps} failed.  ");
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
void Charcoal::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }
    else if ( ucKey == ' ' )
    {
        // toggle running
        m_bRunning = !m_bRunning;
        m_fStopTime = GetTimeInSeconds();
    }
    else if ( ucKey == 'p' )
    {
        m_bDisplayPaper = !m_bDisplayPaper;
        UpdateConstants(m_spkScene);
    }
    else if ( ucKey == 's' )
    {
        m_bSmudge = !m_bSmudge;
        UpdateConstants(m_spkScene);
    }
    else if ( ucKey == 'l' )
    {
        m_bDisplayLighting = !m_bDisplayLighting;
        UpdateConstants(m_spkScene);
    }
}
//----------------------------------------------------------------------------
