// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "VertexNoise.h"

VertexNoise g_kTheApp;

const int VertexNoise::TABLE_SIZE = 32;

//----------------------------------------------------------------------------
VertexNoise::VertexNoise ()
    :
    Application("VertexNoise",0,0,640,480,ColorRGB(0.85f,0.85f,0.85f))
{
    m_bInitialized = false;
}
//----------------------------------------------------------------------------
bool VertexNoise::Setup ()
{
    Stream kStream;
    bool bLoaded = kStream.Load("Face.mgc");
    if ( !bLoaded )
        return false;

    m_spkScene = (Node*) kStream.GetObjectAt(0);
    m_spkWireframe = new WireframeState;
    m_spkScene->SetRenderState(m_spkWireframe);

    m_spkTriMesh = WmlSmartPointerCast(TriMesh,
        m_spkScene->GetChild(0));

    m_spkTriMesh->SetVertexShader(m_spkVertShader);
    InitVertexNoiseConstants();
    return true;
}
//----------------------------------------------------------------------------
void VertexNoise::InitVertexNoiseConstants()
{
	int* aiP = new int[TABLE_SIZE*2+2];              // permutation table
    Vector4f* akG = new Vector4f[TABLE_SIZE*2+2];
	int i;

    // initalize random gradients
	for(i  =0; i < TABLE_SIZE; i++)
    {
		aiP[i] = i;
        akG[i][0] = Mathf::SymmetricRandom();
		akG[i][1] = Mathf::SymmetricRandom();
		akG[i][2] = Mathf::SymmetricRandom();
        akG[i][3] = 0.0f;
        akG[i].Normalize();
	}

	// initialize permutation table (random shuffle)
	for(i = 0; i < TABLE_SIZE; i++)
    {
		int j, t;
		j = (rand() >> 4) % TABLE_SIZE;
		t = aiP[i];
		aiP[i] = aiP[j];
		aiP[j] = t;

        akG[i][3] = (float) aiP[i];
    }

    for(i = 0; i < TABLE_SIZE+2; i++)
    {

        // mirror first half of table into second half (+2)
        akG[i+TABLE_SIZE][0] = akG[i%TABLE_SIZE][0];
        akG[i+TABLE_SIZE][1] = akG[i%TABLE_SIZE][1];
        akG[i+TABLE_SIZE][2] = akG[i%TABLE_SIZE][2];
        akG[i+TABLE_SIZE][3] = akG[i%TABLE_SIZE][3];
	}

    for (i = 0; i < TABLE_SIZE*2+2; i++)
    {
        char acName[8];
        sprintf(acName,"pg[%i]",i);
        ShaderConst* pkConst = m_spkTriMesh->GetVertexShaderConstants()->
            GetConstant(acName);
        pkConst->SetData(&(akG[i][0]));
    }

    m_fDisplacement = 8.0f;
    m_kNoiseTrans[0] = 0;
    m_kNoiseTrans[1] = 0;
    m_kNoiseTrans[2] = 0;
    m_kNoiseTrans[3] = 0.0f;
    m_fNoiseScale = 1.0f;

    delete[] aiP;
    delete[] akG;
}
//----------------------------------------------------------------------------
bool VertexNoise::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    m_spkVertShader = VertexShader::Load("VertexNoise.wvs");

    if ( !m_spkVertShader || !Setup() )
        return true;

    m_bVertexShader = true;

    ms_spkCamera->SetFrustum(1.0f,10000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLoc(0.0f,0.0f,0.0f);
    Vector3f kCLeft(0.0f,0.0f,-1.0f);
    Vector3f kCUp(0.0f,1.0f,0.0f);
    Vector3f kCDir(1.0f,0.0f,0.0f);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_spkMotionObject = m_spkScene;
    m_fTrnSpeed = 5.0f;
    m_fRotSpeed = 0.01f;
    m_bTurretActive = true;
    SetTurretAxes();

    m_bInitialized = true;
    return true;
}
//----------------------------------------------------------------------------
void VertexNoise::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkTrnNode = NULL;
    m_spkModel = NULL;
    m_spkWireframe = NULL;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void VertexNoise::OnIdle ()
{
    MeasureTime();
    MoveCamera();

    //m_kNoiseTrans[1] -= 0.01f;
    m_kNoiseTrans[1] = -(float)GetTimeInSeconds()*2.0f;

    if ( m_bVertexShader )
    {
        // update parameters
        m_spkTriMesh->GetVertexConst("BaseColor")->SetData( 0.99607f,
            0.8392f, 0.67059f, 0.0f );
        float afTemp[4] = { m_fDisplacement, m_fDisplacement,
            m_fDisplacement,  m_fDisplacement };
        m_spkTriMesh->GetVertexConst("Displacement")->SetData( afTemp );

        float afTemp2[4] = { m_fNoiseScale, m_fNoiseScale, m_fNoiseScale, 
            m_fNoiseScale };
        m_spkTriMesh->GetVertexConst("NoiseScale")->SetData(afTemp2);
        m_spkTriMesh->GetVertexConst("NoiseTranslate")->SetData(
            &m_kNoiseTrans[0]);
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
                "Load of VertexNoise.wvs or Face.mgc failed." );
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
void VertexNoise::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    switch ( ucKey )
    {
    case ' ':
        m_bVertexShader = !m_bVertexShader;
        if ( m_bVertexShader )
        {
            m_spkTriMesh->SetVertexShader( m_spkVertShader );
            InitVertexNoiseConstants();
        }
        else
        {
            m_spkTriMesh->SetVertexShader( NULL );
        }
        break;
    case '+':
        m_fNoiseScale += 0.2f;
        break;
    case '-':
        m_fNoiseScale -= 0.2f;
        break;
    case '1':
        m_fDisplacement += 0.5f;
        break;
    case '2':
        m_fDisplacement -= 0.5f;
        break;
    case 'r':
        m_fNoiseScale = 1.0f;
        m_kNoiseTrans[0] = 0.0f;
        m_kNoiseTrans[1] = 0.0f;
        m_kNoiseTrans[2] = 0.0f;
        m_kNoiseTrans[3] = 0.0f;
        m_fDisplacement = 8.0f;
        break;
    case 'w':
        m_spkWireframe->Enabled() = !m_spkWireframe->Enabled();
        break;
    }
}
//----------------------------------------------------------------------------
