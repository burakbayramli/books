// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "Skinning.h"

Skinning g_kTheApp;

//----------------------------------------------------------------------------
Skinning::Skinning ()
    :
    Application("Skinning",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
    m_bInitialized = false;
}
//----------------------------------------------------------------------------
TriMesh* Skinning::CreateCylinder (int iRadialSamples,
    const Vector3f& rkCenter, const Vector3f& rkU, const Vector3f& rkV,
    const Vector3f& rkAxis, float fRadius, float fHeight, bool bWantNormals,
    bool bWantColors, bool bOutsideView)
{
    const int iAxisSamples = 7;

    // allocate vertices
    int iVQuantity = iAxisSamples*(iRadialSamples+1);
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
        memset(akColor,0,iVQuantity*sizeof(ColorRGB));
    }

    // allocate texture coordinates for skinning weights
    Vector2f* akUV = new Vector2f[iVQuantity];
    Vector2f* akST = new Vector2f[iVQuantity];

    // allocate connectivity
    int iTQuantity = 2*(iAxisSamples-1)*iRadialSamples;
    int* aiConnect = new int[3*iTQuantity];

    // generate geometry
    float fInvRS = 1.0f/(float)iRadialSamples;
    float fInvASm1 = 1.0f/(float)(iAxisSamples-1);
    float fHalfHeight = 0.5f*fHeight;
    int iR, iA, iAStart, i;

    // Generate points on the unit circle to be used in computing the mesh
    // points on a cylinder slice.
    float* afSin = new float[iRadialSamples+1];
    float* afCos = new float[iRadialSamples+1];
    for (iR = 0; iR < iRadialSamples; iR++)
    {
        float fAngle = Mathf::TWO_PI*fInvRS*iR;
        afCos[iR] = Mathf::Cos(fAngle);
        afSin[iR] = Mathf::Sin(fAngle);
    }
    afSin[iRadialSamples] = afSin[0];
    afCos[iRadialSamples] = afCos[0];

    // generate the cylinder itself
    for (iA = 0, i = 0; iA < iAxisSamples; iA++)
    {
        float fAxisFraction = iA*fInvASm1;  // in [0,1]
        float fZ = -fHalfHeight + fHeight*fAxisFraction;

        // compute center of slice
        Vector3f kSliceCenter = rkCenter + fZ*rkAxis;

        // compute slice vertices with duplication at end point
        int iSave = i;
        for (iR = 0; iR < iRadialSamples; iR++)
        {
            Vector3f kNormal = afCos[iR]*rkU + afSin[iR]*rkV;
            akVertex[i] = kSliceCenter + fRadius*kNormal;
            if ( bWantNormals )
            {
                if ( bOutsideView )
                    akNormal[i] = kNormal;
                else
                    akNormal[i] = -kNormal;
            }

            // Set texture coordinates to correct vertex weights
            if ( iA == 0 )
                akUV[i].X() = 1.0f;
            else if ( iA == 1 )
                akUV[i].X() = 0.5f;
            else
                akUV[i].X() = 0.0f;

            if ( ( iA == 1 ) || ( iA == 3 ) )
                akUV[i].Y() = 0.5f;
            else if ( iA == 2 )
                akUV[i].Y() = 1.0f;
            else
                akUV[i].Y() = 0.0f;


            if ( ( iA == 3 ) || ( iA == 5 ) )
                akST[i].X() = 0.5f;
            else if ( iA == 4 )
                akST[i].X() = 1.0f;
            else
                akST[i].X() = 0.0f;

            if ( iA == 6 )
                akST[i].Y() = 1.0f;
            else if ( iA == 5 )
                akST[i].Y() = 0.5f;
            else
                akST[i].Y() = 0.0f;

            akColor[i] = ColorRGB( fAxisFraction, 1-fAxisFraction, 0.3f );
            i++;
        }

        akVertex[i] = akVertex[iSave];
        if ( bWantNormals )
        {
            akNormal[i] = akNormal[iSave];
        }

        // Set texture coordinates to correct vertex weights
        if ( iA == 0 )
            akUV[i].X() = 1.0f;
        else if ( iA == 1 )
            akUV[i].X() = 0.5f;
        else
            akUV[i].X() = 0.0f;

        if ( ( iA == 1 ) || ( iA == 3 ) )
            akUV[i].Y() = 0.5f;
        else if ( iA == 2 )
            akUV[i].Y() = 1.0f;
        else
            akUV[i].Y() = 0.0f;


        if ( ( iA == 3 ) || ( iA == 5 ) )
            akST[i].X() = 0.5f;
        else if ( iA == 4 )
            akST[i].X() = 1.0f;
        else
            akST[i].X() = 0.0f;

        if ( iA == 6 )
            akST[i].Y() = 1.0f;
        else if ( iA == 5 )
            akST[i].Y() = 0.5f;
        else
            akST[i].Y() = 0.0f;

        akColor[i] = ColorRGB( fAxisFraction, 1-fAxisFraction, 0.3f );
        i++;
    }

    // generate connectivity
    int* aiLocalConnect = aiConnect;
    for (iA = 0, iAStart = 0; iA < iAxisSamples-1; iA++)
    {
        int i0 = iAStart;
        int i1 = i0 + 1;
        iAStart += iRadialSamples + 1;
        int i2 = iAStart;
        int i3 = i2 + 1;
        for (i = 0; i < iRadialSamples; i++, aiLocalConnect += 6)
        {
            if ( bOutsideView )
            {
                aiLocalConnect[0] = i0++;
                aiLocalConnect[1] = i1;
                aiLocalConnect[2] = i2;
                aiLocalConnect[3] = i1++;
                aiLocalConnect[4] = i3++;
                aiLocalConnect[5] = i2++;
            }
            else // inside view
            {
                aiLocalConnect[0] = i0++;
                aiLocalConnect[1] = i2;
                aiLocalConnect[2] = i1;
                aiLocalConnect[3] = i1++;
                aiLocalConnect[4] = i2++;
                aiLocalConnect[5] = i3++;
            }
        }
    }

    delete[] afCos;
    delete[] afSin;

    return new TriMesh(iVQuantity,akVertex,akNormal,akColor,akUV,
        iTQuantity,aiConnect,akST);
}
//----------------------------------------------------------------------------
bool Skinning::Setup ()
{
    m_spkScene = new Node(1);

    int iRadialSamples = 10;
    Vector3f kCenter(0.0f,0.0f,100.0f);
    Vector3f kU(0.0f,0.0f,-1.0f);
    Vector3f kV(0.0f,1.0f,0.0f);
    Vector3f kAxis(1.0f,0.0f,0.0f);
    float fRadius = 10.0f;
    float fHeight = 80.0f;

    m_spkTriMesh = CreateCylinder(iRadialSamples,kCenter,kU,kV,kAxis,fRadius,
        fHeight,true,true,true);

    m_spkTriMesh->SetVertexShader(m_spkVertShader);
    m_spkScene->AttachChild(m_spkTriMesh); 
    return true;
}
//----------------------------------------------------------------------------
bool Skinning::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    m_spkVertShader = VertexShader::Load("Skinning.wvs");

    if ( !m_spkVertShader || !Setup() )
        return true;

    m_spkScene->UpdateGS(0.0f);

    ms_spkCamera->SetFrustum(1.0f,10000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLeft(1.0f,0.0f,0.0f);
    Vector3f kCUp(0.0f,1.0f,0.0f);
    Vector3f kCDir(0.0f,0.0f,1.0f);
    Vector3f kCLoc(0.0f, 0.0f, 0.0f);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_spkMotionObject = m_spkTriMesh;
    m_fTrnSpeed = 0.01f;
    m_fRotSpeed = 0.001f;
    m_bTurretActive = true;
    SetTurretAxes();

    m_fTime = GetTimeInSeconds();

    m_bShaderEnabled = true;
    m_bInitialized = true;
    return true;
}
//----------------------------------------------------------------------------
void Skinning::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkTrnNode = NULL;
    m_spkModel = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void Skinning::OnIdle ()
{
    MeasureTime();
    MoveCamera();

    m_fTime = GetTimeInSeconds();

    Matrix3f kTemp;

    // create some arbitrary skinning transformations

    const float fFactor = Mathf::PI/1.25f;
    int iDiv = (int)(m_fTime/fFactor);
    // the angle now ranges from -factor/4 to +factor/4
    float fAngle = Mathf::FAbs(m_fTime-iDiv*fFactor-fFactor/2.0f)-
        fFactor/4.0f;

    for( int i = 0; i < 4; i++ )
    {
        if ( (int)(m_fTime/fFactor+0.25f)%2 )
        {
            kTemp.FromEulerAnglesZXY (Mathf::FAbs((float)i-1.5f)*fAngle,
                0,0);
        }
        else
        {
            kTemp.FromEulerAnglesZXY (((float)i-1.5f)*fAngle,
                0,0);
        }

        m_akSkinMat[i][0][0] = kTemp[0][0];
        m_akSkinMat[i][0][1] = kTemp[0][1];
        m_akSkinMat[i][0][2] = kTemp[0][2];
        m_akSkinMat[i][0][3] = 0.0f;

        m_akSkinMat[i][1][0] = kTemp[1][0];
        m_akSkinMat[i][1][1] = kTemp[1][1];
        m_akSkinMat[i][1][2] = kTemp[1][2];
        m_akSkinMat[i][1][3] = 10*Mathf::Sin(m_fTime+0.5f*(float)i);

        m_akSkinMat[i][2][0] = kTemp[2][0];
        m_akSkinMat[i][2][1] = kTemp[2][1];
        m_akSkinMat[i][2][2] = kTemp[2][2];
        m_akSkinMat[i][2][3] = 0.0f;

        m_akSkinMat[i][3][0] = 0.0f;
        m_akSkinMat[i][3][1] = 0.0f;
        m_akSkinMat[i][3][2] = 0.0f;
        m_akSkinMat[i][3][3] = 1.0f;
    }

    // Set the skinning matrices in the shader
    if ( m_bShaderEnabled )
    {
        m_spkTriMesh->GetVertexConst("SkinningMat[0]")->
            SetData(m_akSkinMat[0]);
        m_spkTriMesh->GetVertexConst("SkinningMat[1]")->
            SetData(m_akSkinMat[1]);
        m_spkTriMesh->GetVertexConst("SkinningMat[2]")->
            SetData(m_akSkinMat[2]);
        m_spkTriMesh->GetVertexConst("SkinningMat[3]")->
            SetData(m_akSkinMat[3]);
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
                "Load of skinning.wvs failed." );
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
void Skinning::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }
    else if ( ucKey == ' ' )
    {
        m_bShaderEnabled = !m_bShaderEnabled;
        if ( m_bShaderEnabled )
        {
            m_spkTriMesh->SetVertexShader(m_spkVertShader);
        }
        else
        {
            m_spkTriMesh->SetVertexShader(NULL);
        }
    }
}
//----------------------------------------------------------------------------
