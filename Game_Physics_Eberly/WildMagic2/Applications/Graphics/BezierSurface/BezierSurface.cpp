// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "BezierSurface.h"

BezierSurface g_kTheApp;

//----------------------------------------------------------------------------
BezierSurface::BezierSurface ()
    :
    Application("BezierSurface",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
}
//----------------------------------------------------------------------------
BezierMesh* BezierSurface::CreateRectangleMesh (int iDegree, bool bUseNormals,
    bool bUseColors, bool bUseTextures)
{
    int iCtrlQuantity = (iDegree+1)*(iDegree+1);
    int* aiIndex = new int[iCtrlQuantity];
    int i;
    for (i = 0; i < iCtrlQuantity; i++)
        aiIndex[i] = i;

    Vector3f* akVertex = new Vector3f[iCtrlQuantity];

    ColorRGB* akColor;
    if ( bUseColors )
        akColor = new ColorRGB[iCtrlQuantity];
    else
        akColor = NULL;

    Vector2f* akTexture;
    if ( bUseTextures )
        akTexture = new Vector2f[iCtrlQuantity];
    else
        akTexture = NULL;

    i = 0;
    for (int iY = 0; iY <= iDegree; iY++)
    {
        float fV = iY/float(iDegree);
        float fY = 2.0f*fV - 1.0f;
        for (int iX = 0; iX <= iDegree; iX++)
        {
            float fU = iX/float(iDegree);
            float fX = 2.0f*fU - 1.0f;

            akVertex[i].X() = fX;
            akVertex[i].Y() = fY;
            akVertex[i].Z() = 1.0f - fX*fX - fY*fY;

            if ( bUseColors )
                akColor[i] = ColorRGB(fU,0.0f,fV);

            if ( bUseTextures )
                akTexture[i] = Vector2f(fU,fV);

            i++;
        }
    }

    BezierPatchPtr* aspkPatch = new BezierPatchPtr[1];
    if ( iDegree == 2 )
        aspkPatch[0] = new BezierRectangle2(aiIndex);
    else
        aspkPatch[0] = new BezierRectangle3(aiIndex);

    BezierMesh* pkMesh = new BezierMesh(iCtrlQuantity,akVertex,
        bUseNormals,akColor,akTexture,1,aspkPatch);

    return pkMesh;
}
//----------------------------------------------------------------------------
BezierMesh* BezierSurface::CreateTriangleMesh (int iDegree,
    bool bUseNormals, bool bUseColors, bool bUseTextures)
{
    int iCtrlQuantity = (iDegree+1)*(iDegree+2)/2;
    int* aiIndex = new int[iCtrlQuantity];
    int i;
    for (i = 0; i < iCtrlQuantity; i++)
        aiIndex[i] = i;

    Vector3f* akVertex = new Vector3f[iCtrlQuantity];

    ColorRGB* akColor;
    if ( bUseColors )
        akColor = new ColorRGB[iCtrlQuantity];
    else
        akColor = NULL;

    Vector2f* akTexture;
    if ( bUseTextures )
        akTexture = new Vector2f[iCtrlQuantity];
    else
        akTexture = NULL;

    i = 0;
    for (int iY = 0; iY <= iDegree; iY++)
    {
        float fV = iY/float(iDegree);
        float fY = 2.0f*fV - 1.0f;
        for (int iX = 0; iX <= iDegree - iY; iX++)
        {
            float fU = iX/float(iDegree);
            float fX = 2.0f*fU - 1.0f;

            akVertex[i].X() = fX;
            akVertex[i].Y() = fY;
            akVertex[i].Z() = 1.0f - fX*fX - fY*fY;

            if ( bUseColors )
                akColor[i] = ColorRGB(fU,0.0f,fV);

            if ( bUseTextures )
                akTexture[i] = Vector2f(fU,fV);

            i++;
        }
    }

    BezierPatchPtr* aspkPatch = new BezierPatchPtr[1];
    if ( iDegree == 2 )
        aspkPatch[0] = new BezierTriangle2(aiIndex);
    else
        aspkPatch[0] = new BezierTriangle3(aiIndex);

    BezierMesh* pkMesh = new BezierMesh(iCtrlQuantity,akVertex,
        bUseNormals,akColor,akTexture,1,aspkPatch);

    return pkMesh;
}
//----------------------------------------------------------------------------
BezierMesh* BezierSurface::CreateCylinderMesh (int iDegree,
    bool bUseNormals, bool bUseColors, bool bUseTextures)
{
    int iCtrlQuantity = 2*(iDegree+1);
    int* aiIndex = new int[iCtrlQuantity];
    int i;
    for (i = 0; i < iCtrlQuantity; i++)
        aiIndex[i] = i;

    Vector3f* akVertex = new Vector3f[iCtrlQuantity];

    ColorRGB* akColor;
    if ( bUseColors )
        akColor = new ColorRGB[iCtrlQuantity];
    else
        akColor = 0;

    Vector2f* akTexture;
    if ( bUseTextures )
        akTexture = new Vector2f[iCtrlQuantity];
    else
        akTexture = 0;

    i = 0;
    for (int iY = 0; iY < 2; iY++)
    {
        float fV = float(iY);
        float fY = 2.0f*fV - 1.0f;
        for (int iX = 0; iX <= iDegree; iX++)
        {
            float fU = iX/float(iDegree);
            float fX = 2.0f*fU - 1.0f;

            akVertex[i].X() = fX;
            akVertex[i].Y() = fY;
            akVertex[i].Z() = 1.0f - fX*fX - fY*fY;

            if ( bUseColors )
                akColor[i] = ColorRGB(fU,0.0f,fV);

            if ( bUseTextures )
                akTexture[i] = Vector2f(fU,fV);

            i++;
        }
    }

    BezierPatchPtr* aspkPatch = new BezierPatchPtr[1];
    if ( iDegree == 2 )
        aspkPatch[0] = new BezierCylinder2(aiIndex);
    else
        aspkPatch[0] = new BezierCylinder3(aiIndex);

    BezierMesh* pkMesh = new BezierMesh(iCtrlQuantity,akVertex,
        bUseNormals,akColor,akTexture,1,aspkPatch);

    return pkMesh;
}
//----------------------------------------------------------------------------
bool BezierSurface::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // set up camera
    ms_spkCamera->SetFrustum(1.0f,100.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLoc(-2.78f,0.00f,3.17f);
    Vector3f kCLeft(-0.79f,0.0f,-0.61f);
    Vector3f kCUp(0.0f,1.0f,0.0f);
    Vector3f kCDir(0.61f,0.0f,-0.79f);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // set up scene graph
    m_spkScene = new Node;

    bool bUseN = false;
    bool bUseC = true;
    bool bUseT = false;

    if ( bUseT )
    {
        unsigned int* auiData = new unsigned int[32*32];
        for (int i = 0; i < 32*32; i++)
        {
            auiData[i] =
                (rand() % 256) |
                ((rand() % 256) << 8) |
                ((rand() % 256) << 16);
        }
        Image* pkImage = new Image(Image::IT_RGBA8888,32,32,
            (unsigned char*)auiData);

        m_spkTexture = new Texture;
        m_spkTexture->SetImage(pkImage);
        m_spkTexture->Mipmap() = Texture::MM_NONE;
        m_spkTextureState = new TextureState;
        m_spkTextureState->Set(0,m_spkTexture);
        m_spkScene->SetRenderState(m_spkTextureState);
    }

    if ( GetCommand() )
    {
        if ( GetCommand()->Boolean("t") )
        {
            m_spkMesh = CreateTriangleMesh(2,bUseN,bUseC,bUseT);
        }
        else if ( GetCommand()->Boolean("r") )
        {
            m_spkMesh = CreateRectangleMesh(3,bUseN,bUseC,bUseT);
        }
        else if ( GetCommand()->Boolean("c") )
        {
            m_spkMesh = CreateCylinderMesh(2,bUseN,bUseC,bUseT);
        }
        else
        {
            m_spkMesh = CreateTriangleMesh(2,bUseN,bUseC,bUseT);
        }
    }
    else
    {
        m_spkMesh = CreateTriangleMesh(2,bUseN,bUseC,bUseT);
    }

    m_spkScene->AttachChild(m_spkMesh);

    m_spkWireframeState = new WireframeState;
    m_spkScene->SetRenderState(m_spkWireframeState);

    m_spkZBufferState = new ZBufferState;
    m_spkZBufferState->Enabled() = true;
    m_spkZBufferState->Writeable() = true;
    m_spkZBufferState->Compare() = ZBufferState::CF_LEQUAL;
    m_spkScene->SetRenderState(m_spkZBufferState);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_bTurretActive = true;
    SetTurretAxes();
    m_fTrnSpeed = 0.1f;
    m_fRotSpeed = 0.01f;

    return true;
}
//----------------------------------------------------------------------------
void BezierSurface::OnTerminate ()
{
    m_spkMesh = NULL;
    m_spkTexture = NULL;
    m_spkTextureState = NULL;
    m_spkWireframeState = NULL;
    m_spkZBufferState = NULL;
    m_spkScene = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void BezierSurface::OnIdle ()
{
    MeasureTime();
    MoveCamera();

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
void BezierSurface::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    int iLevel, iP;

    switch ( ucKey )
    {
    case '0':  // reset frame rate measurements
        ResetTime();
        return;
    case 'w':
        m_spkWireframeState->Enabled() = !m_spkWireframeState->Enabled();
        return;
    case '+':
        iLevel = m_spkMesh->GetTessellationLevel();
        if ( iLevel < 8 )
            m_spkMesh->Tessellate(iLevel+1);
        return;
    case '-':
        iLevel = m_spkMesh->GetTessellationLevel();
        if ( iLevel > 0 )
            m_spkMesh->Tessellate(iLevel-1);
        return;
    case '<':
        for (iP = 0; iP < m_spkMesh->GetPatchQuantity(); iP++)
        {
            BezierPatchPtr spPatch = m_spkMesh->Patch(iP);
            if ( spPatch->GetType() == BezierPatch::PT_CYLINDER )
            {
                BezierCylinder* pkCyln =
                    WmlSmartPointerCast(BezierCylinder,spPatch);
                if ( pkCyln->CylinderLevel() > 0 )
                {
                    pkCyln->CylinderLevel()--;
                    m_spkMesh->Tessellate(m_spkMesh->GetTessellationLevel());
                }
            }
        }
        return;
    case '>':
        for (iP = 0; iP < m_spkMesh->GetPatchQuantity(); iP++)
        {
            BezierPatchPtr spPatch = m_spkMesh->Patch(iP);
            if ( spPatch->GetType() == BezierPatch::PT_CYLINDER )
            {
                BezierCylinder* pkCyln =
                    WmlSmartPointerCast(BezierCylinder,spPatch);
                if ( pkCyln->CylinderLevel() < 8 )
                {
                    pkCyln->CylinderLevel()++;
                    m_spkMesh->Tessellate(m_spkMesh->GetTessellationLevel());
                }
            }
        }
        return;
    }
}
//----------------------------------------------------------------------------
void BezierSurface::OnMouseClick (int iButton, int iState, int iX, int iY,
    unsigned int uiModifiers)
{
    if ( iButton != MOUSE_LEFT_BUTTON || iState != MOUSE_DOWN )
        return;

    Vector3f kOrigin, kDirection;
    ms_spkCamera->GetPickRay(iX,iY,GetWidth(),GetHeight(),kOrigin,kDirection);

    // determine if the mesh or background was picked
    Spatial::PickArray kResults;
    m_spkScene->DoPick(kOrigin,kDirection,kResults);

    float fRayT;
    ColorRGB* akColor = new ColorRGB[1];

    if ( kResults.size() > 0 )
    {
        // The mesh was picked.  Increase the tessellation level if a shift
        // key is simultaneously pressed with the left button.  Decrease the
        // tessellation level otherwise.
        int iLevel;
        if ( uiModifiers & KEY_SHIFT )
        {
            iLevel = m_spkMesh->GetTessellationLevel();
            if ( iLevel < 8 )
                m_spkMesh->Tessellate(iLevel+1);
        }
        else
        {
            iLevel = m_spkMesh->GetTessellationLevel();
            if ( iLevel > 0 )
                m_spkMesh->Tessellate(iLevel-1);
        }

        // Create a point slightly in front of the mesh where the mouse was
        // clicked.
        BezierMesh::PickRecord* pkRec = (BezierMesh::PickRecord*)kResults[0];
        fRayT = 0.99f*pkRec->m_fRayT;
        akColor[0].r = 1.0f;
        akColor[0].g = 1.0f;
        akColor[0].b = 1.0f;

        for (int i = 0; i < (int)kResults.size(); i++)
            delete kResults[i];
    }
    else
    {
        // The background was picked.  Draw a single point where the mouse was
        // clicked.
        fRayT = 0.5f*(ms_spkCamera->GetFrustumNear() +
            ms_spkCamera->GetFrustumFar());
        akColor[0].r = Mathf::UnitRandom();
        akColor[0].g = Mathf::UnitRandom();
        akColor[0].b = Mathf::UnitRandom();
    }

    Vector3f* akVertex = new Vector3f[1];
    akVertex[0] = kOrigin + fRayT*kDirection;

    Polypoint* pkPoints = new Polypoint(1,akVertex,NULL,akColor,NULL);
    m_spkScene->AttachChild(pkPoints);
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();
}
//----------------------------------------------------------------------------
