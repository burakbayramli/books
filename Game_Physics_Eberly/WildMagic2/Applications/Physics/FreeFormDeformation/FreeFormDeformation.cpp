// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "FreeFormDeformation.h"

FreeFormDeformation g_kTheApp;

//----------------------------------------------------------------------------
FreeFormDeformation::FreeFormDeformation ()
    :
    Application("FreeFormDeformation",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
    m_iQuantity = 4;
    m_iDegree = 3;
    m_pkVolume = NULL;
    m_akParameter = NULL;
    m_bDoRandom = false;
    m_fAmplitude = 0.01f;
    m_fRadius = 0.25f;
    m_fLastUpdateTime = GetTimeInSeconds();
    m_bMouseDown = false;
}
//----------------------------------------------------------------------------
FreeFormDeformation::~FreeFormDeformation ()
{
}
//----------------------------------------------------------------------------
bool FreeFormDeformation::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // Set up the scene graph.  The TrnNode objects allows the application
    // to rotate the scene about its center.  Global wireframe and depth
    // buffering is used.
    m_spkScene = new Node(1);
    m_spkTrnNode = new Node(2);
    m_spkScene->AttachChild(m_spkTrnNode);
    m_spkWireframeState = new WireframeState;
    m_spkScene->SetRenderState(m_spkWireframeState);
    m_spkZBufferState = new ZBufferState;
    m_spkZBufferState->Enabled() = true;
    m_spkZBufferState->Writeable() = true;
    m_spkZBufferState->Compare() = ZBufferState::CF_LEQUAL;
    m_spkScene->SetRenderState(m_spkZBufferState);

    Setup();

    // center-and-fit mesh for viewing by camera
    m_spkMesh->UpdateGS(0.0f);
    Bound kWBound = m_spkMesh->WorldBound();
    m_spkTrnNode->Translate() = -kWBound.Center();

    // set up camera
    ms_spkCamera->SetFrustum(0.1f,100.0f,-0.055f,0.055f,0.04125f,-0.04125f);
    Vector3f kCLeft(1.0f,0.0f,0.0f);
    Vector3f kCUp(0.0f,1.0f,0.0f);
    Vector3f kCDir(0.0f,0.0f,1.0f);
    Vector3f kCLoc = -4.0f*kWBound.Radius()*kCDir;
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_spkMotionObject = m_spkScene;
    m_bTurretActive = true;
    SetTurretAxes();
    m_fTrnSpeed = 0.01f;
    m_fRotSpeed = 0.02f;

    return true;
}
//----------------------------------------------------------------------------
void FreeFormDeformation::OnTerminate ()
{
    delete m_pkVolume;
    delete[] m_akParameter;

    m_spkScene = NULL;
    m_spkTrnNode = NULL;
    m_spkWireframeState = NULL;
    m_spkZBufferState = NULL;
    m_spkMesh = NULL;
    m_spkPolylineRoot = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void FreeFormDeformation::OnIdle ()
{
    MeasureTime();

    MoveCamera();
    if ( m_bDoRandom )
    {
        // deform the mesh no faster than 30 frames per second
        float fTime = GetTimeInSeconds();
        if ( fTime - m_fLastUpdateTime >= 0.0333333f )
        {
            m_fLastUpdateTime = fTime;
            DoRandomControlPoints();
            m_spkScene->UpdateGS(0.0f);
        }
    }

    if ( MoveObject() )
        m_spkScene->UpdateGS(0.0f);

    // draw the scene as fast as possible (not limited to 30 fps)
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
void FreeFormDeformation::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    switch ( ucKey )
    {
    case 'w':  // toggle wireframe
    case 'W':
        m_spkWireframeState->Enabled() = !m_spkWireframeState->Enabled();
        break;
    case 'r':  // toggle automated random versus user-adjusted controls
    case 'R':
        m_bDoRandom = !m_bDoRandom;
        break;
    case 'c':  // toggle whether or not the polylines/boxes are drawn
    case 'C':
        m_spkPolylineRoot->ForceCull() = !m_spkPolylineRoot->ForceCull();
        m_spkControlRoot->ForceCull() = !m_spkControlRoot->ForceCull();
        break;
    }
}
//----------------------------------------------------------------------------
void FreeFormDeformation::OnMouseClick (int iButton, int iState, int iX,
    int iY, unsigned int)
{
    if ( m_bDoRandom || iButton != MOUSE_LEFT_BUTTON )
        return;

    if ( iState == MOUSE_DOWN )
    {
        m_bMouseDown = true;
        OnMouseDown(iX,iY);
    }
    else
    {
        m_bMouseDown = false;
    }
}
//----------------------------------------------------------------------------
void FreeFormDeformation::OnMotion (int iX, int iY, unsigned int)
{
    if ( m_bDoRandom || !m_bMouseDown || !m_spkSelected )
        return;

    OnMouseMove(iX,iY);
}
//----------------------------------------------------------------------------
void FreeFormDeformation::OnMouseDown (int iX, int iY)
{
    // the current selected control point is deactivated
    if ( m_spkSelected )
    {
        m_spkSelected->SetRenderState(m_spkControlInactive);
        m_spkSelected->UpdateRS();
        m_spkSelected = NULL;
    }

    // construct the pick ray
    Vector3f kOrigin, kDirection;
    ms_spkCamera->GetPickRay(iX,iY,GetWidth(),GetHeight(),kOrigin,kDirection);

    // determine which control point has been selected (if any)
    Spatial::PickArray kResults;
    m_spkControlRoot->DoPick(kOrigin,kDirection,kResults);
    if ( kResults.size() > 0 )
    {
        // find the closest selected object
        float fTMin = kResults[0]->m_fRayT;
        SpatialPtr spkCtrl = kResults[0]->m_spkObject;
        delete kResults[0];
        if ( kResults.size() > 1 )
        {
            for (int i = 1; i < (int)kResults.size(); i++)
            {
                if ( kResults[i]->m_fRayT < fTMin )
                {
                    fTMin = kResults[i]->m_fRayT;
                    spkCtrl = kResults[i]->m_spkObject;
                }
                delete kResults[i];
            }
        }
        m_spkSelected = WmlSmartPointerCast(TriMesh,spkCtrl);
        m_spkSelected->SetRenderState(m_spkControlActive);
        m_spkSelected->UpdateRS();
    }
}
//----------------------------------------------------------------------------
void FreeFormDeformation::OnMouseMove (int iX, int iY)
{
    // Construct a pick ray, but this time we want to move the control
    // point from its current location to that ray.  The motion is in the
    // plane spanned by camera left/up vectors.  The new location is the
    // intersection of the pick ray and the plane Dot(D,X-P) = 0 where P
    // is the old control point location and where D is the camera direction
    // vector.
    Vector3f kOrigin, kDirection;
    ms_spkCamera->GetPickRay(iX,iY,GetWidth(),GetHeight(),kOrigin,kDirection);

    // The new world position is at a distance along the pick ray that is
    // equal to the distance from the camera location to the old world
    // position.
    Vector3f kOldWorldPos = m_spkSelected->WorldTranslate();
    Vector3f kDiff = kOldWorldPos - ms_spkCamera->GetLocation();
    Vector3f kNewWorldPos = kOrigin + kDiff.Length()*kDirection;

    // Move the control point to the new world location.  The technical
    // problem is that we need to modify the world coordinates for the
    // selected control point.  Thus, we need to determine how to change the
    // local translation in order to cause the correct world translation.
    Node* pkPar = m_spkSelected->GetParent();
    kDiff = kNewWorldPos - kOldWorldPos;
    Vector3f kLocalDiff = (kDiff*pkPar->WorldRotate())/pkPar->WorldScale();
    m_spkSelected->Translate() += kLocalDiff;
    m_spkSelected->UpdateGS(0.0f);

    // modify the control point itself
    int i0, i1, i2;
    sscanf(m_spkSelected->GetName(),"%d %d %d",&i0,&i1,&i2);
    m_pkVolume->ControlPoint(i0,i1,i2) = m_spkSelected->Translate();

    // TO DO.  Only need to update mesh vertices that are affected by the
    // change in one control point.  This requires working with the B-spline
    // basis function and knowing which (u,v,w) to evaluate at (i.e. determine
    // the local control region).
    UpdateMesh();

    // TO DO.  Only need to update neighboring lines.
    UpdatePolylines();
}
//----------------------------------------------------------------------------
void FreeFormDeformation::Setup ()
{
    Stream kInfile;
    kInfile.Load("QuartzBrain.mgc");
    m_spkMesh = (TriMesh*)kInfile.GetObjectAt(0);
    m_spkTrnNode->AttachChild(m_spkMesh);

    CreateBSplineVolume();
    CreatePolylines();
    CreateControlBoxes();
}
//----------------------------------------------------------------------------
void FreeFormDeformation::CreateBSplineVolume ()
{
    // generate the B-spline volume function
    m_pkVolume = new BSplineVolumef(m_iQuantity,m_iQuantity,m_iQuantity,
        m_iDegree,m_iDegree,m_iDegree);

    // get AABB of form [xmin,xmax]x[ymin,ymax]x[zmin,zmax]
    int iVQuantity = m_spkMesh->GetVertexQuantity();
    Vector3f* akVertex = m_spkMesh->Vertices();
    m_fXMin = akVertex[0].X();
    m_fYMin = akVertex[0].Y();
    m_fZMin = akVertex[0].Z();
    float fXMax = m_fXMin, fYMax = m_fYMin, fZMax = m_fZMin;
    int i;
    for (i = 0; i < iVQuantity; i++)
    {
        if ( akVertex[i].X() < m_fXMin )
            m_fXMin = akVertex[i].X();
        else if ( akVertex[i].X() > fXMax )
            fXMax = akVertex[i].X();

        if ( akVertex[i].Y() < m_fYMin )
            m_fYMin = akVertex[i].Y();
        else if ( akVertex[i].Y() > fYMax )
            fYMax = akVertex[i].Y();

        if ( akVertex[i].Z() < m_fZMin )
            m_fZMin = akVertex[i].Z();
        else if ( akVertex[i].Z() > fZMax )
            fZMax = akVertex[i].Z();
    }

    // generate the control points
    float fXRange = fXMax - m_fXMin;
    float fYRange = fYMax - m_fYMin;
    float fZRange = fZMax - m_fZMin;
    m_fDX = fXRange/(float)(m_iQuantity-1);
    m_fDY = fYRange/(float)(m_iQuantity-1);
    m_fDZ = fZRange/(float)(m_iQuantity-1);
    Vector3f kCtrl;
    int i0, i1, i2;
    for (i0 = 0; i0 < m_iQuantity; i0++)
    {
        kCtrl.X() = m_fXMin + m_fDX*i0;
        for (i1 = 0; i1 < m_iQuantity; i1++)
        {
            kCtrl.Y() = m_fYMin + m_fDY*i1;
            for (i2 = 0; i2 < m_iQuantity; i2++)
            {
                kCtrl.Z() = m_fZMin + m_fDZ*i2;
                m_pkVolume->ControlPoint(i0,i1,i2) = kCtrl;
            }
        }
    }

    // compute the (u,v,w) values of the mesh relative to the B-spline volume
    float fInvXRange = 1.0f/fXRange;
    float fInvYRange = 1.0f/fYRange;
    float fInvZRange = 1.0f/fZRange;
    m_akParameter = new Vector3f[iVQuantity];
    for (i = 0; i < iVQuantity; i++)
    {
        m_akParameter[i].X() = (akVertex[i].X() - m_fXMin)*fInvXRange;
        m_akParameter[i].Y() = (akVertex[i].Y() - m_fYMin)*fInvYRange;
        m_akParameter[i].Z() = (akVertex[i].Z() - m_fZMin)*fInvZRange;
    }
}
//----------------------------------------------------------------------------
void FreeFormDeformation::CreatePolylines ()
{
    // generate the polylines that connect adjacent control points
    m_spkPolylineRoot = new Node(3*m_iQuantity*m_iQuantity*(m_iQuantity-1));
    m_spkTrnNode->AttachChild(m_spkPolylineRoot);

    Vector3f* akPVertex;
    ColorRGB* akPColor;
    Polyline* pkPoly;
    int i0, i1, i2;

    for (i0 = 0; i0 < m_iQuantity; i0++)
    {
        for (i1 = 0; i1 < m_iQuantity; i1++)
        {
            for (i2 = 0; i2 < m_iQuantity-1; i2++)
            {
                akPVertex = new Vector3f[2];
                akPVertex[0] = m_pkVolume->ControlPoint(i0,i1,i2);
                akPVertex[1] = m_pkVolume->ControlPoint(i0,i1,i2+1);
                akPColor = new ColorRGB[2];
                akPColor[0] = ColorRGB(0.0f,0.0f,0.75f);
                akPColor[1] = ColorRGB(0.0f,0.0f,0.75f);
                pkPoly = new Polyline(2,akPVertex,NULL,akPColor,NULL,false);
                m_spkPolylineRoot->AttachChild(pkPoly);
            }
        }

        for (i2 = 0; i2 < m_iQuantity; i2++)
        {
            for (i1 = 0; i1 < m_iQuantity-1; i1++)
            {
                akPVertex = new Vector3f[2];
                akPVertex[0] = m_pkVolume->ControlPoint(i0,i1,i2);
                akPVertex[1] = m_pkVolume->ControlPoint(i0,i1+1,i2);
                akPColor = new ColorRGB[2];
                akPColor[0] = ColorRGB(0.0f,0.75f,0.0f);
                akPColor[1] = ColorRGB(0.0f,0.75f,0.0f);
                pkPoly = new Polyline(2,akPVertex,NULL,akPColor,NULL,false);
                m_spkPolylineRoot->AttachChild(pkPoly);
            }
        }
    }

    for (i0 = 0; i0 < m_iQuantity-1; i0++)
    {
        for (i1 = 0; i1 < m_iQuantity; i1++)
        {
            for (i2 = 0; i2 < m_iQuantity; i2++)
            {
                akPVertex = new Vector3f[2];
                akPVertex[0] = m_pkVolume->ControlPoint(i0,i1,i2);
                akPVertex[1] = m_pkVolume->ControlPoint(i0+1,i1,i2);
                akPColor = new ColorRGB[2];
                akPColor[0] = ColorRGB(0.75f,0.0f,0.0f);
                akPColor[1] = ColorRGB(0.75f,0.0f,0.0f);
                pkPoly = new Polyline(2,akPVertex,NULL,akPColor,NULL,false);
                m_spkPolylineRoot->AttachChild(pkPoly);
            }
        }
    }
}
//----------------------------------------------------------------------------
void FreeFormDeformation::CreateControlBoxes ()
{
    // generate small boxes to represent the control points
    m_spkControlRoot = new Node(m_iQuantity*m_iQuantity*m_iQuantity);
    m_spkTrnNode->AttachChild(m_spkControlRoot);

    // create a single box to be copied by each control point box
    const float fHalfWidth = 0.02f;
    Vector3f akVertex[8] =
    {
        Vector3f(-fHalfWidth,-fHalfWidth,-fHalfWidth),
        Vector3f(+fHalfWidth,-fHalfWidth,-fHalfWidth),
        Vector3f(+fHalfWidth,+fHalfWidth,-fHalfWidth),
        Vector3f(-fHalfWidth,+fHalfWidth,-fHalfWidth),
        Vector3f(-fHalfWidth,-fHalfWidth,+fHalfWidth),
        Vector3f(+fHalfWidth,-fHalfWidth,+fHalfWidth),
        Vector3f(+fHalfWidth,+fHalfWidth,+fHalfWidth),
        Vector3f(-fHalfWidth,+fHalfWidth,+fHalfWidth),
    };

    int aiConnect[36] =
    {
        0, 2, 1,
        0, 3, 2,
        4, 5, 6,
        4, 6, 7,
        0, 5, 4,
        0, 1, 5,
        3, 7, 6,
        3, 6, 2,
        1, 2, 6,
        1, 6, 5,
        0, 4, 7,
        0, 7, 3
    };

    // create the materials and light to be attached to each box
    m_spkControlActive = new MaterialState;
    m_spkControlActive->Emissive() = ColorRGB::BLACK;
    m_spkControlActive->Ambient() = ColorRGB(1.0f,0.0f,0.0f);
    m_spkControlActive->Diffuse() = ColorRGB(1.0f,0.0f,0.0f);
    m_spkControlActive->Specular() = ColorRGB::BLACK;
    m_spkControlActive->Shininess() = 1.0f;
    m_spkControlActive->Alpha() = 1.0f;

    m_spkControlInactive = new MaterialState;
    m_spkControlInactive->Emissive() = ColorRGB::BLACK;
    m_spkControlInactive->Ambient() = ColorRGB(0.75f,0.75f,0.75f);
    m_spkControlInactive->Diffuse() = ColorRGB(0.75f,0.75f,0.75f);
    m_spkControlInactive->Specular() = ColorRGB::BLACK;
    m_spkControlInactive->Shininess() = 1.0f;
    m_spkControlInactive->Alpha() = 1.0f;

    AmbientLight* pkALight = new AmbientLight;
    pkALight->Ambient() = ColorRGB::WHITE;
    pkALight->Diffuse() =ColorRGB::WHITE;
    pkALight->Specular() = ColorRGB::BLACK;
    pkALight->On() = true;
    m_spkControlLight = new LightState;
    m_spkControlLight->Attach(pkALight);

    int i0, i1, i2;
    for (i0 = 0; i0 < m_iQuantity; i0++)
    {
        for (i1 = 0; i1 < m_iQuantity; i1++)
        {
            for (i2 = 0; i2 < m_iQuantity; i2++)
            {
                Vector3f* akBVertex = new Vector3f[8];
                memcpy(akBVertex,akVertex,8*sizeof(Vector3f));
                int* aiBConnect = new int[36];
                memcpy(aiBConnect,aiConnect,36*sizeof(int));

                TriMesh* pkBox = new TriMesh(8,akBVertex,NULL,NULL,NULL,12,
                    aiBConnect);
                pkBox->Translate() = m_pkVolume->ControlPoint(i0,i1,i2);

                // Encode the indices in the name for later use.  This will
                // allow fast lookup of volume control points.
                char acName[32];
                sprintf(acName,"%d %d %d",i0,i1,i2);
                pkBox->SetName(acName);

                pkBox->SetRenderState(m_spkControlInactive);
                pkBox->SetRenderState(m_spkControlLight);

                m_spkControlRoot->AttachChild(pkBox);
            }
        }
    }
}
//----------------------------------------------------------------------------
void FreeFormDeformation::UpdateMesh ()
{
    // Update the mesh points.  Typically you would update the model bound
    // after modifying the vertices, but in this application the mesh is
    // always visible, so an accurate model bound for culling purposes is
    // not necessary--the model bound is never updated.

    int iVQuantity = m_spkMesh->GetVertexQuantity();
    Vector3f* akVertex = m_spkMesh->Vertices();
    for (int i = 0; i < iVQuantity; i++)
    {
        akVertex[i] = m_pkVolume->GetPosition(m_akParameter[i].X(),
            m_akParameter[i].Y(),m_akParameter[i].Z());
    }

    // Typical calls after modification, but not in this application.
    // m_spkMesh->UpdateModelBound();
    // m_spkMesh->UpdateModelNormals();  // if lighting affects mesh
}
//----------------------------------------------------------------------------
void FreeFormDeformation::UpdatePolylines ()
{
    // update the polyline mesh
    Vector3f* akPVertex;
    Polyline* pkPoly;
    int i0, i1, i2, i = 0;

    for (i0 = 0; i0 < m_iQuantity; i0++)
    {
        for (i1 = 0; i1 < m_iQuantity; i1++)
        {
            for (i2 = 0; i2 < m_iQuantity-1; i2++)
            {
                pkPoly = WmlSmartPointerCast(Polyline,
                    m_spkPolylineRoot->GetChild(i));
                akPVertex = pkPoly->Vertices();
                akPVertex[0] = m_pkVolume->ControlPoint(i0,i1,i2);
                akPVertex[1] = m_pkVolume->ControlPoint(i0,i1,i2+1);
                i++;
            }
        }

        for (i2 = 0; i2 < m_iQuantity; i2++)
        {
            for (i1 = 0; i1 < m_iQuantity-1; i1++)
            {
                pkPoly = WmlSmartPointerCast(Polyline,
                    m_spkPolylineRoot->GetChild(i));
                akPVertex = pkPoly->Vertices();
                akPVertex[0] = m_pkVolume->ControlPoint(i0,i1,i2);
                akPVertex[1] = m_pkVolume->ControlPoint(i0,i1+1,i2);
                i++;
            }
        }
    }

    for (i0 = 0; i0 < m_iQuantity-1; i0++)
    {
        for (i1 = 0; i1 < m_iQuantity; i1++)
        {
            for (i2 = 0; i2 < m_iQuantity; i2++)
            {
                pkPoly = WmlSmartPointerCast(Polyline,
                    m_spkPolylineRoot->GetChild(i));
                akPVertex = pkPoly->Vertices();
                akPVertex[0] = m_pkVolume->ControlPoint(i0,i1,i2);
                akPVertex[1] = m_pkVolume->ControlPoint(i0+1,i1,i2);
                i++;
            }
        }
    }
}
//----------------------------------------------------------------------------
void FreeFormDeformation::UpdateControlBoxes ()
{
    int i0, i1, i2, i = 0;
    for (i0 = 0; i0 < m_iQuantity; i0++)
    {
        for (i1 = 0; i1 < m_iQuantity; i1++)
        {
            for (i2 = 0; i2 < m_iQuantity; i2++)
            {
                TriMesh* pkBox = WmlSmartPointerCast(TriMesh,
                    m_spkControlRoot->GetChild(i));
                pkBox->Translate() = m_pkVolume->ControlPoint(i0,i1,i2);
                i++;
            }
        }
    }
}
//----------------------------------------------------------------------------
void FreeFormDeformation::DoRandomControlPoints ()
{
    // Randomly perturb the control points, but stay near the original
    // control points.
    int i0, i1, i2;
    Vector3f kCtrl;
    for (i0 = 0; i0 < m_iQuantity; i0++)
    {
        kCtrl.X() = m_fXMin + m_fDX*i0;
        for (i1 = 0; i1 < m_iQuantity; i1++)
        {
            kCtrl.Y() = m_fYMin + m_fDY*i1;
            for (i2 = 0; i2 < m_iQuantity; i2++)
            {
                kCtrl.Z() = m_fZMin + m_fDZ*i2;

                Vector3f kNewCtrl = m_pkVolume->ControlPoint(i0,i1,i2) +
                    m_fAmplitude*Vector3f(Mathf::SymmetricRandom(),
                    Mathf::SymmetricRandom(),Mathf::SymmetricRandom());

                Vector3f kDiff = kNewCtrl - kCtrl;
                float fLength = kDiff.Length();
                if ( fLength > m_fRadius )
                    kDiff *= m_fRadius/fLength;

                m_pkVolume->ControlPoint(i0,i1,i2) = kCtrl + kDiff;
            }
        }
    }

    UpdateMesh();
    UpdatePolylines();
    UpdateControlBoxes();
}
//----------------------------------------------------------------------------
