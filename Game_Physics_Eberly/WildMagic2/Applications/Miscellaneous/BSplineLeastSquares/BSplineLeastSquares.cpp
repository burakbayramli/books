// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "BSplineLeastSquares.h"
#include "WmlBSplineReduction.h"
#include "WmlBSplineCurve3.h"
#include <fstream>
using namespace std;

BSplineLeastSquares g_kTheApp;

//----------------------------------------------------------------------------
BSplineLeastSquares::BSplineLeastSquares ()
    :
    Application("BSplineLeastSquares",0,0,512,512,ColorRGB(1.0f,1.0f,1.0f))
{
    m_iDegree = 3;
}
//----------------------------------------------------------------------------
bool BSplineLeastSquares::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // create model
    m_spkScene = new Node(1);
    m_spkTrnNode = new Node(2);
    m_spkScene->AttachChild(m_spkTrnNode);

    // get control points for input curve
    ifstream kIStr("ControlPoints.txt");
    int iCtrlQuantity = 0;
    kIStr >> iCtrlQuantity;
    Vector3d* akCtrl = new Vector3d[iCtrlQuantity];
    for (int i = 0; i < iCtrlQuantity; i++)
    {
        kIStr >> akCtrl[i].X();
        kIStr >> akCtrl[i].Y();
        kIStr >> akCtrl[i].Z();
    }

    // create polyline connecting control points of input B-spline curve
    double dFraction = 0.10;
    Polyline* pkPoly = OriginalPolyline(iCtrlQuantity,akCtrl);
    m_spkTrnNode->AttachChild(pkPoly);

    // create polyline that approximates the reduced B-spline curve
    pkPoly = ReducedPolyline(iCtrlQuantity,akCtrl,dFraction);
    m_spkTrnNode->AttachChild(pkPoly);
    delete[] akCtrl;

    m_spkScene->UpdateGS(0.0f);
    Bound kWBound = m_spkScene->WorldBound();
    m_spkTrnNode->Translate() = -kWBound.Center();

    // sample camera code
    ms_spkCamera->SetFrustum(1.0f,10000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLeft(1.0f,0.0f,0.0f);
    Vector3f kCUp(0.0f,1.0f,0.0f);
    Vector3f kCDir(0.0f,0.0f,1.0f);
    Vector3f kCLoc = -3.0f*kWBound.Radius()*kCDir;
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    // camera turret and tumble mode
    m_spkMotionObject = m_spkScene;
    m_fTrnSpeed = 10.0f;
    m_fRotSpeed = 0.01f;
    m_bTurretActive = true;
    SetTurretAxes();

    return true;
}
//----------------------------------------------------------------------------
void BSplineLeastSquares::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkTrnNode = NULL;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void BSplineLeastSquares::OnIdle ()
{
    MeasureTime();
    MoveCamera();

    if ( MoveObject() )
        m_spkScene->UpdateGS(0.0f);

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
void BSplineLeastSquares::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }
}
//----------------------------------------------------------------------------
Polyline* BSplineLeastSquares::OriginalPolyline (int iCtrlQuantity,
    Vector3d* akCtrl)
{
    BSplineCurve3d kSpline(iCtrlQuantity,akCtrl,m_iDegree,false,true);

    Vector3f* akVertex = new Vector3f[iCtrlQuantity];
    ColorRGB* akColor = new ColorRGB[iCtrlQuantity];
    for (int i = 0; i < iCtrlQuantity; i++)
    {
        double dTime = i/(double)iCtrlQuantity;
        Vector3d kPos = kSpline.GetPosition(dTime);
        akVertex[i].X() = (float)kPos.X();
        akVertex[i].Y() = (float)kPos.Y();
        akVertex[i].Z() = (float)kPos.Z();
        akColor[i] = ColorRGB(1.0f,0.0f,0.0f);
    }

    return new Polyline(iCtrlQuantity,akVertex,NULL,akColor,NULL,false);
}
//----------------------------------------------------------------------------
Polyline* BSplineLeastSquares::ReducedPolyline (int iCtrlQuantity,
    Vector3d* akCtrl, double dFraction)
{
    int iLSCtrlQuantity;
    Vector3d* akLSCtrl;
    BSplineReduction3d(iCtrlQuantity,akCtrl,m_iDegree,dFraction,
        iLSCtrlQuantity,(Vector<3,double>*&)akLSCtrl);

    BSplineCurve3d kSpline(iLSCtrlQuantity,akLSCtrl,m_iDegree,false,true);
    delete[] akLSCtrl;

    Vector3f* akVertex = new Vector3f[iCtrlQuantity];
    ColorRGB* akColor = new ColorRGB[iCtrlQuantity];
    for (int i = 0; i < iCtrlQuantity; i++)
    {
        double dTime = i/(double)iCtrlQuantity;
        Vector3d kPos = kSpline.GetPosition(dTime);
        akVertex[i].X() = (float)kPos.X();
        akVertex[i].Y() = (float)kPos.Y();
        akVertex[i].Z() = (float)kPos.Z();
        akColor[i] = ColorRGB(0.0f,0.0f,1.0f);
    }

    return new Polyline(iCtrlQuantity,akVertex,NULL,akColor,NULL,false);
}
//----------------------------------------------------------------------------
