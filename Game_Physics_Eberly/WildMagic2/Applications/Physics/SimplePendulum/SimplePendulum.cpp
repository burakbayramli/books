// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "WmlRasterDrawing.h"
#include "WmlImages.h"
#include "WmlIntegrate1.h"
#include "WmlMath.h"
using namespace Wml;
using namespace std;

// The equation of motion is X" + K*sin(X) = 0, X(0) = X0, X'(0) = V0 where
// X(t) is the angle formed by the pendulum rod with the vertical.  If
// Y(t) = X'(t), then the system of differential equations is
//
//   X' = Y,  X(0) = X0
//   Y' = -K*sin(X),  Y(0) = V0
//
const float g_fK = 1.0f;

// for saving graphs of the solution to the differential equations
const int g_iN = 512;
ImageRGB82D g_kImage(g_iN,g_iN);
unsigned int g_uiColor = 0;
void DrawPixel (int iX, int iY)
{
    if ( 0 <= iX && iX < g_iN && 0 <= iY && iY < g_iN )
        g_kImage(iX,iY) = g_uiColor;
}

//----------------------------------------------------------------------------
float* ExplicitEuler (float fX0, float fY0, float fH, int g_iN)
{
    float* afExplicit = new float[g_iN];
    for (int i = 0; i < g_iN; i++)
    {
        float fX1 = fX0 + fH*fY0;
        float fY1 = fY0 - fH*g_fK*Mathf::Sin(fX0);

        afExplicit[i] = fX1;
        fX0 = fX1;
        fY0 = fY1;
    }
    return afExplicit;
}
//----------------------------------------------------------------------------
float* ImplicitEuler (float fX0, float fY0, float fH, int g_iN)
{
    const float fK0 = g_fK*fH*fH;

    float* afImplicit = new float[g_iN];
    for (int i = 0; i < g_iN; i++)
    {
        float fK1 = fX0 + fH*fY0;
        float fX1 = fX0;
        int jMaxIteration = 32;
        for (int j = 0; j < jMaxIteration; j++)
        {
            float fG = fX1 + fK0*Mathf::Sin(fX1) - fK1;
            float fGDer = 1.0f + fK0*Mathf::Cos(fX1);
            fX1 -= fG/fGDer;
        }
        float fY1 = fY0 - fH*g_fK*Mathf::Sin(fX1);

        afImplicit[i] = fX1;
        fX0 = fX1;
        fY0 = fY1;
    }
    return afImplicit;
}
//----------------------------------------------------------------------------
float* RungeKutta (float fX0, float fY0, float fH, int g_iN)
{
    float* afRunge = new float[g_iN];
    for (int i = 0; i < g_iN; i++)
    {
        float fK1X = fH*fY0;
        float fK1Y = -fH*g_fK*Mathf::Sin(fX0);
        float fX1 = fX0 + 0.5f*fK1X;
        float fY1 = fY0 + 0.5f*fK1Y;
        float fK2X = fH*fY1;
        float fK2Y = -fH*g_fK*Mathf::Sin(fX1);
        fX1 = fX0 + 0.5f*fK2X;
        fY1 = fY0 + 0.5f*fK2Y;
        float fK3X = fH*fY1;
        float fK3Y = -fH*g_fK*Mathf::Sin(fX1);
        fX1 = fX0 + fK3X;
        fY1 = fY0 + fK3Y;
        float fK4X = fH*fY1;
        float fK4Y = -fH*g_fK*Mathf::Sin(fX1);
        fX1 = fX0 + (fK1X + 2.0f*fK2X + 2.0f*fK3X + fK4X)/6.0f;
        fY1 = fY0 + (fK1Y + 2.0f*fK2Y + 2.0f*fK3Y + fK4Y)/6.0f;

        afRunge[i] = fX1;
        fX0 = fX1;
        fY0 = fY1;
    }
    return afRunge;
}
//----------------------------------------------------------------------------
float* LeapFrog (float fX0, float fY0, float fH, int g_iN)
{
    float* afLeapFrog = new float[g_iN];

    // generate first iterate with Euler's to start up the process
    float fX1 = fX0 + fH*fY0;
    float fY1 = fY0 - fH*g_fK*Mathf::Sin(fX0);
    afLeapFrog[0] = fX1;

    for (int i = 1; i < g_iN; i++)
    {
        float fX2 = fX0 + 2.0f*fH*fY1;
        float fY2 = fY0 - 2.0f*fH*g_fK*Mathf::Sin(fX1);

        afLeapFrog[i] = fX2;
        fX0 = fX1;
        fY0 = fY1;
        fX1 = fX2;
        fY1 = fY2;
    }
    return afLeapFrog;
}
//----------------------------------------------------------------------------
void SolveMethod (float* (*oMethod)(float,float,float,int),
    const char* acOutIm, const char* acOutTxt)
{
    float fX0 = 0.1f, fY0 = 1.0f;
    float fH = 0.1f;

    float* afApprox = oMethod(fX0,fY0,fH,g_iN);
    ofstream kOutTxt(acOutTxt);
    int i;
    for (i = 0; i < g_iN; i++)
        kOutTxt << "i = " << i << ", " << afApprox[i] << endl;

    // set image to white
    for (i = 0; i < g_kImage.GetQuantity(); i++)
        g_kImage[i] = GetColor24(255,255,255);

    // draw the approximate solution
    float fY = 256.0f*(afApprox[0]+3.0f)/6.0f;
    int iY0 = g_iN-1-(int)fY;
    for (i = 1; i < g_iN; i++)
    {
        fY = 256.0f*(afApprox[i]+3.0f)/6.0f;
        int iY1 = g_iN-1-(int)fY;
        Line2D(i-1,iY0,i,iY1,DrawPixel);
        iY0 = iY1;
    }

    g_kImage.Save(acOutIm);

    delete[] afApprox;
}
//----------------------------------------------------------------------------
void Stiff1 ()
{
    const int iMaxIter = 1024+256;
    const float fCSqr = 2.0f, fC = Mathf::Sqrt(2.0f);

    float fH = 0.01f;
    float fX0 = 1.0f, fX0Save = fX0;
    float fY0 = -fC*fX0;

    float* afApprox = new float[iMaxIter];
    int i;
    for (i = 0; i < iMaxIter; i++)
    {
        float fK1X = fH*fY0;
        float fK1Y = fH*fCSqr*fX0;
        float fX1 = fX0 + 0.5f*fK1X;
        float fY1 = fY0 + 0.5f*fK1Y;
        float fK2X = fH*fY1;
        float fK2Y = fH*fCSqr*fX1;
        fX1 = fX0 + 0.5f*fK2X;
        fY1 = fY0 + 0.5f*fK2Y;
        float fK3X = fH*fY1;
        float fK3Y = fH*fCSqr*fX1;
        fX1 = fX0 + fK3X;
        fY1 = fY0 + fK3Y;
        float fK4X = fH*fY1;
        float fK4Y = fH*fCSqr*fX1;
        fX1 = fX0 + (fK1X + 2.0f*fK2X + 2.0f*fK3X + fK4X)/6.0f;
        fY1 = fY0 + (fK1Y + 2.0f*fK2Y + 2.0f*fK3Y + fK4Y)/6.0f;

        afApprox[i] = fX1;
        fX0 = fX1;
        fY0 = fY1;
    }

    ofstream kOutTxt("stiff1.txt");
    for (i = 0; i < iMaxIter; i++)
        kOutTxt << "i = " << i << ", " << afApprox[i] << endl;

    // set image to white
    for (i = 0; i < g_kImage.GetQuantity(); i++)
        g_kImage[i] = GetColor24(255,255,255);

    // draw the true solution
    float fY = 256.0f*(fX0Save+3.0f)/6.0f;
    int iY0 = g_iN-1-(int)fY;
    for (i = 1; i < g_iN; i++)
    {
        int j = (iMaxIter-1)*i/(g_iN-1);
        fY = 256.0f*(fX0Save*Mathf::Exp(-fC*j*fH)+3.0f)/6.0f;
        int iY1 = g_iN-1-(int)fY;
        Line2D(i-1,iY0,i,iY1,DrawPixel);
        iY0 = iY1;
    }

    g_kImage.Save("stiff1_true.im");

    // set image to white
    for (i = 0; i < g_kImage.GetQuantity(); i++)
        g_kImage[i] = GetColor24(255,255,255);

    // draw the approximate solution
    fY = 256.0f*(afApprox[0]+3.0f)/6.0f;
    iY0 = g_iN-1-(int)fY;
    for (i = 1; i < g_iN; i++)
    {
        int j = (iMaxIter-1)*i/(g_iN-1);
        fY = 256.0f*(afApprox[j]+3.0f)/6.0f;
        int iY1 = g_iN-1-(int)fY;
        Line2D(i-1,iY0,i,iY1,DrawPixel);
        iY0 = iY1;
    }
    g_kImage.Save("stiff1_appr.im");

    delete[] afApprox;

}
//----------------------------------------------------------------------------
float F0 (float fT, float fX, float fY)
{
    return 9.0f*fX + 24.0f*fY + 5.0f*Mathf::Cos(fT) - Mathf::Sin(fT)/3.0f;
}
//----------------------------------------------------------------------------
float F1 (float fT, float fX, float fY)
{
    return -24.0f*fX - 51.0f*fY - 9.0f*Mathf::Cos(fT) + Mathf::Sin(fT)/3.0f;
}
//----------------------------------------------------------------------------
void Stiff2True ()
{
    float fH = 0.05f;
    float fX0 = 4.0f/3.0f;
    float fY0 = 2.0f/3.0f;
    float fT0 = 0.0f;

    // true solution
    ofstream kOutTxt("stiff2_true.txt");
    char acMsg[512];
    const int iMaxIter = 20;
    for (int i = 0; i <= iMaxIter; i++, fT0 += fH)
    {
        float fE0 = Mathf::Exp(-3.0f*fT0);
        float fE1 = Mathf::Exp(-39.0f*fT0);
        float fCDiv3 = Mathf::Cos(fT0)/3.0f;
        fX0 = 2.0f*fE0 - fE1 + fCDiv3;
        fY0 = -fE0 + 2.0f*fE1 - fCDiv3;
        if ( i >= 2 && ((i%2) == 0) )
        {
            sprintf(acMsg,"i = %d, %+8.6f, %+8.6f",i,fX0,fY0);
            kOutTxt << acMsg << endl;
        }
    }
}
//----------------------------------------------------------------------------
void Stiff2Approximate ()
{
    // approximation with step size 0.05
    float fH = 0.05f;
    float fX0 = 4.0f/3.0f;
    float fY0 = 2.0f/3.0f;
    float fT0 = 0.0f;

    const int iMaxIter = 20;
    float* afApprox0 = new float[iMaxIter+1];
    float* afApprox1 = new float[iMaxIter+1];
    afApprox0[0] = fX0;
    afApprox1[0] = fY0;
    int i;
    for (i = 1; i <= iMaxIter; i++)
    {
        float fK1X = fH*F0(fT0,fX0,fY0);
        float fK1Y = fH*F1(fT0,fX0,fY0);
        float fX1 = fX0 + 0.5f*fK1X;
        float fY1 = fY0 + 0.5f*fK1Y;
        float fK2X = fH*F0(fT0+0.5f*fH,fX1,fY1);
        float fK2Y = fH*F1(fT0+0.5f*fH,fX1,fY1);
        fX1 = fX0 + 0.5f*fK2X;
        fY1 = fY0 + 0.5f*fK2Y;
        float fK3X = fH*F0(fT0+0.5f*fH,fX1,fY1);
        float fK3Y = fH*F1(fT0+0.5f*fH,fX1,fY1);
        fX1 = fX0 + fK3X;
        fY1 = fY0 + fK3Y;
        float fK4X = fH*F0(fT0+fH,fX1,fY1);
        float fK4Y = fH*F1(fT0+fH,fX1,fY1);
        fX1 = fX0 + (fK1X + 2.0f*fK2X + 2.0f*fK3X + fK4X)/6.0f;
        fY1 = fY0 + (fK1Y + 2.0f*fK2Y + 2.0f*fK3Y + fK4Y)/6.0f;

        afApprox0[i] = fX1;
        afApprox1[i] = fY1;
        fX0 = fX1;
        fY0 = fY1;
        fT0 += fH;
    }

    ofstream kOutTxt("stiff2_appr_h0.05.txt");
    char acMsg[512];
    for (i = 0; i <= iMaxIter; i++)
    {
        if ( (i % 2) == 0 )
        {
            sprintf(acMsg,"i = %d, %+8.6f, %+8.6f",i,afApprox0[i],
                afApprox1[i]);
            kOutTxt << acMsg << endl;
        }
    }
    kOutTxt.close();

    // approximation with step size 0.1
    fH = 0.1f;
    fX0 = 4.0f/3.0f;
    fY0 = 2.0f/3.0f;
    fT0 = 0.0f;

    afApprox0[0] = fX0;
    afApprox1[0] = fY0;
    for (i = 1; i <= iMaxIter/2; i++)
    {
        float fK1X = fH*F0(fT0,fX0,fY0);
        float fK1Y = fH*F1(fT0,fX0,fY0);
        float fX1 = fX0 + 0.5f*fK1X;
        float fY1 = fY0 + 0.5f*fK1Y;
        float fK2X = fH*F0(fT0+0.5f*fH,fX1,fY1);
        float fK2Y = fH*F1(fT0+0.5f*fH,fX1,fY1);
        fX1 = fX0 + 0.5f*fK2X;
        fY1 = fY0 + 0.5f*fK2Y;
        float fK3X = fH*F0(fT0+0.5f*fH,fX1,fY1);
        float fK3Y = fH*F1(fT0+0.5f*fH,fX1,fY1);
        fX1 = fX0 + fK3X;
        fY1 = fY0 + fK3Y;
        float fK4X = fH*F0(fT0+fH,fX1,fY1);
        float fK4Y = fH*F1(fT0+fH,fX1,fY1);
        fX1 = fX0 + (fK1X + 2.0f*fK2X + 2.0f*fK3X + fK4X)/6.0f;
        fY1 = fY0 + (fK1Y + 2.0f*fK2Y + 2.0f*fK3Y + fK4Y)/6.0f;

        afApprox0[i] = fX1;
        afApprox1[i] = fY1;
        fX0 = fX1;
        fY0 = fY1;
        fT0 += fH;
    }

    kOutTxt.open("stiff2_appr_h0.10.txt");
    for (i = 0; i <= iMaxIter/2; i++)
    {
        sprintf(acMsg,"i = %d, %+8.6f, %+8.6f",i,afApprox0[i],afApprox1[i]);
        kOutTxt << acMsg << endl;
    }
    kOutTxt.close();

    delete[] afApprox0;
    delete[] afApprox1;
}
//----------------------------------------------------------------------------
int main ()
{
    SolveMethod(ExplicitEuler,"explicit.im","explicit.txt");
    SolveMethod(ImplicitEuler,"implicit.im","implicit.txt");
    SolveMethod(RungeKutta,"runge.im","runge.txt");
    SolveMethod(LeapFrog,"leapfrog.im","leapfrog.txt");
    Stiff1();
    Stiff2True();
    Stiff2Approximate();
    return 0;
}
//----------------------------------------------------------------------------
