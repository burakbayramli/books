// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "ImViewer.h"
#include "WmlImages.h"
using namespace Wml;

ImViewer g_kTheApp;

//----------------------------------------------------------------------------
ImViewer::ImViewer ()
    :
    Application2("ImViewer",0,0,256,256,ColorRGB(1.0f,1.0f,1.0f))
{
    m_bMouseDown = false;
    m_iZ = 0;
    m_aiBound = NULL;
    m_akData = NULL;
    m_afData = NULL;
    m_iSliceQuantity = 0;
}
//----------------------------------------------------------------------------
ImViewer::~ImViewer ()
{
}
//----------------------------------------------------------------------------
bool ImViewer::OnPrecreate ()
{
    if ( !Application2::OnPrecreate() )
        return false;

    // get the name of the image file to open and display
    if ( !GetCommand() )
    {
        // command line must be specified
        return false;
    }
    char* acFilename = NULL;
    GetCommand()->Filename(acFilename);
    if ( !acFilename )
    {
        // input filename must be specified on the command line
        return false;
    }

    // load the image data
    int iRTTI, iSizeOf;
    char* acData = NULL;
    bool bLoaded = Lattice::LoadRaw(acFilename,m_iDimensions,m_aiBound,
        m_iQuantity,iRTTI,iSizeOf,acData);
    delete[] acFilename;
    if ( !bLoaded )
    {
        delete[] acData;
        return false;
    }

    // Set window size based on image size.  Adjust height to allow for
    // status bar.  The window width is chosen so that rows are multiples
    // of 4 bytes.
    ms_iHeight = m_aiBound[1];
    if ( ms_iHeight < 128 )
        ms_iHeight = 128;

    int iStatusHeight = 20;
    ms_iHeight += iStatusHeight;

    ms_iWidth = m_aiBound[0];
    if ( ms_iWidth < 256 )
        ms_iWidth = 256;

    int iRem = (ms_iWidth % 4);
    if ( iRem > 0 )
        ms_iWidth += 4-iRem;

    // The application image is stored differently depending on the input
    // image format.
    int i;
    if ( iRTTI == Ergb5::GetRTTI() )
    {
        m_akData = new Color[m_iQuantity];
        unsigned short* ausData = (unsigned short*)acData;
        for (i = 0; i < m_iQuantity; i++)
        {
            unsigned char ucR = GetRed16(ausData[i]);
            unsigned char ucG = GetGreen16(ausData[i]);
            unsigned char ucB = GetBlue16(ausData[i]);
            m_akData[i] = Color(ucR,ucG,ucB);
        }
    }
    else if ( iRTTI == Ergb8::GetRTTI() )
    {
        m_akData = new Color[m_iQuantity];
        unsigned int* auiData = (unsigned int*)acData;
        for (i = 0; i < m_iQuantity; i++)
        {
            unsigned char ucR = GetRed24(auiData[i]);
            unsigned char ucG = GetGreen24(auiData[i]);
            unsigned char ucB = GetBlue24(auiData[i]);
            m_akData[i] = Color(ucR,ucG,ucB);
        }
    }
    else if ( iRTTI == Efloat::GetRTTI() )
    {
        m_afData = new float[m_iQuantity];
        memcpy(m_afData,acData,m_iQuantity*sizeof(float));
    }
    else
    {
        m_afData = new float[m_iQuantity];
        ImageConvert(m_iQuantity,iRTTI,acData,Efloat::GetRTTI(),m_afData);
    }
    delete[] acData;

    // compute min, max and range of float images
    if ( m_afData )
    {
        m_fMin = m_afData[0];
        m_fMax = m_fMin;
        for (i = 1; i < m_iQuantity; i++)
        {
            if ( m_afData[i] < m_fMin )
                m_fMin = m_afData[i];
            else if ( m_afData[i] > m_fMax )
                m_fMax = m_afData[i];
        }
        m_fRange = m_fMax - m_fMin;
        if ( m_fRange > 0.0f )
            m_fInvRange = 1.0f/m_fRange;
        else
            m_fInvRange = 0.0f;
    }

    // we only need memory to hold the active slice
    m_iSliceQuantity = m_aiBound[0]*m_aiBound[1];

    return true;
}
//----------------------------------------------------------------------------
bool ImViewer::OnInitialize ()
{
    if ( !Application2::OnInitialize() )
        return false;

    CopySliceToScreen();
    OnDisplay();
    return true;
}
//----------------------------------------------------------------------------
void ImViewer::OnTerminate ()
{
    delete[] m_akData;
    delete[] m_afData;
    delete[] m_aiBound;
    Application2::OnTerminate();
}
//----------------------------------------------------------------------------
void ImViewer::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }
}
//----------------------------------------------------------------------------
void ImViewer::OnSpecialKeyDown (int iKey, int iX, int iY)
{
    if ( m_iDimensions != 3 )
        return;

    if ( iKey == KEY_UP_ARROW )
    {
        // up-arrow pressed, go to next image slize
        if ( m_iZ < m_aiBound[2] - 1 )
        {
            m_iZ++;
            CopySliceToScreen();
            if ( m_bMouseDown )
                ReadPixelValue(iX,iY);
            OnDisplay();
        }
    }
    else if ( iKey == KEY_DOWN_ARROW )
    {
        // down-arrow pressed, go to previous image slice
        if ( m_iZ > 0 )
        {
            m_iZ--;
            CopySliceToScreen();
            if ( m_bMouseDown )
                ReadPixelValue(iX,iY);
            OnDisplay();
        }
    }
}
//----------------------------------------------------------------------------
void ImViewer::OnMouseClick (int iButton, int iState, int iX, int iY,
    unsigned int)
{
    if ( iButton == MOUSE_LEFT_BUTTON )
    {
        if ( iState == MOUSE_DOWN )
        {
            m_bMouseDown = true;
            ReadPixelValue(iX,iY);
            OnDisplay();
        }
        else if ( iState == MOUSE_UP )
        {
            m_bMouseDown = false;
            ReadPixelValue(-1,-1);
            OnDisplay();
        }
    }
}
//----------------------------------------------------------------------------
void ImViewer::OnMotion (int iX, int iY, unsigned int uiModifiers)
{
    if ( m_bMouseDown )
    {
        ReadPixelValue(iX,iY);
        OnDisplay();
    }
}
//----------------------------------------------------------------------------
void ImViewer::CopySliceToScreen ()
{
    ClearScreen();

    int iX, iY;

    if ( m_afData )
    {
        if ( m_fRange > 0.0f )
        {
            float* afSlice = &m_afData[m_iZ*m_iSliceQuantity];
            for (iY = 0; iY < m_aiBound[1]; iY++)
            {
                for (iX = 0; iX < m_aiBound[0]; iX++)
                {
                    int i = iX+m_aiBound[0]*iY;
                    float fGray = 255.0f*(afSlice[i]-m_fMin)*m_fInvRange;
                    unsigned char ucGray = (unsigned char)(fGray);
                    m_akScreen[Index(iX,iY)] = Color(ucGray,ucGray,ucGray);
                }
            }
        }
    }
    else
    {
        Color* akSlice = &m_akData[m_iZ*m_iSliceQuantity];
        for (iY = 0; iY < m_aiBound[1]; iY++)
        {
            for (iX = 0; iX < m_aiBound[0]; iX++)
            {
                int i = iX+m_aiBound[0]*iY;
                m_akScreen[Index(iX,iY)] = akSlice[i];
            }
        }
    }
}
//----------------------------------------------------------------------------
void ImViewer::ReadPixelValue (int iX, int iY)
{
    if ( 0 <= iX && iX < m_aiBound[0] && 0 <= iY && iY < m_aiBound[1] )
    {
        int iIndex = iX + m_aiBound[0]*iY;
        if ( m_afData )
        {
            float* afSlice = &m_afData[m_iZ*m_iSliceQuantity];
            float fValue = afSlice[iIndex];
            if ( m_iDimensions == 2 )
                sprintf(m_acPixelStr,"(%d,%d) %f",iX,iY,fValue);
            else
                sprintf(m_acPixelStr,"(%d,%d,%d) %f",iX,iY,m_iZ,fValue);
        }
        else
        {
            Color* akSlice = &m_akData[m_iZ*m_iSliceQuantity];
            Color kValue = akSlice[iIndex];
            if ( m_iDimensions == 2 )
            {
                sprintf(m_acPixelStr,"(%d,%d) r=%d g=%d b=%d",iX,iY,
                    (int)kValue.r,(int)kValue.g,(int)kValue.b);
            }
            else
            {
                sprintf(m_acPixelStr,"(%d,%d,%d) r=%d g=%d b=%d",iX,iY,m_iZ,
                    (int)kValue.r,(int)kValue.g,(int)kValue.b);
            }
        }
    }
    else
    {
        int iLength = (int)strlen(m_acPixelStr);
        memset(m_acPixelStr,' ',iLength);
    }
}
//----------------------------------------------------------------------------
void ImViewer::ScreenOverlay ()
{
    if ( m_bMouseDown )
    {
        ms_spkRenderer->Draw(4,ms_iHeight-4,ColorRGB(0.0f,0.0f,0.0f),
            m_acPixelStr);
    }
}
//----------------------------------------------------------------------------
