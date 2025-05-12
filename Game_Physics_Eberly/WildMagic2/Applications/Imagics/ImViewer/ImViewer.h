// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef IMVIEWER_H
#define IMVIEWER_H

#include "WmlApplication2.h"

class ImViewer : public Application2
{
public:
    ImViewer ();
    virtual ~ImViewer ();

    virtual bool OnPrecreate ();
    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);
    virtual void OnSpecialKeyDown (int iKey, int iX, int iY);
    virtual void OnMouseClick (int iButton, int iState, int iX, int iY,
        unsigned int uiModifiers);
    virtual void OnMotion (int iX, int iY, unsigned int uiModifiers);
    virtual void ScreenOverlay ();

protected:
    bool LoadImage ();
    void CopySliceToScreen ();
    void ReadPixelValue (int iX, int iY);
    void WritePixelString ();

    int m_iDimensions, m_iQuantity, m_iSliceQuantity;
    int* m_aiBound;
    float* m_afData;
    Color* m_akData;
    float m_fMin, m_fMax, m_fRange, m_fInvRange;
    int m_iZ;
    char m_acPixelStr[256];
    bool m_bMouseDown;
};

#endif
