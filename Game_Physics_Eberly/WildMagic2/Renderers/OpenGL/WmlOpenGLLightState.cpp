// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlAmbientLight.h"
#include "WmlDirectionalLight.h"
#include "WmlPointLight.h"
#include "WmlSpotLight.h"
#include "WmlOpenGLRenderer.h"
using namespace Wml;

//----------------------------------------------------------------------------
void OpenGLRenderer::SetLightState (LightState* pkState)
{
    int iQuantity = pkState->GetQuantity();

    if ( m_eOverrideLightingMode != OLM_DISABLE && iQuantity > 0 )
    {
        // If all lights are off, nothing to do.
        int i;
        Light* pkLight;
        for (i = 0; i < iQuantity; i++)
        {
            pkLight = pkState->Get(i);
            if ( pkLight->On() )
                break;
        }
        if ( i == iQuantity )
        {
            glDisable(GL_LIGHTING);
            return;
        }

        glEnable(GL_LIGHTING);

        // This is enabled if any of the active lights want it.
        bool bSpecularAfterTexture = false;

        ColorRGB akTmpColor;
        GLfloat afColor[4];
        afColor[3] = 1.0f;

        for (i = 0; i < iQuantity; i++)
        {
            pkLight = pkState->Get(i);
            GLint iIndex = GL_LIGHT0 + i;

            if ( !pkLight->On() )
            {
                glDisable((GLenum)iIndex);
                continue;
            }

            glEnable((GLenum)iIndex);

            if ( pkLight->SpecularAfterTexture() )
                bSpecularAfterTexture = true;

            // ambient
            if ( m_eOverrideLightingMode == OLM_ONLY_SPECULAR )
            {
                afColor[0] = 0.0f;
                afColor[1] = 0.0f;
                afColor[2] = 0.0f;
            }
            else
            {
                akTmpColor = pkLight->Intensity()*pkLight->Ambient();
                akTmpColor.Clamp();
                afColor[0] = akTmpColor.r;
                afColor[1] = akTmpColor.g;
                afColor[2] = akTmpColor.b;
            }
            glLightfv((GLenum)iIndex,GL_AMBIENT,afColor);

            // diffuse
            if ( pkLight->GetType() == Light::LT_AMBIENT
            ||   m_eOverrideLightingMode == OLM_ONLY_SPECULAR )
            {
                afColor[0] = 0.0f;
                afColor[1] = 0.0f;
                afColor[2] = 0.0f;
            }
            else
            {
                akTmpColor = pkLight->Intensity()*pkLight->Diffuse();
                akTmpColor.Clamp();
                afColor[0] = akTmpColor.r;
                afColor[1] = akTmpColor.g;
                afColor[2] = akTmpColor.b;
            }
            glLightfv((GLenum)iIndex,GL_DIFFUSE,afColor);

            // specular
            if ( pkLight->GetType() == Light::LT_AMBIENT
            ||   m_eOverrideLightingMode == OLM_ONLY_NON_SPECULAR )
            {
                afColor[0] = 0.0f;
                afColor[1] = 0.0f;
                afColor[2] = 0.0f;
            }
            else
            {
                akTmpColor = pkLight->Intensity()*pkLight->Specular();
                akTmpColor.Clamp();
                afColor[0] = akTmpColor.r;
                afColor[1] = akTmpColor.g;
                afColor[2] = akTmpColor.b;
            }
            glLightfv((GLenum)iIndex,GL_SPECULAR,afColor);

            if ( pkLight->Attenuate() )
            {
                glLightf((GLenum)iIndex,GL_CONSTANT_ATTENUATION,
                    pkLight->Constant());
                glLightf((GLenum)iIndex,GL_LINEAR_ATTENUATION,
                    pkLight->Linear());
                glLightf((GLenum)iIndex,GL_QUADRATIC_ATTENUATION,
                    pkLight->Quadratic());
            }
            else
            {
                glLightf((GLenum)iIndex,GL_CONSTANT_ATTENUATION,1.0f);
                glLightf((GLenum)iIndex,GL_LINEAR_ATTENUATION,0.0f);
                glLightf((GLenum)iIndex,GL_QUADRATIC_ATTENUATION,0.0f);
            }

            GLfloat afPosParam[4], afDirParam[3];
            switch ( pkLight->GetType() )
            {
            case Light::LT_DIRECTIONAL:
            {
                DirectionalLight* pkDL = (DirectionalLight*) pkLight;
                afPosParam[0] = -pkDL->Direction().X();
                afPosParam[1] = -pkDL->Direction().Y();
                afPosParam[2] = -pkDL->Direction().Z();
                afPosParam[3] = 0.0f;
                glLightfv((GLenum)iIndex,GL_POSITION,afPosParam);
                break;
            }
            case Light::LT_POINT:
            case Light::LT_SPOT:
            {
                PointLight* pkPL = (PointLight*) pkLight;
                afPosParam[0] = pkPL->Location().X();
                afPosParam[1] = pkPL->Location().Y();
                afPosParam[2] = pkPL->Location().Z();
                afPosParam[3] = 1.0f;
                glLightfv((GLenum)iIndex,GL_POSITION,afPosParam);
                break;
            }
            default:  // Light::LT_AMBIENT, Light::LT_QUANTITY
                break;
            }

            if ( pkLight->GetType() == Light::LT_SPOT )
            {
                SpotLight* pkSpot = (SpotLight*) pkLight;
                afDirParam[0] = pkSpot->Direction().X();
                afDirParam[1] = pkSpot->Direction().Y();
                afDirParam[2] = pkSpot->Direction().Z();
                glLightf((GLenum)iIndex,GL_SPOT_CUTOFF,
                    180.0f*pkSpot->GetAngle()/Mathf::PI);
                glLightfv((GLenum)iIndex,GL_SPOT_DIRECTION,afDirParam);
                glLightf((GLenum)iIndex,GL_SPOT_EXPONENT,
                    pkSpot->Exponent());
            }
            else
            {
                GLfloat afDefaultDirection[3] = { 0.0f, 0.0f, -1.0f };
                glLightf((GLenum)iIndex,GL_SPOT_CUTOFF,180.0f);
                glLightfv((GLenum)iIndex,GL_SPOT_DIRECTION,
                    afDefaultDirection);
                glLightf((GLenum)iIndex,GL_SPOT_EXPONENT,0.0f);
            }
        }

        // TO DO.  This will change in WM2.  The WM1.x code was adding the
        // ambient contributions twice.  This code eliminates one of those
        // contributions.
        GLfloat afAmbient[4] = { 0.0f, 0.0f, 0.0f, 1.0f };
        glLightModelfv(GL_LIGHT_MODEL_AMBIENT,afAmbient);

        if ( m_bCapSpecularAfterTexture )
        {
            if ( bSpecularAfterTexture )
            {
                glLightModeli(GL_LIGHT_MODEL_COLOR_CONTROL, 
                    GL_SEPARATE_SPECULAR_COLOR);
            }
            else
            {
                glLightModeli(GL_LIGHT_MODEL_COLOR_CONTROL, 
                    GL_SINGLE_COLOR);
            }
        }

        for (i = iQuantity; i < LightState::MAX_LIGHTS; i++)
            glDisable((GLenum)(GL_LIGHT0 + i));
    }
    else
    {
        glDisable(GL_LIGHTING);
    }
}
//----------------------------------------------------------------------------
