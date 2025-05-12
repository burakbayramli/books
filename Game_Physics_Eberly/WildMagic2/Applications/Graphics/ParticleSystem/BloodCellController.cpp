// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlMath.h"
#include "WmlParticles.h"
#include "BloodCellController.h"
using namespace Wml;

//----------------------------------------------------------------------------
BloodCellController::BloodCellController ()
{
}
//----------------------------------------------------------------------------
void BloodCellController::UpdatePointMotion (float)
{
    Particles* pkParticle = (Particles*) m_pkObject;

    unsigned int uiVMax = pkParticle->GetVertexQuantity();
    Vector3f* akVertex = pkParticle->Vertices();
    float* afSize = pkParticle->Sizes();
    for (unsigned int uiV = 0; uiV < uiVMax; uiV++)
    {
        akVertex[uiV].X() += 0.01f*Mathf::SymmetricRandom();
        if ( akVertex[uiV].X() > 1.0f )
            akVertex[uiV].X() = 1.0f;
        else if ( akVertex[uiV].X() < -1.0f )
            akVertex[uiV].X() = -1.0f;

        akVertex[uiV].Y() += 0.01f*Mathf::SymmetricRandom();
        if ( akVertex[uiV].Y() > 1.0f )
            akVertex[uiV].Y() = 1.0f;
        else if ( akVertex[uiV].Y() < -1.0f )
            akVertex[uiV].Y() = -1.0f;

        akVertex[uiV].Z() += 0.01f*Mathf::SymmetricRandom();
        if ( akVertex[uiV].Z() > 1.0f )
            akVertex[uiV].Z() = 1.0f;
        else if ( akVertex[uiV].Z() < -1.0f )
            akVertex[uiV].Z() = -1.0f;

        afSize[uiV] *= (1.0f + 0.01f*Mathf::SymmetricRandom());
        if ( afSize[uiV] > 0.25f )
            afSize[uiV] = 0.25f;
    }
    pkParticle->UpdateModelBound();
}
//----------------------------------------------------------------------------
