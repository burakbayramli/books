// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlStandardMesh.h"
using namespace Wml;

//----------------------------------------------------------------------------
void Wml::CreateRectangleMesh (TriMesh*& rpkMesh, const Vector3f& rkCenter,
    const Vector3f& rkU, const Vector3f& rkV, const Vector3f& rkAxis,
    float fUExtent, float fVExtent, bool bWantNormals,  bool bWantColors,
    bool bWantUVs)
{
    // allocate vertices
    int iVQuantity = 4;
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

    // allocate texture coordinates if requested
    Vector2f* akUV = NULL;
    if ( bWantUVs )
        akUV = new Vector2f[iVQuantity];

    // allocate connectivity
    int iTQuantity = 2;
    int* aiConnect = new int[3*iTQuantity];

    // generate geometry
    Vector3f kUTerm = fUExtent*rkU;
    Vector3f kVTerm = fVExtent*rkV;
    akVertex[0] = rkCenter - kUTerm - kVTerm;
    akVertex[1] = rkCenter + kUTerm - kVTerm;
    akVertex[2] = rkCenter + kUTerm + kVTerm;
    akVertex[3] = rkCenter - kUTerm + kVTerm;

    if ( bWantNormals )
    {
        for (int i = 0; i < iVQuantity; i++)
            akNormal[i] = rkAxis;
    }

    if ( bWantUVs )
    {
        akUV[0] = Vector2f(0.0f,0.0f);
        akUV[1] = Vector2f(1.0f,0.0f);
        akUV[2] = Vector2f(1.0f,1.0f);
        akUV[3] = Vector2f(0.0f,1.0f);
    }

    // generate connectivity
    aiConnect[0] = 0;  aiConnect[1] = 1;  aiConnect[2] = 2;
    aiConnect[3] = 0;  aiConnect[4] = 2;  aiConnect[5] = 3;

    if ( rpkMesh )
    {
        rpkMesh->Reconstruct(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
    else
    {
        rpkMesh = new TriMesh(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
}
//----------------------------------------------------------------------------
void Wml::CreateDiskMesh (TriMesh*& rpkMesh, int iShellSamples,
    int iRadialSamples, const Vector3f& rkCenter, float fRadius,
    const Vector3f& rkU, const Vector3f& rkV, const Vector3f& rkAxis,
    bool bWantNormals, bool bWantColors, bool bWantUVs)
{
    int iRSm1 = iRadialSamples - 1, iSSm1 = iShellSamples - 1;

    // allocate vertices
    int iVQuantity = 1 + iRadialSamples*iSSm1;
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

    // allocate texture coordinates if requested
    Vector2f* akUV = NULL;
    if ( bWantUVs )
        akUV = new Vector2f[iVQuantity];

    // allocate connectivity
    int iTQuantity = iRadialSamples*(2*iSSm1-1);
    int* aiConnect = new int[3*iTQuantity];

    // generate geometry
    int iR, iS, i;

    // center of disk
    akVertex[0] = Vector3f::ZERO;
    if ( bWantNormals )
    {
        akNormal[0] = rkAxis;
    }
    if ( bWantUVs )
    {
        akUV[0].X() = 0.5f;
        akUV[0].Y() = 0.5f;
    }

    float fInvSSm1 = 1.0f/(float)iSSm1;
    float fInvRS = 1.0f/(float)iRadialSamples;
    for (iR = 0; iR < iRadialSamples; iR++)
    {
        float fAngle = Mathf::TWO_PI*fInvRS*iR;
        float fCos = Mathf::Cos(fAngle);
        float fSin = Mathf::Sin(fAngle);
        Vector3f kRadial = fCos*rkU + fSin*rkV;

        for (iS = 1; iS < iShellSamples; iS++)
        {
            float fFraction = fInvSSm1*iS;  // in (0,R]
            Vector3f kFracRadial = fFraction*kRadial;
            i = iS+iSSm1*iR;
            akVertex[i] = fRadius*kFracRadial;
            if ( bWantNormals )
            {
                akNormal[i] = rkAxis;
            }
            if ( bWantUVs )
            {
                akUV[i].X() = 0.5f*(1.0f+kFracRadial.X());
                akUV[i].Y() = 0.5f*(1.0f+kFracRadial.Y());
            }
        }
    }

    // rotate and translate disk to specified center
    for (i = 0; i < iVQuantity; i++)
        akVertex[i] += rkCenter;

    // generate connectivity
    int* aiLocalConnect = aiConnect;
    int iT = 0;
    for (int iR0 = iRSm1, iR1 = 0; iR1 < iRadialSamples; iR0 = iR1++)
    {
        aiLocalConnect[0] = 0;
        aiLocalConnect[1] = 1+iSSm1*iR0;
        aiLocalConnect[2] = 1+iSSm1*iR1;
        aiLocalConnect += 3;
        iT++;
        for (int iS = 1; iS < iSSm1; iS++, aiLocalConnect += 6)
        {
            int i00 = iS+iSSm1*iR0;
            int i01 = iS+iSSm1*iR1;
            int i10 = i00+1;
            int i11 = i01+1;
            aiLocalConnect[0] = i00;
            aiLocalConnect[1] = i10;
            aiLocalConnect[2] = i11;
            aiLocalConnect[3] = i00;
            aiLocalConnect[4] = i11;
            aiLocalConnect[5] = i01;
            iT += 2;
        }
    }
    assert( iT == iTQuantity );

    if ( rpkMesh )
    {
        rpkMesh->Reconstruct(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
    else
    {
        rpkMesh = new TriMesh(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
}
//----------------------------------------------------------------------------
void Wml::CreateBoxMesh (TriMesh*& rpkMesh, const Vector3f& rkCenter,
    const Vector3f& rkU, const Vector3f& rkV, const Vector3f& rkW,
    float fUExtent, float fVExtent, float fWExtent, bool bWantNormals,
    bool bWantColors, bool bWantUVs, bool bOutsideView)
{
    // allocate vertices
    int iVQuantity = 8;
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

    // allocate texture coordinates if requested
    Vector2f* akUV = NULL;
    if ( bWantUVs )
        akUV = new Vector2f[iVQuantity];

    // allocate connectivity
    int iTQuantity = 12;
    int* aiConnect = new int[3*iTQuantity];

    // generate geometry
    Vector3f kUTerm = fUExtent*rkU;
    Vector3f kVTerm = fVExtent*rkV;
    Vector3f kWTerm = fWExtent*rkW;
    akVertex[0] = rkCenter - kUTerm - kVTerm - kWTerm;
    akVertex[1] = rkCenter + kUTerm - kVTerm - kWTerm;
    akVertex[2] = rkCenter + kUTerm + kVTerm - kWTerm;
    akVertex[3] = rkCenter - kUTerm + kVTerm - kWTerm;
    akVertex[4] = rkCenter - kUTerm - kVTerm + kWTerm;
    akVertex[5] = rkCenter + kUTerm - kVTerm + kWTerm;
    akVertex[6] = rkCenter + kUTerm + kVTerm + kWTerm;
    akVertex[7] = rkCenter - kUTerm + kVTerm + kWTerm;

    if ( bWantUVs )
    {
        akUV[0] = Vector2f(0.25f,0.75f);
        akUV[1] = Vector2f(0.75f,0.75f);
        akUV[2] = Vector2f(0.75f,0.25f);
        akUV[3] = Vector2f(0.25f,0.25f);
        akUV[4] = Vector2f(0.0f,1.0f);
        akUV[5] = Vector2f(1.0f,1.0f);
        akUV[6] = Vector2f(1.0f,0.0f);
        akUV[7] = Vector2f(0.0f,0.0f);
    }

    // generate connectivity (outside view)
    aiConnect[ 0] = 0;  aiConnect[ 1] = 2;  aiConnect[ 2] = 1;
    aiConnect[ 3] = 0;  aiConnect[ 4] = 3;  aiConnect[ 5] = 2;
    aiConnect[ 6] = 0;  aiConnect[ 7] = 1;  aiConnect[ 8] = 5;
    aiConnect[ 9] = 0;  aiConnect[10] = 5;  aiConnect[11] = 4;
    aiConnect[12] = 0;  aiConnect[13] = 4;  aiConnect[14] = 7;
    aiConnect[15] = 0;  aiConnect[16] = 7;  aiConnect[17] = 3;
    aiConnect[18] = 6;  aiConnect[19] = 4;  aiConnect[20] = 5;
    aiConnect[21] = 6;  aiConnect[22] = 7;  aiConnect[23] = 4;
    aiConnect[24] = 6;  aiConnect[25] = 5;  aiConnect[26] = 1;
    aiConnect[27] = 6;  aiConnect[28] = 1;  aiConnect[29] = 2;
    aiConnect[30] = 6;  aiConnect[31] = 2;  aiConnect[32] = 3;
    aiConnect[33] = 6;  aiConnect[34] = 3;  aiConnect[35] = 7;

    if ( !bOutsideView )
    {
        int* aiLocalConnect = aiConnect;
        for (int iT = 0; iT < iTQuantity; iT++, aiLocalConnect +=3)
        {
            int iSave = aiLocalConnect[1];
            aiLocalConnect[1] = aiLocalConnect[2];
            aiLocalConnect[2] = iSave;
        }
    }

    if ( rpkMesh )
    {
        rpkMesh->Reconstruct(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
    else
    {
        rpkMesh = new TriMesh(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }

    if ( bWantNormals )
        rpkMesh->UpdateModelNormals();
}
//----------------------------------------------------------------------------
void Wml::CreateCylinderMesh (TriMesh*& rpkMesh, int iAxisSamples,
    int iRadialSamples, const Vector3f& rkCenter, const Vector3f& rkU,
    const Vector3f& rkV, const Vector3f& rkAxis, float fRadius,
    float fHeight, bool bWantNormals, bool bWantColors, bool bWantUVs,
    bool bOutsideView)
{
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

    // allocate texture coordinates if requested
    Vector2f* akUV = NULL;
    if ( bWantUVs )
        akUV = new Vector2f[iVQuantity];

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
            float fRadialFraction = iR*fInvRS;  // in [0,1)
            Vector3f kNormal = afCos[iR]*rkU + afSin[iR]*rkV;
            akVertex[i] = kSliceCenter + fRadius*kNormal;
            if ( bWantNormals )
            {
                if ( bOutsideView )
                    akNormal[i] = kNormal;
                else
                    akNormal[i] = -kNormal;
            }
            if ( bWantUVs )
            {
                akUV[i].X() = fRadialFraction;
                akUV[i].Y() = fAxisFraction;
            }
            i++;
        }

        akVertex[i] = akVertex[iSave];
        if ( bWantNormals )
        {
            akNormal[i] = akNormal[iSave];
        }
        if ( bWantUVs )
        {
            akUV[i].X() = 1.0f;
            akUV[i].Y() = fAxisFraction;
        }
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

    if ( rpkMesh )
    {
        rpkMesh->Reconstruct(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
    else
    {
        rpkMesh = new TriMesh(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
}
//----------------------------------------------------------------------------
void Wml::CreateSphereMesh (TriMesh*& rpkMesh, int iZSamples,
    int iRadialSamples, const Vector3f& rkCenter, float fRadius,
    const Vector3f& rkU, const Vector3f& rkV, const Vector3f& rkAxis,
    bool bWantNormals, bool bWantColors, bool bWantUVs, bool bOutsideView)
{
    int iZSm1 = iZSamples-1, iZSm2 = iZSamples-2, iZSm3 = iZSamples-3;
    int iRSp1 = iRadialSamples+1;

    // allocate vertices
    int iVQuantity = iZSm2*iRSp1 + 2;
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

    // allocate texture coordinates if requested
    Vector2f* akUV = NULL;
    if ( bWantUVs )
        akUV = new Vector2f[iVQuantity];

    // allocate connectivity
    int iTQuantity = 2*iZSm2*iRadialSamples;
    int* aiConnect = new int[3*iTQuantity];

    // generate geometry
    float fInvRS = 1.0f/(float)iRadialSamples;
    float fZFactor = 2.0f/(float)iZSm1;
    int iR, iZ, iZStart, i;

    // Generate points on the unit circle to be used in computing the mesh
    // points on a cylinder slice.
    float* afSin = new float[iRSp1];
    float* afCos = new float[iRSp1];
    for (iR = 0; iR < iRadialSamples; iR++)
    {
        float fAngle = Mathf::TWO_PI*fInvRS*iR;
        afCos[iR] = Mathf::Cos(fAngle);
        afSin[iR] = Mathf::Sin(fAngle);
    }
    afSin[iRadialSamples] = afSin[0];
    afCos[iRadialSamples] = afCos[0];

    // generate the cylinder itself
    for (iZ = 1, i = 0; iZ < iZSm1; iZ++)
    {
        float fZFraction = -1.0f + fZFactor*iZ;  // in (-1,1)
        float fZ = fRadius*fZFraction;

        // compute center of slice
        Vector3f kSliceCenter = rkCenter + fZ*rkAxis;

        // compute radius of slice
        float fSliceRadius = Mathf::Sqrt(Mathf::FAbs(fRadius*fRadius-fZ*fZ));

        // compute slice vertices with duplication at end point
        Vector3f kNormal;
        int iSave = i;
        for (iR = 0; iR < iRadialSamples; iR++)
        {
            float fRadialFraction = iR*fInvRS;  // in [0,1)
            Vector3f kRadial = afCos[iR]*rkU + afSin[iR]*rkV;
            akVertex[i] = kSliceCenter + fSliceRadius*kRadial;
            if ( bWantNormals )
            {
                kNormal = akVertex[i] - rkCenter;
                kNormal.Normalize();
                if ( bOutsideView )
                    akNormal[i] = kNormal;
                else
                    akNormal[i] = -kNormal;
            }
            if ( bWantUVs )
            {
                akUV[i].X() = fRadialFraction;
                akUV[i].Y() = 0.5f*(fZFraction+1.0f);
            }
            i++;
        }

        akVertex[i] = akVertex[iSave];
        if ( bWantNormals )
        {
            akNormal[i] = akNormal[iSave];
        }
        if ( bWantUVs )
        {
            akUV[i].X() = 1.0f;
            akUV[i].Y() = 0.5f*(fZFraction+1.0f);
        }
        i++;
    }

    // south pole
    akVertex[i] = rkCenter - fRadius*rkAxis;
    if ( bWantNormals )
    {
        if ( bOutsideView )
            akNormal[i] = -rkAxis;
        else
            akNormal[i] = rkAxis;
    }
    if ( bWantUVs )
    {
        akUV[i].X() = 0.5f;
        akUV[i].Y() = 0.0f;
    }
    i++;

    // north pole
    akVertex[i] = rkCenter + fRadius*rkAxis;
    if ( bWantNormals )
    {
        if ( bOutsideView )
            akNormal[i] = rkAxis;
        else
            akNormal[i] = -rkAxis;
    }
    if ( bWantUVs )
    {
        akUV[i].X() = 0.5f;
        akUV[i].Y() = 1.0f;
    }
    i++;
    assert( i == iVQuantity );

    // generate connectivity
    int* aiLocalConnect = aiConnect;
    for (iZ = 0, iZStart = 0; iZ < iZSm3; iZ++)
    {
        int i0 = iZStart;
        int i1 = i0 + 1;
        iZStart += iRSp1;
        int i2 = iZStart;
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
            else  // inside view
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

    // south pole triangles
    int iVQm2 = iVQuantity-2;
    for (i = 0; i < iRadialSamples; i++, aiLocalConnect += 3)
    {
        if ( bOutsideView )
        {
            aiLocalConnect[0] = i;
            aiLocalConnect[1] = iVQm2;
            aiLocalConnect[2] = i+1;
        }
        else  // inside view
        {
            aiLocalConnect[0] = i;
            aiLocalConnect[1] = i+1;
            aiLocalConnect[2] = iVQm2;
        }
    }

    // north pole triangles
    int iVQm1 = iVQuantity-1, iOffset = iZSm3*iRSp1;
    for (i = 0; i < iRadialSamples; i++, aiLocalConnect += 3)
    {
        if ( bOutsideView )
        {
            aiLocalConnect[0] = i+iOffset;
            aiLocalConnect[1] = i+1+iOffset;
            aiLocalConnect[2] = iVQm1;
        }
        else  // inside view
        {
            aiLocalConnect[0] = i+iOffset;
            aiLocalConnect[1] = iVQm1;
            aiLocalConnect[2] = i+1+iOffset;
        }
    }

    int* aiFinalConnect = aiConnect + 3*iTQuantity;
    assert( aiLocalConnect == aiFinalConnect );

    delete[] afCos;
    delete[] afSin;

    if ( rpkMesh )
    {
        rpkMesh->Reconstruct(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
    else
    {
        rpkMesh = new TriMesh(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
}
//----------------------------------------------------------------------------
void Wml::CreateTorusMesh (TriMesh*& rpkMesh, int iCircleSamples,
    int iRadialSamples, const Vector3f& rkCenter, const Vector3f& rkU,
    const Vector3f& rkV, const Vector3f& rkAxis, float fOuterRadius,
    float fInnerRadius, bool bWantNormals, bool bWantColors, bool bWantUVs,
    bool bOutsideView)
{
    // allocate vertices
    int iVQuantity = (iCircleSamples+1)*(iRadialSamples+1);
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

    // allocate texture coordinates if requested
    Vector2f* akUV = NULL;
    if ( bWantUVs )
        akUV = new Vector2f[iVQuantity];

    // allocate connectivity
    int iTQuantity = 2*iCircleSamples*iRadialSamples;
    int* aiConnect = new int[3*iTQuantity];

    // generate geometry
    float fInvCS = 1.0f/(float)iCircleSamples;
    float fInvRS = 1.0f/(float)iRadialSamples;
    int iC, iR, i;

    // generate the cylinder itself
    for (iC = 0, i = 0; iC < iCircleSamples; iC++)
    {
        // compute center point on torus circle at specified angle
        float fCircleFraction = iC*fInvCS;  // in [0,1)
        float fTheta = Mathf::TWO_PI*fCircleFraction;
        float fCosTheta = Mathf::Cos(fTheta);
        float fSinTheta = Mathf::Sin(fTheta);
        Vector3f kRadial = fCosTheta*rkU + fSinTheta*rkV;
        Vector3f kTorusMiddle = rkCenter + fOuterRadius*kRadial;

        // compute slice vertices with duplication at end point
        int iSave = i;
        for (iR = 0; iR < iRadialSamples; iR++)
        {
            float fRadialFraction = iR*fInvRS;  // in [0,1)
            float fPhi = Mathf::TWO_PI*fRadialFraction;
            float fCosPhi = Mathf::Cos(fPhi);
            float fSinPhi = Mathf::Sin(fPhi);
            Vector3f kNormal = fCosPhi*kRadial + fSinPhi*rkAxis;
            akVertex[i] = kTorusMiddle + fInnerRadius*kNormal;
            if ( bWantNormals )
            {
                if ( bOutsideView )
                    akNormal[i] = kNormal;
                else
                    akNormal[i] = -kNormal;
            }
            if ( bWantUVs )
            {
                akUV[i].X() = fRadialFraction;
                akUV[i].Y() = fCircleFraction;
            }
            i++;
        }

        akVertex[i] = akVertex[iSave];
        if ( bWantNormals )
        {
            akNormal[i] = akNormal[iSave];
        }
        if ( bWantUVs )
        {
            akUV[i].X() = 1.0f;
            akUV[i].Y() = fCircleFraction;
        }
        i++;
    }

    // duplicate the cylinder ends to form a torus
    for (iR = 0; iR <= iRadialSamples; iR++, i++)
    {
        akVertex[i] = akVertex[iR];
        if ( bWantNormals )
        {
            akNormal[i] = akNormal[iR];
        }
        if ( bWantUVs )
        {
            akUV[i].X() = akUV[iR].X();
            akUV[i].Y() = 1.0f;
        }
    }
    assert( i == iVQuantity );

    // generate connectivity
    int* aiLocalConnect = aiConnect;
    int iCStart = 0;
    for (iC = 0; iC < iCircleSamples; iC++)
    {
        int i0 = iCStart;
        int i1 = i0 + 1;
        iCStart += iRadialSamples + 1;
        int i2 = iCStart;
        int i3 = i2 + 1;
        for (i = 0; i < iRadialSamples; i++, aiLocalConnect += 6)
        {
            if ( bOutsideView )
            {
                aiLocalConnect[0] = i0++;
                aiLocalConnect[1] = i2;
                aiLocalConnect[2] = i1;
                aiLocalConnect[3] = i1++;
                aiLocalConnect[4] = i2++;
                aiLocalConnect[5] = i3++;
            }
            else  // inside view
            {
                aiLocalConnect[0] = i0++;
                aiLocalConnect[1] = i1;
                aiLocalConnect[2] = i2;
                aiLocalConnect[3] = i1++;
                aiLocalConnect[4] = i3++;
                aiLocalConnect[5] = i2++;
            }
        }
    }

    if ( rpkMesh )
    {
        rpkMesh->Reconstruct(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
    else
    {
        rpkMesh = new TriMesh(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
}
//----------------------------------------------------------------------------
void Wml::CreateTetrahedronMesh (TriMesh*& rpkMesh, bool bWantNormals,
    bool bWantColors, bool bWantUVs, bool bOutsideView)
{
    float fSqrt2Div3 = Mathf::Sqrt(2.0f)/3.0f;
    float fSqrt6Div3 = Mathf::Sqrt(6.0f)/3.0f;
    float fOneThird = 1.0f/3.0f;
    int i;

    // allocate vertices
    int iVQuantity = 4;
    Vector3f* akVertex = new Vector3f[iVQuantity];
    akVertex[0] = Vector3f(0.0f,0.0f,1.0f);
    akVertex[1] = Vector3f(2.0f*fSqrt2Div3,0.0f,-fOneThird);
    akVertex[2] = Vector3f(-fSqrt2Div3,fSqrt6Div3,-fOneThird);
    akVertex[3] = Vector3f(-fSqrt2Div3,-fSqrt6Div3,-fOneThird);

    // allocate normals if requested
    Vector3f* akNormal = NULL;
    if ( bWantNormals )
    {
        akNormal = new Vector3f[iVQuantity];
        for (i = 0; i < iVQuantity; i++)
            akNormal[i] = akVertex[i];
    }

    // allocate colors if requested
    ColorRGB* akColor = NULL;
    if ( bWantColors )
    {
        akColor = new ColorRGB[iVQuantity];
        memset(akColor,0,iVQuantity*sizeof(ColorRGB));
    }

    // allocate texture coordinates if requested
    Vector2f* akUV = NULL;
    if ( bWantUVs )
    {
        akUV = new Vector2f[iVQuantity];
        for (i = 0; i < iVQuantity; i++)
        {
            if ( Mathf::FAbs(akVertex[i].Z()) < 1.0f )
            {
                akUV[i].X() = 0.5f*(1.0f + Mathf::ATan2(akVertex[i].Y(),
                    akVertex[i].X())*Mathf::INV_PI);
            }
            else
            {
                akUV[i].X() = 0.5f;
            }
            akUV[i].Y() = Mathf::ACos(akVertex[i].Z())*Mathf::INV_PI;
        }
    }

    // allocate connectivity
    int iTQuantity = 4;
    int* aiConnect = new int[3*iTQuantity];
    aiConnect[ 0] = 0;  aiConnect[ 1] = 1;  aiConnect[ 2] = 2;
    aiConnect[ 3] = 0;  aiConnect[ 4] = 2;  aiConnect[ 5] = 3;
    aiConnect[ 6] = 0;  aiConnect[ 7] = 3;  aiConnect[ 8] = 1;
    aiConnect[ 9] = 1;  aiConnect[10] = 3;  aiConnect[11] = 2;

    if ( !bOutsideView )
    {
        for (i = 0; i < iTQuantity; i++)
        {
            int iSave = aiConnect[3*i+1];
            aiConnect[3*i+1] = aiConnect[3*i+2];
            aiConnect[3*i+2] = iSave;
        }
    }

    if ( rpkMesh )
    {
        rpkMesh->Reconstruct(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
    else
    {
        rpkMesh = new TriMesh(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
}
//----------------------------------------------------------------------------
void Wml::CreateHexahedronMesh (TriMesh*& rpkMesh, bool bWantNormals,
    bool bWantColors, bool bWantUVs, bool bOutsideView)
{
    float fSqrtThird = Mathf::Sqrt(1.0f/3.0f);
    int i;

    // allocate vertices
    int iVQuantity = 8;
    Vector3f* akVertex = new Vector3f[iVQuantity];
    akVertex[0] = Vector3f(-fSqrtThird,-fSqrtThird,-fSqrtThird);
    akVertex[1] = Vector3f( fSqrtThird,-fSqrtThird,-fSqrtThird);
    akVertex[2] = Vector3f( fSqrtThird, fSqrtThird,-fSqrtThird);
    akVertex[3] = Vector3f(-fSqrtThird, fSqrtThird,-fSqrtThird);
    akVertex[4] = Vector3f(-fSqrtThird,-fSqrtThird, fSqrtThird);
    akVertex[5] = Vector3f( fSqrtThird,-fSqrtThird, fSqrtThird);
    akVertex[6] = Vector3f( fSqrtThird, fSqrtThird, fSqrtThird);
    akVertex[7] = Vector3f(-fSqrtThird, fSqrtThird, fSqrtThird);

    // allocate normals if requested
    Vector3f* akNormal = NULL;
    if ( bWantNormals )
    {
        akNormal = new Vector3f[iVQuantity];
        for (i = 0; i < iVQuantity; i++)
            akNormal[i] = akVertex[i];
    }

    // allocate colors if requested
    ColorRGB* akColor = NULL;
    if ( bWantColors )
    {
        akColor = new ColorRGB[iVQuantity];
        memset(akColor,0,iVQuantity*sizeof(ColorRGB));
    }

    // allocate texture coordinates if requested
    Vector2f* akUV = NULL;
    if ( bWantUVs )
    {
        akUV = new Vector2f[iVQuantity];
        for (i = 0; i < iVQuantity; i++)
        {
            if ( Mathf::FAbs(akVertex[i].Z()) < 1.0f )
            {
                akUV[i].X() = 0.5f*(1.0f + Mathf::ATan2(akVertex[i].Y(),
                    akVertex[i].X())*Mathf::INV_PI);
            }
            else
            {
                akUV[i].X() = 0.5f;
            }
            akUV[i].Y() = Mathf::ACos(akVertex[i].Z())*Mathf::INV_PI;
        }
    }

    // allocate connectivity
    int iTQuantity = 12;
    int* aiConnect = new int[3*iTQuantity];
    aiConnect[ 0] = 0;  aiConnect[ 1] = 3;  aiConnect[ 2] = 2;
    aiConnect[ 3] = 0;  aiConnect[ 4] = 2;  aiConnect[ 5] = 1;
    aiConnect[ 6] = 0;  aiConnect[ 7] = 1;  aiConnect[ 8] = 5;
    aiConnect[ 9] = 0;  aiConnect[10] = 5;  aiConnect[11] = 4;
    aiConnect[12] = 0;  aiConnect[13] = 4;  aiConnect[14] = 7;
    aiConnect[15] = 0;  aiConnect[16] = 7;  aiConnect[17] = 3;
    aiConnect[18] = 6;  aiConnect[19] = 5;  aiConnect[20] = 1;
    aiConnect[21] = 6;  aiConnect[22] = 1;  aiConnect[23] = 2;
    aiConnect[24] = 6;  aiConnect[25] = 2;  aiConnect[26] = 3;
    aiConnect[27] = 6;  aiConnect[28] = 3;  aiConnect[29] = 7;
    aiConnect[30] = 6;  aiConnect[31] = 7;  aiConnect[32] = 4;
    aiConnect[33] = 6;  aiConnect[34] = 4;  aiConnect[35] = 5;

    if ( !bOutsideView )
    {
        for (i = 0; i < iTQuantity; i++)
        {
            int iSave = aiConnect[3*i+1];
            aiConnect[3*i+1] = aiConnect[3*i+2];
            aiConnect[3*i+2] = iSave;
        }
    }

    if ( rpkMesh )
    {
        rpkMesh->Reconstruct(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
    else
    {
        rpkMesh = new TriMesh(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
}
//----------------------------------------------------------------------------
void Wml::CreateOctahedronMesh (TriMesh*& rpkMesh, bool bWantNormals,
    bool bWantColors, bool bWantUVs, bool bOutsideView)
{
    int i;

    // allocate vertices
    int iVQuantity = 6;
    Vector3f* akVertex = new Vector3f[iVQuantity];
    akVertex[0] = Vector3f( 1.0f, 0.0f, 0.0f);
    akVertex[1] = Vector3f(-1.0f, 0.0f, 0.0f);
    akVertex[2] = Vector3f( 0.0f, 1.0f, 0.0f);
    akVertex[3] = Vector3f( 0.0f,-1.0f, 0.0f);
    akVertex[4] = Vector3f( 0.0f, 0.0f, 1.0f);
    akVertex[5] = Vector3f( 0.0f, 0.0f,-1.0f);

    // allocate normals if requested
    Vector3f* akNormal = NULL;
    if ( bWantNormals )
    {
        akNormal = new Vector3f[iVQuantity];
        for (i = 0; i < iVQuantity; i++)
            akNormal[i] = akVertex[i];
    }

    // allocate colors if requested
    ColorRGB* akColor = NULL;
    if ( bWantColors )
    {
        akColor = new ColorRGB[iVQuantity];
        memset(akColor,0,iVQuantity*sizeof(ColorRGB));
    }

    // allocate texture coordinates if requested
    Vector2f* akUV = NULL;
    if ( bWantUVs )
    {
        akUV = new Vector2f[iVQuantity];
        for (i = 0; i < iVQuantity; i++)
        {
            if ( Mathf::FAbs(akVertex[i].Z()) < 1.0f )
            {
                akUV[i].X() = 0.5f*(1.0f + Mathf::ATan2(akVertex[i].Y(),
                    akVertex[i].X())*Mathf::INV_PI);
            }
            else
            {
                akUV[i].X() = 0.5f;
            }
            akUV[i].Y() = Mathf::ACos(akVertex[i].Z())*Mathf::INV_PI;
        }
    }

    // allocate connectivity
    int iTQuantity = 8;
    int* aiConnect = new int[3*iTQuantity];
    aiConnect[ 0] = 4;  aiConnect[ 1] = 0;  aiConnect[ 2] = 2;
    aiConnect[ 3] = 4;  aiConnect[ 4] = 2;  aiConnect[ 5] = 1;
    aiConnect[ 6] = 4;  aiConnect[ 7] = 1;  aiConnect[ 8] = 3;
    aiConnect[ 9] = 4;  aiConnect[10] = 3;  aiConnect[11] = 0;
    aiConnect[12] = 5;  aiConnect[13] = 2;  aiConnect[14] = 0;
    aiConnect[15] = 5;  aiConnect[16] = 1;  aiConnect[17] = 2;
    aiConnect[18] = 5;  aiConnect[19] = 3;  aiConnect[20] = 1;
    aiConnect[21] = 5;  aiConnect[22] = 0;  aiConnect[23] = 3;

    if ( !bOutsideView )
    {
        for (i = 0; i < iTQuantity; i++)
        {
            int iSave = aiConnect[3*i+1];
            aiConnect[3*i+1] = aiConnect[3*i+2];
            aiConnect[3*i+2] = iSave;
        }
    }

    if ( rpkMesh )
    {
        rpkMesh->Reconstruct(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
    else
    {
        rpkMesh = new TriMesh(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
}
//----------------------------------------------------------------------------
void Wml::CreateDodecahedronMesh (TriMesh*& rpkMesh, bool bWantNormals,
    bool bWantColors, bool bWantUVs, bool bOutsideView)
{
    float fA = 1.0f/Mathf::Sqrt(3.0f);
    float fB = Mathf::Sqrt((3.0f-Mathf::Sqrt(5.0f))/6.0f);
    float fC = Mathf::Sqrt((3.0f+Mathf::Sqrt(5.0f))/6.0f);
    int i;

    // allocate vertices
    int iVQuantity = 20;
    Vector3f* akVertex = new Vector3f[20];
    akVertex[ 0] = Vector3f( fA, fA, fA);
    akVertex[ 1] = Vector3f( fA, fA,-fA);
    akVertex[ 2] = Vector3f( fA,-fA, fA);
    akVertex[ 3] = Vector3f( fA,-fA,-fA);
    akVertex[ 4] = Vector3f(-fA, fA, fA);
    akVertex[ 5] = Vector3f(-fA, fA,-fA);
    akVertex[ 6] = Vector3f(-fA,-fA, fA);
    akVertex[ 7] = Vector3f(-fA,-fA,-fA);
    akVertex[ 8] = Vector3f(  fB,  fC, 0.0f);
    akVertex[ 9] = Vector3f( -fB,  fC, 0.0f);
    akVertex[10] = Vector3f(  fB, -fC, 0.0f);
    akVertex[11] = Vector3f( -fB, -fC, 0.0f);
    akVertex[12] = Vector3f(  fC, 0.0f,  fB);
    akVertex[13] = Vector3f(  fC, 0.0f, -fB);
    akVertex[14] = Vector3f( -fC, 0.0f,  fB);
    akVertex[15] = Vector3f( -fC, 0.0f, -fB);
    akVertex[16] = Vector3f(0.0f,   fB,  fC);
    akVertex[17] = Vector3f(0.0f,  -fB,  fC);
    akVertex[18] = Vector3f(0.0f,   fB, -fC);
    akVertex[19] = Vector3f(0.0f,  -fB, -fC);

    // allocate normals if requested
    Vector3f* akNormal = NULL;
    if ( bWantNormals )
    {
        akNormal = new Vector3f[iVQuantity];
        for (i = 0; i < iVQuantity; i++)
            akNormal[i] = akVertex[i];
    }

    // allocate colors if requested
    ColorRGB* akColor = NULL;
    if ( bWantColors )
    {
        akColor = new ColorRGB[iVQuantity];
        memset(akColor,0,iVQuantity*sizeof(ColorRGB));
    }

    // allocate texture coordinates if requested
    Vector2f* akUV = NULL;
    if ( bWantUVs )
    {
        akUV = new Vector2f[iVQuantity];
        for (i = 0; i < iVQuantity; i++)
        {
            if ( Mathf::FAbs(akVertex[i].Z()) < 1.0f )
            {
                akUV[i].X() = 0.5f*(1.0f + Mathf::ATan2(akVertex[i].Y(),
                    akVertex[i].X())*Mathf::INV_PI);
            }
            else
            {
                akUV[i].X() = 0.5f;
            }
            akUV[i].Y() = Mathf::ACos(akVertex[i].Z())*Mathf::INV_PI;
        }
    }

    // allocate connectivity
    int iTQuantity = 36;
    int* aiConnect = new int[3*iTQuantity];
    aiConnect[  0] =  0;  aiConnect[  1] =  8;  aiConnect[  2] =  9;
    aiConnect[  3] =  0;  aiConnect[  4] =  9;  aiConnect[  5] =  4;
    aiConnect[  6] =  0;  aiConnect[  7] =  4;  aiConnect[  8] = 16;
    aiConnect[  9] =  0;  aiConnect[ 10] = 12;  aiConnect[ 11] = 13;
    aiConnect[ 12] =  0;  aiConnect[ 13] = 13;  aiConnect[ 14] =  1;
    aiConnect[ 15] =  0;  aiConnect[ 16] =  1;  aiConnect[ 17] =  8;
    aiConnect[ 18] =  0;  aiConnect[ 19] = 16;  aiConnect[ 20] = 17;
    aiConnect[ 21] =  0;  aiConnect[ 22] = 17;  aiConnect[ 23] =  2;
    aiConnect[ 24] =  0;  aiConnect[ 25] =  2;  aiConnect[ 26] = 12;
    aiConnect[ 27] =  8;  aiConnect[ 28] =  1;  aiConnect[ 29] = 18;
    aiConnect[ 30] =  8;  aiConnect[ 31] = 18;  aiConnect[ 32] =  5;
    aiConnect[ 33] =  8;  aiConnect[ 34] =  5;  aiConnect[ 35] =  9;
    aiConnect[ 36] = 12;  aiConnect[ 37] =  2;  aiConnect[ 38] = 10;
    aiConnect[ 39] = 12;  aiConnect[ 40] = 10;  aiConnect[ 41] =  3;
    aiConnect[ 42] = 12;  aiConnect[ 43] =  3;  aiConnect[ 44] = 13;
    aiConnect[ 45] = 16;  aiConnect[ 46] =  4;  aiConnect[ 47] = 14;
    aiConnect[ 48] = 16;  aiConnect[ 49] = 14;  aiConnect[ 50] =  6;
    aiConnect[ 51] = 16;  aiConnect[ 52] =  6;  aiConnect[ 53] = 17;
    aiConnect[ 54] =  9;  aiConnect[ 55] =  5;  aiConnect[ 56] = 15;
    aiConnect[ 57] =  9;  aiConnect[ 58] = 15;  aiConnect[ 59] = 14;
    aiConnect[ 60] =  9;  aiConnect[ 61] = 14;  aiConnect[ 62] =  4;
    aiConnect[ 63] =  6;  aiConnect[ 64] = 11;  aiConnect[ 65] = 10;
    aiConnect[ 66] =  6;  aiConnect[ 67] = 10;  aiConnect[ 68] =  2;
    aiConnect[ 69] =  6;  aiConnect[ 70] =  2;  aiConnect[ 71] = 17;
    aiConnect[ 72] =  3;  aiConnect[ 73] = 19;  aiConnect[ 74] = 18;
    aiConnect[ 75] =  3;  aiConnect[ 76] = 18;  aiConnect[ 77] =  1;
    aiConnect[ 78] =  3;  aiConnect[ 79] =  1;  aiConnect[ 80] = 13;
    aiConnect[ 81] =  7;  aiConnect[ 82] = 15;  aiConnect[ 83] =  5;
    aiConnect[ 84] =  7;  aiConnect[ 85] =  5;  aiConnect[ 86] = 18;
    aiConnect[ 87] =  7;  aiConnect[ 88] = 18;  aiConnect[ 89] = 19;
    aiConnect[ 90] =  7;  aiConnect[ 91] = 11;  aiConnect[ 92] =  6;
    aiConnect[ 93] =  7;  aiConnect[ 94] =  6;  aiConnect[ 95] = 14;
    aiConnect[ 96] =  7;  aiConnect[ 97] = 14;  aiConnect[ 98] = 15;
    aiConnect[ 99] =  7;  aiConnect[100] = 19;  aiConnect[101] =  3;
    aiConnect[102] =  7;  aiConnect[103] =  3;  aiConnect[104] = 10;
    aiConnect[105] =  7;  aiConnect[106] = 10;  aiConnect[107] = 11;

    if ( !bOutsideView )
    {
        for (i = 0; i < iTQuantity; i++)
        {
            int iSave = aiConnect[3*i+1];
            aiConnect[3*i+1] = aiConnect[3*i+2];
            aiConnect[3*i+2] = iSave;
        }
    }

    if ( rpkMesh )
    {
        rpkMesh->Reconstruct(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
    else
    {
        rpkMesh = new TriMesh(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
}
//----------------------------------------------------------------------------
void Wml::CreateIcosahedronMesh (TriMesh*& rpkMesh, bool bWantNormals,
    bool bWantColors, bool bWantUVs, bool bOutsideView)
{
    float fGoldenRatio = 0.5f*(1.0f+Mathf::Sqrt(5.0f));
    float fInvRoot = 1.0f/Mathf::Sqrt(1.0f+fGoldenRatio*fGoldenRatio);
    float fU = fGoldenRatio*fInvRoot;
    float fV = fInvRoot;
    int i;

    // allocate vertices
    int iVQuantity = 12;
    Vector3f* akVertex = new Vector3f[iVQuantity];
    akVertex[ 0] = Vector3f(  fU,  fV,0.0f);
    akVertex[ 1] = Vector3f( -fU,  fV,0.0f);
    akVertex[ 2] = Vector3f(  fU, -fV,0.0f);
    akVertex[ 3] = Vector3f( -fU, -fV,0.0f);
    akVertex[ 4] = Vector3f(  fV,0.0f,  fU);
    akVertex[ 5] = Vector3f(  fV,0.0f, -fU);
    akVertex[ 6] = Vector3f( -fV,0.0f,  fU);
    akVertex[ 7] = Vector3f( -fV,0.0f, -fU);
    akVertex[ 8] = Vector3f(0.0f,  fU,  fV);
    akVertex[ 9] = Vector3f(0.0f, -fU,  fV);
    akVertex[10] = Vector3f(0.0f,  fU, -fV);
    akVertex[11] = Vector3f(0.0f, -fU, -fV);

    // allocate normals if requested
    Vector3f* akNormal = NULL;
    if ( bWantNormals )
    {
        akNormal = new Vector3f[iVQuantity];
        for (i = 0; i < iVQuantity; i++)
            akNormal[i] = akVertex[i];
    }

    // allocate colors if requested
    ColorRGB* akColor = NULL;
    if ( bWantColors )
    {
        akColor = new ColorRGB[iVQuantity];
        memset(akColor,0,iVQuantity*sizeof(ColorRGB));
    }

    // allocate texture coordinates if requested
    Vector2f* akUV = NULL;
    if ( bWantUVs )
    {
        akUV = new Vector2f[iVQuantity];
        for (i = 0; i < iVQuantity; i++)
        {
            if ( Mathf::FAbs(akVertex[i].Z()) < 1.0f )
            {
                akUV[i].X() = 0.5f*(1.0f + Mathf::ATan2(akVertex[i].Y(),
                    akVertex[i].X())*Mathf::INV_PI);
            }
            else
            {
                akUV[i].X() = 0.5f;
            }
            akUV[i].Y() = Mathf::ACos(akVertex[i].Z())*Mathf::INV_PI;
        }
    }

    // allocate connectivity
    int iTQuantity = 20;
    int* aiConnect = new int[3*iTQuantity];
    aiConnect[ 0] =  0;  aiConnect[ 1] =  8;  aiConnect[ 2] =  4;
    aiConnect[ 3] =  0;  aiConnect[ 4] =  5;  aiConnect[ 5] = 10;
    aiConnect[ 6] =  2;  aiConnect[ 7] =  4;  aiConnect[ 8] =  9;
    aiConnect[ 9] =  2;  aiConnect[10] = 11;  aiConnect[11] =  5;
    aiConnect[12] =  1;  aiConnect[13] =  6;  aiConnect[14] =  8;
    aiConnect[15] =  1;  aiConnect[16] = 10;  aiConnect[17] =  7;
    aiConnect[18] =  3;  aiConnect[19] =  9;  aiConnect[20] =  6;
    aiConnect[21] =  3;  aiConnect[22] =  7;  aiConnect[23] = 11;
    aiConnect[24] =  0;  aiConnect[25] = 10;  aiConnect[26] =  8;
    aiConnect[27] =  1;  aiConnect[28] =  8;  aiConnect[29] = 10;
    aiConnect[30] =  2;  aiConnect[31] =  9;  aiConnect[32] = 11;
    aiConnect[33] =  3;  aiConnect[34] = 11;  aiConnect[35] =  9;
    aiConnect[36] =  4;  aiConnect[37] =  2;  aiConnect[38] =  0;
    aiConnect[39] =  5;  aiConnect[40] =  0;  aiConnect[41] =  2;
    aiConnect[42] =  6;  aiConnect[43] =  1;  aiConnect[44] =  3;
    aiConnect[45] =  7;  aiConnect[46] =  3;  aiConnect[47] =  1;
    aiConnect[48] =  8;  aiConnect[49] =  6;  aiConnect[50] =  4;
    aiConnect[51] =  9;  aiConnect[52] =  4;  aiConnect[53] =  6;
    aiConnect[54] = 10;  aiConnect[55] =  5;  aiConnect[56] =  7;
    aiConnect[57] = 11;  aiConnect[58] =  7;  aiConnect[59] =  5;

    if ( !bOutsideView )
    {
        for (i = 0; i < iTQuantity; i++)
        {
            int iSave = aiConnect[3*i+1];
            aiConnect[3*i+1] = aiConnect[3*i+2];
            aiConnect[3*i+2] = iSave;
        }
    }

    if ( rpkMesh )
    {
        rpkMesh->Reconstruct(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
    else
    {
        rpkMesh = new TriMesh(iVQuantity,akVertex,akNormal,akColor,akUV,
            iTQuantity,aiConnect);
    }
}
//----------------------------------------------------------------------------
