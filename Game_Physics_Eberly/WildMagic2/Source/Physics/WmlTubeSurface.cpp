// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "WmlTubeSurface.h"
using namespace Wml;

WmlImplementRTTI(TubeSurface,TriMesh);
WmlImplementStream(TubeSurface);

//----------------------------------------------------------------------------
TubeSurface::TubeSurface (Curve3f* pkMedial, RadialFunction oRadial,
    bool bClosed, const Vector3f& rkUpVector, int iMedialSamples,
    int iSliceSamples, bool bWantNormals, bool bWantColors,
    bool bSampleByArcLength, bool bInsideView, const Vector2f* pkTextureMin,
    const Vector2f* pkTextureMax)
{
    assert( pkMedial && oRadial );
    assert((pkTextureMin && pkTextureMax)||(!pkTextureMin && !pkTextureMax));

    m_pkMedial = pkMedial;
    m_oRadial = oRadial;
    m_kUpVector = rkUpVector;
    m_iMedialSamples = iMedialSamples;
    m_iSliceSamples = iSliceSamples;
    m_bClosed = bClosed;
    m_bSampleByArcLength = bSampleByArcLength;

    // compute the surface vertices
    int iVQuantity;
    if ( m_bClosed )
        iVQuantity = (m_iSliceSamples+1)*(m_iMedialSamples+1);
    else
        iVQuantity = (m_iSliceSamples+1)*m_iMedialSamples;

    Vector3f* akVertex = new Vector3f[iVQuantity];
    ComputeSinCos();
    ComputeVertices(akVertex);

    // compute the surface normals
    Vector3f* akNormal = NULL;
    if ( bWantNormals )
    {
        akNormal = new Vector3f[iVQuantity];
        ComputeNormals(akVertex,akNormal);
    }

    // allocate the surface colors
    ColorRGB* akColor = NULL;
    if ( bWantColors )
    {
        akColor = new ColorRGB[iVQuantity];
        memset(akColor,0,iVQuantity*sizeof(ColorRGB));
    }

    // compute the surface textures coordinates
    Vector2f* akTexture = NULL;
    if ( pkTextureMin && pkTextureMax )
    {
        akTexture = new Vector2f[iVQuantity];
        ComputeTextures(*pkTextureMin,*pkTextureMax,akTexture);
    }

    // compute the surface triangle connectivity
    int iTQuantity;
    int* aiConnect;
    ComputeConnectivity(iTQuantity,aiConnect,bInsideView);

    // create the triangle mesh for the tube surface
    Reconstruct(iVQuantity,akVertex,akNormal,akColor,akTexture,iTQuantity,
        aiConnect);
    UpdateModelBound();
}
//----------------------------------------------------------------------------
TubeSurface::TubeSurface ()
{
    m_pkMedial = NULL;
    m_oRadial = NULL;
    m_afSin = NULL;
    m_afCos = NULL;
}
//----------------------------------------------------------------------------
TubeSurface::~TubeSurface ()
{
    delete m_pkMedial;
    delete[] m_afSin;
    delete[] m_afCos;
}
//----------------------------------------------------------------------------
void TubeSurface::ComputeSinCos ()
{
    // Compute slice vertex coefficients.  The first and last coefficients
    // are duplicated to allow a closed cross section that has two different
    // pairs of texture coordinates at the shared vertex.

    m_afSin = new float[m_iSliceSamples + 1];
    m_afCos = new float[m_iSliceSamples + 1];

    float fInvSliceSamples = 1.0f/(float)m_iSliceSamples;
    for (int i = 0; i < m_iSliceSamples; i++)
    {
        float fAngle = Mathf::TWO_PI*fInvSliceSamples*i;
        m_afCos[i] = Mathf::Cos(fAngle);
        m_afSin[i] = Mathf::Sin(fAngle);
    }
    m_afSin[m_iSliceSamples] = m_afSin[0];
    m_afCos[m_iSliceSamples] = m_afCos[0];
}
//----------------------------------------------------------------------------
void TubeSurface::ComputeVertices (Vector3f* akVertex)
{
    float fTMin = m_pkMedial->GetMinTime();
    float fTRange = m_pkMedial->GetMaxTime() - fTMin;

    // sampling by arc length requires the total length of the curve
    float fTotalLength;
    if ( m_bSampleByArcLength )
        fTotalLength = m_pkMedial->GetTotalLength();
    else
        fTotalLength = 0.0f;

    // vertex construction requires a normalized time (uses a division)
    float fDenom;
    if ( m_bClosed )
        fDenom = 1.0f/(float)m_iMedialSamples;
    else
        fDenom = 1.0f/(float)(m_iMedialSamples-1);

    for (int iM = 0, iV = 0; iM < m_iMedialSamples; iM++)
    {
        float fT;
        if ( m_bSampleByArcLength )
            fT = m_pkMedial->GetTime(iM*fTotalLength*fDenom);
        else
            fT = fTMin + iM*fTRange*fDenom;

        float fRadius = m_oRadial(fT);

        // compute frame (position P, tangent T, normal N, binormal B)
        Vector3f kP, kT, kN, kB;
        if ( m_kUpVector != Vector3f::ZERO )
        {
            // Always use 'up' vector N rather than curve normal.  You must
            // constrain the curve so that T and N are never parallel.  To
            // build the frame from this, let
            //     B = Cross(T,N)/Length(Cross(T,N))
            // and replace
            //     N = Cross(B,T)/Length(Cross(B,T)).
            kP = m_pkMedial->GetPosition(fT);
            kT = m_pkMedial->GetTangent(fT);
            kB = kT.UnitCross(m_kUpVector);
            kN = kB.UnitCross(kT);
        }
        else
        {
            // use Frenet frame to create slices
            m_pkMedial->GetFrame(fT,kP,kT,kN,kB);
        }

        // compute slice vertices, duplication at end point as noted earlier
        int iSave = iV;
        for (int i = 0; i < m_iSliceSamples; i++)
        {
            akVertex[iV] = kP + fRadius*(m_afCos[i]*kN + m_afSin[i]*kB);
            iV++;
        }
        akVertex[iV] = akVertex[iSave];
        iV++;
    }

    if ( m_bClosed )
    {
        for (int i = 0; i <= m_iSliceSamples; i++)
        {
            int i1 = Index(i,m_iMedialSamples);
            int i0 = Index(i,0);
            akVertex[i1] = akVertex[i0];
        }
    }
}
//----------------------------------------------------------------------------
void TubeSurface::ComputeNormals (const Vector3f* akVertex,
    Vector3f* akNormal)
{
    int iS, iSm, iSp, iM, iMm, iMp;
    Vector3f kDir0, kDir1;

    // interior normals (central differences)
    for (iM = 1; iM <= m_iMedialSamples-2; iM++)
    {
        for (iS = 0; iS < m_iSliceSamples; iS++)
        {
            iSm = ( iS > 0 ? iS-1 : m_iSliceSamples-1 );
            iSp = iS + 1;
            iMm = iM - 1;
            iMp = iM + 1;
            kDir0 = akVertex[Index(iSm,iM)] - akVertex[Index(iSp,iM)];
            kDir1 = akVertex[Index(iS,iMm)] - akVertex[Index(iS,iMp)];
            akNormal[Index(iS,iM)] = kDir0.UnitCross(kDir1);
        }
        akNormal[Index(m_iSliceSamples,iM)] = akNormal[Index(0,iM)];
    }

    // boundary normals
    if ( m_bClosed )
    {
        // central differences
        for (iS = 0; iS < m_iSliceSamples; iS++)
        {
            iSm = ( iS > 0 ? iS-1 : m_iSliceSamples-1 );
            iSp = iS + 1;

            // m = 0
            kDir0 = akVertex[Index(iSm,0)] - akVertex[Index(iSp,0)];
            kDir1 = akVertex[Index(iS,m_iMedialSamples-1)] -
                akVertex[Index(iS,1)];
            akNormal[iS] = kDir0.UnitCross(kDir1);

            // m = max
            akNormal[Index(iS,m_iMedialSamples)] = akNormal[Index(iS,0)];
        }
        akNormal[Index(m_iSliceSamples,0)] = akNormal[Index(0,0)];
        akNormal[Index(m_iSliceSamples,m_iMedialSamples)] =
            akNormal[Index(0,m_iMedialSamples)];
    }
    else
    {
        // one-sided finite differences

        // m = 0
        for (iS = 0; iS < m_iSliceSamples; iS++)
        {
            iSm = ( iS > 0 ? iS-1 : m_iSliceSamples-1 );
            iSp = iS + 1;
            kDir0 = akVertex[Index(iSm,0)] - akVertex[Index(iSp,0)];
            kDir1 = akVertex[Index(iS,0)] - akVertex[Index(iS,1)];
            akNormal[Index(iS,0)] = kDir0.UnitCross(kDir1);
        }
        akNormal[Index(m_iSliceSamples,0)] = akNormal[Index(0,0)];

        // m = max-1
        for (iS = 0; iS < m_iSliceSamples; iS++)
        {
            iSm = ( iS > 0 ? iS-1 : m_iSliceSamples-1 );
            iSp = iS + 1;
            kDir0 = akVertex[Index(iSm,m_iMedialSamples-1)] -
                akVertex[Index(iSp,m_iMedialSamples-1)];
            kDir1 = akVertex[Index(iS,m_iMedialSamples-2)] -
                akVertex[Index(iS,m_iMedialSamples-1)];
            akNormal[iS] = kDir0.UnitCross(kDir1);
        }
        akNormal[Index(m_iSliceSamples,m_iMedialSamples-1)] =
            akNormal[Index(0,m_iMedialSamples-1)];
    }
}
//----------------------------------------------------------------------------
void TubeSurface::ComputeTextures (const Vector2f& rkTextureMin,
    const Vector2f& rkTextureMax, Vector2f* akTexture)
{
    Vector2f kTextureRange = rkTextureMax - rkTextureMin;
    int iMMax = ( m_bClosed ? m_iMedialSamples : m_iMedialSamples - 1 );
    for (int iM = 0, iV = 0; iM <= iMMax; iM++)
    {
        float fMRatio = ((float)iM)/((float)iMMax);
        float fMValue = rkTextureMin.Y() + fMRatio*kTextureRange.Y();
        for (int iS = 0; iS <= m_iSliceSamples; iS++)
        {
            float fSRatio = ((float)iS)/((float)m_iSliceSamples);
            float fSValue = rkTextureMin.X() + fSRatio*kTextureRange.X();
            akTexture[iV].X() = fSValue;
            akTexture[iV].Y() = fMValue;
            iV++;
        }
    }
}
//----------------------------------------------------------------------------
void TubeSurface::ComputeConnectivity (int& riTQuantity, int*& raiConnect,
    bool bInsideView)
{
    if ( m_bClosed )
        riTQuantity = 2*m_iSliceSamples*m_iMedialSamples;
    else
        riTQuantity = 2*m_iSliceSamples*(m_iMedialSamples-1);

    raiConnect = new int[3*riTQuantity];

    int iM, iMStart, i0, i1, i2, i3, i;
    int* aiConnect = raiConnect;
    for (iM = 0, iMStart = 0; iM < m_iMedialSamples-1; iM++)
    {
        i0 = iMStart;
        i1 = i0 + 1;
        iMStart += m_iSliceSamples + 1;
        i2 = iMStart;
        i3 = i2 + 1;
        for (i = 0; i < m_iSliceSamples; i++, aiConnect += 6)
        {
            if ( bInsideView )
            {
                aiConnect[0] = i0++;
                aiConnect[1] = i2;
                aiConnect[2] = i1;
                aiConnect[3] = i1++;
                aiConnect[4] = i2++;
                aiConnect[5] = i3++;
            }
            else  // outside view
            {
                aiConnect[0] = i0++;
                aiConnect[1] = i1;
                aiConnect[2] = i2;
                aiConnect[3] = i1++;
                aiConnect[4] = i3++;
                aiConnect[5] = i2++;
            }
        }
    }

    if ( m_bClosed )
    {
        i0 = iMStart;
        i1 = i0 + 1;
        i2 = 0;
        i3 = i2 + 1;
        for (i = 0; i < m_iSliceSamples; i++, aiConnect += 6)
        {
            if ( bInsideView )
            {
                aiConnect[0] = i0++;
                aiConnect[1] = i2;
                aiConnect[2] = i1;
                aiConnect[3] = i1++;
                aiConnect[4] = i2++;
                aiConnect[5] = i3++;
            }
            else  // outside view
            {
                aiConnect[0] = i0++;
                aiConnect[1] = i1;
                aiConnect[2] = i2;
                aiConnect[3] = i1++;
                aiConnect[4] = i3++;
                aiConnect[5] = i2++;
            }
        }
    }
}
//----------------------------------------------------------------------------
void TubeSurface::GetTMinSlice (Vector3f* akSlice)
{
    for (int i = 0; i <= m_iSliceSamples; i++)
        akSlice[i] = m_akVertex[i];
}
//----------------------------------------------------------------------------
void TubeSurface::GetTMaxSlice (Vector3f* akSlice)
{
    int j = m_iVertexQuantity - m_iSliceSamples - 1;
    for (int i = 0; i <= m_iSliceSamples; i++, j++)
        akSlice[i] = m_akVertex[j];
}
//----------------------------------------------------------------------------
void TubeSurface::UpdateSurface ()
{
    ComputeVertices(m_akVertex);
    if ( m_akNormal )
        ComputeNormals(m_akVertex,m_akNormal);
    UpdateModelBound();
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* TubeSurface::Factory (Stream& rkStream)
{
    TubeSurface* pkObject = new TubeSurface;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void TubeSurface::Load (Stream& rkStream, Stream::Link* pkLink)
{
    TriMesh::Load(rkStream,pkLink);

    // native data
    if ( rkStream.GetVersion() <= Version(1,4) )
    {
        // These quantities are no longer streamed.
        bool bWantNormals;
        Vector2f kTextureMin, kTextureMax;

        StreamReadBool(rkStream,m_bClosed);
        StreamRead(rkStream,m_iMedialSamples);
        StreamRead(rkStream,m_iSliceSamples);
        StreamReadBool(rkStream,m_bSampleByArcLength);
        StreamReadBool(rkStream,bWantNormals);
        StreamRead(rkStream,m_kUpVector);
        StreamRead(rkStream,kTextureMin);
        StreamRead(rkStream,kTextureMax);

        ComputeSinCos();
    }
    else
    {
        StreamRead(rkStream,m_iMedialSamples);
        StreamRead(rkStream,m_iSliceSamples);
        StreamRead(rkStream,m_kUpVector);
        StreamRead(rkStream,m_afSin,m_iSliceSamples+1);
        StreamRead(rkStream,m_afCos,m_iSliceSamples+1);
        StreamReadBool(rkStream,m_bClosed);
        StreamReadBool(rkStream,m_bSampleByArcLength);
    }

    // TO DO.  See note in TubeSurface::Save.
    m_pkMedial = NULL;
    m_oRadial = NULL;
}
//----------------------------------------------------------------------------
void TubeSurface::Link (Stream& rkStream, Stream::Link* pkLink)
{
    TriMesh::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool TubeSurface::Register (Stream& rkStream)
{
    return TriMesh::Register(rkStream);
}
//----------------------------------------------------------------------------
void TubeSurface::Save (Stream& rkStream)
{
    TriMesh::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_iMedialSamples);
    StreamWrite(rkStream,m_iSliceSamples);
    StreamWrite(rkStream,m_kUpVector);
    StreamWrite(rkStream,m_afSin,m_iSliceSamples+1);
    StreamWrite(rkStream,m_afCos,m_iSliceSamples+1);
    StreamWriteBool(rkStream,m_bClosed);
    StreamWriteBool(rkStream,m_bSampleByArcLength);

    // TO DO.  The class Curve3 is abstract and does not know about the data
    // representation for the derived-class object that is passed to the
    // TubeSurface constructor.  RadialFunction is a type of function.  Saving
    // the function pointer is useless since on loading, there is no current
    // way to 'link' to the correct function pointer.  Because of this, any
    // loaded TubeSurface object will require the application to manually set
    // the curve and function via the Medial() and Radial() members.
    //
    // Streaming support should probably be added to the curve classes and
    // a mechanism for saving function pointers should probably be created.
}
//----------------------------------------------------------------------------
StringTree* TubeSurface::SaveStrings ()
{
    StringTree* pkTree = new StringTree(6,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("closed =",m_bClosed));
    pkTree->SetString(2,MakeString("medial samples=",m_iMedialSamples));
    pkTree->SetString(3,MakeString("slice samples=",m_iSliceSamples));
    pkTree->SetString(4,MakeString("sample by arc length =",
        m_bSampleByArcLength));
    pkTree->SetString(5,MakeString("up vector =",m_kUpVector));

    // children
    pkTree->SetChild(0,TriMesh::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int TubeSurface::GetMemoryUsed () const
{
    // TO DO.  Does not count the size of the Curve3 object.  The curve class
    // is virtual, so that system would need to provide a GetBytesUsed member
    // function.
    int iBaseSize = sizeof(TubeSurface) - sizeof(TriMesh);
    int iDynaSize = 2*(m_iSliceSamples+1)*sizeof(float);  // m_afSin, m_afCos
    int iTotalSize = iBaseSize + iDynaSize + TriMesh::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int TubeSurface::GetDiskUsed () const
{
    return TriMesh::GetDiskUsed() +
        sizeof(m_iMedialSamples) +
        sizeof(m_iSliceSamples) +
        sizeof(m_kUpVector) +
        2*(m_iSliceSamples+1)*sizeof(float) +  // m_afSin, m_afCos
        StreamBytesBool(m_bClosed) +
        StreamBytesBool(m_bSampleByArcLength);
}
//----------------------------------------------------------------------------
