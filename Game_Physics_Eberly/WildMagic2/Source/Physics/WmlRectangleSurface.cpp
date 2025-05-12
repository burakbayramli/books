// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "WmlRectangleSurface.h"
#include "WmlCullState.h"
using namespace Wml;

WmlImplementRTTI(RectangleSurface,TriMesh);
WmlImplementStream(RectangleSurface);

//----------------------------------------------------------------------------
RectangleSurface::RectangleSurface (ParametricSurfacef* pkSurface,
    int iUSamples, int iVSamples, bool bWantNormals, bool bWantColors,
    bool bDoubleSided, const Vector2f* pkTextureMin,
    const Vector2f* pkTextureMax)
{
    assert( pkSurface && pkSurface->IsRectangular() );

    m_pkSurface = pkSurface;
    m_iUSamples = iUSamples;
    m_iVSamples = iVSamples;

    float fUMin = m_pkSurface->GetUMin();
    float fURan = m_pkSurface->GetUMax() - fUMin;
    float fUDelta = fURan/(float)(m_iUSamples-1);
    float fVMin = m_pkSurface->GetVMin();
    float fVRan = m_pkSurface->GetVMax() - fVMin;
    float fVDelta = fVRan/(float)(m_iVSamples-1);

    // compute the vertices, normals, and texture coordinates
    int iVQuantity = m_iUSamples*m_iVSamples;
    Vector3f* akVertex = new Vector3f[iVQuantity];

    Vector3f* akNormal = NULL;
    if ( bWantNormals )
        akNormal = new Vector3f[iVQuantity];

    Vector2f* akTexture = NULL;
    float fTUDelta, fTVDelta;
    if ( pkTextureMin && pkTextureMax )
    {
        akTexture = new Vector2f[iVQuantity];
        fTUDelta = (pkTextureMax->X() - pkTextureMin->X())/fURan;
        fTVDelta = (pkTextureMax->Y() - pkTextureMin->Y())/fVRan;
    }

    int iU, iV, i;
    for (iU = 0, i = 0; iU < m_iUSamples; iU++)
    {
        float fUIncr = fUDelta*iU;
        float fU = fUMin + fUIncr;
        for (iV = 0; iV < m_iVSamples; iV++)
        {
            float fVIncr = fVDelta*iV;
            float fV = fVMin + fVIncr;
            akVertex[i] = m_pkSurface->GetPosition(fU,fV);

            if ( akNormal )
                akNormal[i] = m_pkSurface->GetNormal(fU,fV);

            if ( akTexture )
            {
                akTexture[i].X() = pkTextureMin->X() + fTUDelta*fUIncr;
                akTexture[i].Y() = pkTextureMin->Y() + fTVDelta*fVIncr;
            }
            i++;
        }
    }

    // allocate the surface colors
    ColorRGB* akColor = NULL;
    if ( bWantColors )
    {
        akColor = new ColorRGB[iVQuantity];
        memset(akColor,0,iVQuantity*sizeof(ColorRGB));
    }

    // compute the surface triangle connectivity
    int iTQuantity = 2*(m_iUSamples-1)*(m_iVSamples-1);
    int* aiConnect = new int[3*iTQuantity];
    int* aiLocalConnect = aiConnect;
    for (iU = 0, i = 0; iU < m_iUSamples-1; iU++)
    {
        int i0 = i;
        int i1 = i0 + 1;
        i += m_iVSamples;
        int i2 = i;
        int i3 = i2 + 1;
        for (iV = 0; iV < m_iVSamples-1; iV++, aiLocalConnect += 6)
        {
            aiLocalConnect[0] = i0++;
            aiLocalConnect[1] = i1;
            aiLocalConnect[2] = i2;
            aiLocalConnect[3] = i1++;
            aiLocalConnect[4] = i3++;
            aiLocalConnect[5] = i2++;
        }
    }

    // create the triangle mesh for the tube surface
    Reconstruct(iVQuantity,akVertex,akNormal,akColor,akTexture,iTQuantity,
        aiConnect);
    UpdateModelBound();

    if ( bDoubleSided )
    {
        // disable culling
        CullState* pkCS = new CullState;
        pkCS->Enabled() = false;
        SetRenderState(pkCS);
    }
}
//----------------------------------------------------------------------------
RectangleSurface::RectangleSurface ()
{
    m_pkSurface = NULL;
}
//----------------------------------------------------------------------------
RectangleSurface::~RectangleSurface ()
{
    delete m_pkSurface;
}
//----------------------------------------------------------------------------
void RectangleSurface::UpdateSurface ()
{
    float fUMin = m_pkSurface->GetUMin();
    float fUDelta = (m_pkSurface->GetUMax() - fUMin)/(float)(m_iUSamples-1);
    float fVMin = m_pkSurface->GetVMin();
    float fVDelta = (m_pkSurface->GetVMax() - fVMin)/(float)(m_iVSamples-1);

    for (int iU = 0, i = 0; iU < m_iUSamples; iU++)
    {
        float fU = fUMin + fUDelta*iU;
        for (int iV = 0; iV < m_iVSamples; iV++)
        {
            float fV = fVMin + fVDelta*iV;
            m_akVertex[i] = m_pkSurface->GetPosition(fU,fV);

            if ( m_akNormal )
                m_akNormal[i] = m_pkSurface->GetNormal(fU,fV);

            i++;
        }
    }

    UpdateModelBound();
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* RectangleSurface::Factory (Stream& rkStream)
{
    RectangleSurface* pkObject = new RectangleSurface;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void RectangleSurface::Load (Stream& rkStream, Stream::Link* pkLink)
{
    TriMesh::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_iUSamples);
    StreamRead(rkStream,m_iVSamples);

    // TO DO.  See note in RectangleSurface::Save.
    m_pkSurface = NULL;
}
//----------------------------------------------------------------------------
void RectangleSurface::Link (Stream& rkStream, Stream::Link* pkLink)
{
    TriMesh::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool RectangleSurface::Register (Stream& rkStream)
{
    return TriMesh::Register(rkStream);
}
//----------------------------------------------------------------------------
void RectangleSurface::Save (Stream& rkStream)
{
    TriMesh::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_iUSamples);
    StreamWrite(rkStream,m_iVSamples);

    // TO DO.  The class ParametricSurface3 is abstract and does not know
    // about the data representation for the derived-class object that is
    // passed to the RectangleSurface constructor.  Because of this, any
    // loaded RectangleSurface object will require the application to
    // manually set the surface via the Surface() member function.
    //
    // Streaming support should probably be added to the surface classes.
}
//----------------------------------------------------------------------------
StringTree* RectangleSurface::SaveStrings ()
{
    StringTree* pkTree = new StringTree(3,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("u samples =",m_iUSamples));
    pkTree->SetString(2,MakeString("v samples =",m_iVSamples));

    // children
    pkTree->SetChild(0,TriMesh::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int RectangleSurface::GetMemoryUsed () const
{
    // TO DO.  Does not count the size of the Surface object.  The surface
    // class is virtual, so that system would need to provide a GetBytesUsed
    // member function.
    int iBaseSize = sizeof(RectangleSurface) - sizeof(TriMesh);
    int iTotalSize = iBaseSize + TriMesh::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int RectangleSurface::GetDiskUsed () const
{
    return TriMesh::GetDiskUsed() +
        sizeof(m_iUSamples) +
        sizeof(m_iVSamples);
}
//----------------------------------------------------------------------------
