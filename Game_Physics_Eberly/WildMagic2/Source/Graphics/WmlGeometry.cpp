// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlGeometry.h"
#include "WmlRenderer.h"
using namespace Wml;

WmlImplementRTTI(Geometry,Spatial);
WmlImplementStream(Geometry);

//----------------------------------------------------------------------------
Geometry::Geometry (int iVertexQuantity, Vector3f* akVertex,
    Vector3f* akNormal, ColorRGB* akColor, Vector2f* akTexture,
    Vector2f* akTexture1, Vector2f* akTexture2, Vector2f* akTexture3,
    Vector2f* akTextureBump, VertexShader* pkVertexShader,
    PixelShader* pkPixelShader)
{
    m_iVertexQuantity = iVertexQuantity;
    m_akVertex = akVertex;
    m_akNormal = akNormal;
    m_akColor = akColor;
    m_akTexture = akTexture;
    m_akTexture1 = akTexture1;
    m_akTexture2 = akTexture2;
    m_akTexture3 = akTexture3;
    m_akTextureBump = akTextureBump;

    m_pkVertexShaderConsts = NULL;
    m_pkPixelShaderConsts = NULL;
    SetVertexShader(pkVertexShader);
    SetPixelShader(pkPixelShader);

    if ( akTextureBump && !akColor )
        m_akColor = new ColorRGB[iVertexQuantity];

    UpdateModelBound();
}
//----------------------------------------------------------------------------
Geometry::Geometry ()
{
    m_iVertexQuantity = 0;
    m_akVertex = NULL;
    m_akNormal = NULL;
    m_akColor = NULL;
    m_akTexture = NULL;
    m_akTexture1 = NULL;
    m_akTexture2 = NULL;
    m_akTexture3 = NULL;
    m_akTextureBump = NULL;
    m_pkVertexShaderConsts = NULL;
    m_pkPixelShaderConsts = NULL;
    SetVertexShader( NULL );
    SetPixelShader( NULL );
}
//----------------------------------------------------------------------------
Geometry::~Geometry ()
{
    delete[] m_akVertex;
    delete[] m_akNormal;
    delete[] m_akColor;
    delete[] m_akTexture;
    delete[] m_akTexture1;
    delete[] m_akTexture2;
    delete[] m_akTexture3;
    delete[] m_akTextureBump;

    for (int i = 0; i < RenderState::RS_MAX_STATE; i++)
        m_aspkState[i] = NULL;

    SetVertexShader( NULL );
    SetPixelShader( NULL );
}
//----------------------------------------------------------------------------
void Geometry::Reconstruct (int iVertexQuantity)
{
    m_iVertexQuantity = iVertexQuantity;

    delete[] m_akVertex;
    delete[] m_akNormal;
    delete[] m_akColor;
    delete[] m_akTexture;
    delete[] m_akTexture1;
    delete[] m_akTexture2;
    delete[] m_akTexture3;
    delete[] m_akTextureBump;

    if ( m_iVertexQuantity > 0 )
    {
        m_akVertex = new Vector3f[m_iVertexQuantity];

        if ( m_akNormal )
            m_akNormal = new Vector3f[m_iVertexQuantity];

        if ( m_akColor )
            m_akColor = new ColorRGB[m_iVertexQuantity];

        if ( m_akTexture )
            m_akTexture = new Vector2f[m_iVertexQuantity];

        if ( m_akTexture1 )
            m_akTexture1 = new Vector2f[m_iVertexQuantity];

        if ( m_akTexture2 )
            m_akTexture2 = new Vector2f[m_iVertexQuantity];

        if ( m_akTexture3 )
            m_akTexture3 = new Vector2f[m_iVertexQuantity];

        if ( m_akTextureBump )
            m_akTextureBump = new Vector2f[m_iVertexQuantity];
    }
    else
    {
        m_akVertex = NULL;
        m_akNormal = NULL;
        m_akColor = NULL;
        m_akTexture = NULL;
        m_akTexture1 = NULL;
        m_akTexture2 = NULL;
        m_akTexture3 = NULL;
        m_akTextureBump = NULL;
    }
}
//----------------------------------------------------------------------------
void Geometry::Reconstruct (int iVertexQuantity, Vector3f* akVertex,
    Vector3f* akNormal, ColorRGB* akColor, Vector2f* akTexture,
    Vector2f* akTexture1, Vector2f* akTexture2, Vector2f* akTexture3,
    Vector2f* akTextureBump)
{
    m_iVertexQuantity = iVertexQuantity;

    if ( m_akVertex != akVertex )
    {
        delete[] m_akVertex;
        m_akVertex = akVertex;
        if ( m_akVertex )
            UpdateModelBound();
    }

    if ( m_akNormal != akNormal )
    {
        delete[] m_akNormal;
        m_akNormal = akNormal;
    }

    if ( m_akColor != akColor )
    {
        delete[] m_akColor;
        m_akColor = akColor;
    }

    if ( m_akTexture != akTexture )
    {
        delete[] m_akTexture;
        m_akTexture = akTexture;
    }

    if ( m_akTexture1 != akTexture1 )
    {
        delete[] m_akTexture1;
        m_akTexture1 = akTexture1;
    }

    if ( m_akTexture2 != akTexture2 )
    {
        delete[] m_akTexture2;
        m_akTexture2 = akTexture2;
    }

    if ( m_akTexture3 != akTexture3 )
    {
        delete[] m_akTexture3;
        m_akTexture3 = akTexture3;
    }

    if ( m_akTextureBump != akTextureBump )
    {
        delete[] m_akTextureBump;
        m_akTextureBump = akTextureBump;
    }
}
//----------------------------------------------------------------------------
void Geometry::UpdateModelBound ()
{
    m_kBound.ComputeFromData(m_iVertexQuantity,m_akVertex);
}
//----------------------------------------------------------------------------
void Geometry::UpdateWorldBound ()
{
    m_kWorldBound = m_kBound.TransformBy(m_kWorldRotate,m_kWorldTranslate,
        m_fWorldScale);
}
//----------------------------------------------------------------------------
void Geometry::UpdateModelNormals ()
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
void Geometry::UpdateRenderState (RenderState::Stack* pkStack)
{
    pkStack->CopyTo(m_aspkState);
}
//----------------------------------------------------------------------------
void Geometry::Draw (Renderer& rkRenderer)
{
    // The initial render state array has NULL pointers.  These are filled
    // in by a call to UpdateRS() at the root node N of a subtree that
    // contains the geometry object.  All states pointers are non-NULL as a
    // result of this call.  Any topological change in the subtree of N
    // requires you to call UpdateRS().
    //
    // If you reach this assert, you have not called UpdateRS() as you should
    // have.  And just in case you are doing this in Release mode, the
    // function returns to avoid a crash in the renderer.
    assert( m_aspkState[0] );
    if ( !m_aspkState[0] )
        return;

    // Due to DX9 and shader considerations, shaders need to be set before
    // other state (which may be affected or need to be nullified by the
    // presence of shaders).
    rkRenderer.SetShaderState(this);
    rkRenderer.SetState(m_aspkState);
}
//----------------------------------------------------------------------------
Geometry::PickRecord::PickRecord (Geometry* pkObject, float fRayT)
    :
    Spatial::PickRecord(pkObject,fRayT)
{
    // stub for derived classes
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* Geometry::Factory (Stream&)
{
    // Geometry is abstract, Factory never called
    return NULL;
}
//----------------------------------------------------------------------------
void Geometry::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Spatial::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_iVertexQuantity);
    m_akVertex = new Vector3f[m_iVertexQuantity];
    StreamRead(rkStream,m_akVertex,m_iVertexQuantity);

    StreamRead(rkStream,m_akNormal);
    if ( m_akNormal )
    {
        m_akNormal = new Vector3f[m_iVertexQuantity];
        StreamRead(rkStream,m_akNormal,m_iVertexQuantity);
    }

    StreamRead(rkStream,m_akColor);
    if ( m_akColor )
    {
        m_akColor = new ColorRGB[m_iVertexQuantity];
        StreamRead(rkStream,m_akColor,m_iVertexQuantity);
    }

    StreamRead(rkStream,m_akTexture);
    if ( m_akTexture )
    {
        m_akTexture = new Vector2f[m_iVertexQuantity];
        StreamRead(rkStream,m_akTexture,m_iVertexQuantity);
    }

    if ( rkStream.GetVersion() >= Version(1,3) )
    {
        StreamRead(rkStream,m_akTexture1);
        if ( m_akTexture1 )
        {
            m_akTexture1 = new Vector2f[m_iVertexQuantity];
            StreamRead(rkStream,m_akTexture1,m_iVertexQuantity);
        }

        StreamRead(rkStream,m_akTexture2);
        if ( m_akTexture2 )
        {
            m_akTexture2 = new Vector2f[m_iVertexQuantity];
            StreamRead(rkStream,m_akTexture2,m_iVertexQuantity);
        }

        StreamRead(rkStream,m_akTexture3);
        if ( m_akTexture3 )
        {
            m_akTexture3 = new Vector2f[m_iVertexQuantity];
            StreamRead(rkStream,m_akTexture3,m_iVertexQuantity);
        }

        StreamRead(rkStream,m_akTextureBump);
        if ( m_akTextureBump )
        {
            m_akTextureBump = new Vector2f[m_iVertexQuantity];
            StreamRead(rkStream,m_akTextureBump,m_iVertexQuantity);
        }
    }

    StreamRead(rkStream,m_kBound);
}
//----------------------------------------------------------------------------
void Geometry::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Spatial::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool Geometry::Register (Stream& rkStream)
{
    return Spatial::Register(rkStream);

    // render state array is derived, no need to register
}
//----------------------------------------------------------------------------
void Geometry::Save (Stream& rkStream)
{
    Spatial::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_iVertexQuantity);
    StreamWrite(rkStream,m_akVertex,m_iVertexQuantity);

    StreamWrite(rkStream,m_akNormal);
    if ( m_akNormal )
        StreamWrite(rkStream,m_akNormal,m_iVertexQuantity);

    StreamWrite(rkStream,m_akColor);
    if ( m_akColor )
        StreamWrite(rkStream,m_akColor,m_iVertexQuantity);

    StreamWrite(rkStream,m_akTexture);
    if ( m_akTexture )
        StreamWrite(rkStream,m_akTexture,m_iVertexQuantity);

    StreamWrite(rkStream,m_akTexture1);
    if ( m_akTexture1 )
        StreamWrite(rkStream,m_akTexture1,m_iVertexQuantity);

    StreamWrite(rkStream,m_akTexture2);
    if ( m_akTexture2 )
        StreamWrite(rkStream,m_akTexture2,m_iVertexQuantity);

    StreamWrite(rkStream,m_akTexture3);
    if ( m_akTexture3 )
        StreamWrite(rkStream,m_akTexture3,m_iVertexQuantity);

    StreamWrite(rkStream,m_akTextureBump);
    if ( m_akTextureBump )
        StreamWrite(rkStream,m_akTextureBump,m_iVertexQuantity);

    StreamWrite(rkStream,m_kBound);

    // render state array is derived, no need to save
}
//----------------------------------------------------------------------------
StringTree* Geometry::SaveStrings ()
{
    StringTree* pkTree = new StringTree(3,0,2,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("model bound =",m_kBound));
    pkTree->SetString(2,MakeString("vertex quantity =",m_iVertexQuantity));

    // children
    pkTree->SetChild(0,Spatial::SaveStrings());

    StringTree* pkAttrTree = new StringTree(m_iVertexQuantity+1,0,0,0);
    pkAttrTree->SetString(0,MakeString("vertices"));

    // TO DO.  The size of acString may not be large enough with all the new
    // texture coordinate arrays.  I put in asserts to catch this.
    char acString[512], acDummy[128];

    for (int i = 0; i < m_iVertexQuantity; i++)
    {
        acString[0] = 0;

        sprintf(acDummy,"(x: %f, y: %f, z: %f)",m_akVertex[i].X(),
            m_akVertex[i].Y(),m_akVertex[i].Z());
        strcat(acString,acDummy);

        if ( m_akNormal )
        {
            sprintf(acDummy,"(nx: %f, ny: %f, nz: %f)",m_akNormal[i].X(),
                m_akNormal[i].Y(),m_akNormal[i].Z());
            assert( strlen(acString)+strlen(acDummy)+1 <= 512 );
            strcat(acString,acDummy);
        }

        if ( m_akColor )
        {
            sprintf(acDummy,"(r: %f, g: %f, b: %f)",m_akColor[i].r,
                m_akColor[i].g,m_akColor[i].b);
            assert( strlen(acString)+strlen(acDummy)+1 <= 512 );
            strcat(acString,acDummy);
        }

        if ( m_akTexture )
        {
            sprintf(acDummy,"(tx: %f, ty: %f)",m_akTexture[i].X(),
                m_akTexture[i].Y());
            assert( strlen(acString)+strlen(acDummy)+1 <= 512 );
            strcat(acString,acDummy);
        }

        if ( m_akTexture1 )
        {
            sprintf(acDummy,"(tx: %f, ty: %f)",m_akTexture1[i].X(),
                m_akTexture1[i].Y());
            assert( strlen(acString)+strlen(acDummy)+1 <= 512 );
            strcat(acString,acDummy);
        }

        if ( m_akTexture2 )
        {
            sprintf(acDummy,"(tx: %f, ty: %f)",m_akTexture2[i].X(),
                m_akTexture2[i].Y());
            assert( strlen(acString)+strlen(acDummy)+1 <= 512 );
            strcat(acString,acDummy);
        }

        if ( m_akTexture3 )
        {
            sprintf(acDummy,"(tx: %f, ty: %f)",m_akTexture3[i].X(),
                m_akTexture3[i].Y());
            assert( strlen(acString)+strlen(acDummy)+1 <= 512 );
            strcat(acString,acDummy);
        }

        if ( m_akTextureBump )
        {
            sprintf(acDummy,"(tx: %f, ty: %f)",m_akTextureBump[i].X(),
                m_akTextureBump[i].Y());
            assert( strlen(acString)+strlen(acDummy)+1 <= 512 );
            strcat(acString,acDummy);
        }

        pkAttrTree->SetString(i+1,MakeString(acString));
    }

    pkTree->SetChild(1,pkAttrTree);

    return pkTree;
}
//----------------------------------------------------------------------------
int Geometry::GetMemoryUsed () const
{
    int iBaseSize = sizeof(Geometry) - sizeof(Spatial);
    int iDynaSize = m_iVertexQuantity*sizeof(m_akVertex[0]);

    if ( m_akNormal )
        iDynaSize += m_iVertexQuantity*sizeof(m_akNormal[0]);

    if ( m_akColor )
        iDynaSize += m_iVertexQuantity*sizeof(m_akColor[0]);

    if ( m_akTexture )
        iDynaSize += m_iVertexQuantity*sizeof(m_akTexture[0]);

    if ( m_akTexture1 )
        iDynaSize += m_iVertexQuantity*sizeof(m_akTexture1[0]);

    if ( m_akTexture2 )
        iDynaSize += m_iVertexQuantity*sizeof(m_akTexture2[0]);

    if ( m_akTexture3 )
        iDynaSize += m_iVertexQuantity*sizeof(m_akTexture3[0]);

    if ( m_akTextureBump )
        iDynaSize += m_iVertexQuantity*sizeof(m_akTextureBump[0]);

    int iTotalSize = iBaseSize + iDynaSize + Spatial::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int Geometry::GetDiskUsed () const
{
    int iSize = Spatial::GetDiskUsed() +
        sizeof(m_iVertexQuantity) +
        m_iVertexQuantity*sizeof(m_akVertex[0]);

    iSize += sizeof(m_akNormal);
    if ( m_akNormal )
        iSize += m_iVertexQuantity*sizeof(m_akNormal[0]);

    iSize += sizeof(m_akColor);
    if ( m_akColor )
        iSize += m_iVertexQuantity*sizeof(m_akColor[0]);

    iSize += sizeof(m_akTexture);
    if ( m_akTexture )
        iSize += m_iVertexQuantity*sizeof(m_akTexture[0]);

    iSize += sizeof(m_akTexture1);
    if ( m_akTexture1 )
        iSize += m_iVertexQuantity*sizeof(m_akTexture1[0]);

    iSize += sizeof(m_akTexture2);
    if ( m_akTexture2 )
        iSize += m_iVertexQuantity*sizeof(m_akTexture2[0]);

    iSize += sizeof(m_akTexture3);
    if ( m_akTexture3 )
        iSize += m_iVertexQuantity*sizeof(m_akTexture3[0]);

    iSize += sizeof(m_akTextureBump);
    if ( m_akTextureBump )
        iSize += m_iVertexQuantity*sizeof(m_akTextureBump[0]);

    iSize += sizeof(m_kBound);

    return iSize;
}
//----------------------------------------------------------------------------
