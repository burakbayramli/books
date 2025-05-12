// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "SphereTessellation.h"

Application2::Color SphereTessellation::ms_akColor[4] =
{
    Color(0,0,0),
    Color(255,0,0),
    Color(0,255,0),
    Color(0,0,255)
};

const int g_iSize = 512;
SphereTessellation g_kTheApp;

//----------------------------------------------------------------------------
SphereTessellation::SphereTessellation ()
    :
    Application2("SphereTessellation",0,0,g_iSize,g_iSize,
        ColorRGB(1.0f,1.0f,1.0f))
{
    m_bOctahedron = true;
    m_bPerspective = true;
}
//----------------------------------------------------------------------------
SphereTessellation::~SphereTessellation ()
{
}
//----------------------------------------------------------------------------
bool SphereTessellation::OnInitialize ()
{
    if ( !Application2::OnInitialize() )
        return false;

    // number of subdivisions of inscribed polyhedron
    m_iSteps = 0;

    // create and subdivide the polyhedron
    CreatePolyhedron(false);

    // initial camera orientation and eye point
    m_kRot.FromAxisAngle(Vector3f::UNIT_Z,0.0f);
    m_kEye = 2.0f*m_kRot.GetColumn(2);

    OnDisplay();
    return true;
}
//----------------------------------------------------------------------------
void SphereTessellation::OnTerminate ()
{
    QuadricSurfacef::DeletePolyhedron(m_kPoly);
    Application2::OnTerminate();
}
//----------------------------------------------------------------------------
void SphereTessellation::OnDisplay ()
{
    ClearScreen();
    DrawPolyhedron();
    Application2::OnDisplay();
}
//----------------------------------------------------------------------------
void SphereTessellation::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    switch ( ucKey )
    {
    // increase subdivision level
    case '+':
    case '=':
        m_iSteps++;
        CreatePolyhedron(true);
        OnDisplay();
        break;

    // decrease subdivision level
    case '-':
    case '_':
        if ( m_iSteps >= 1 )
        {
            m_iSteps--;
            CreatePolyhedron(true);
            OnDisplay();
        }
        break;

    // toggle type of polyhedron
    case 'p':
    case 'P':
        m_bOctahedron = !m_bOctahedron;
        CreatePolyhedron(true);
        OnDisplay();
        break;

    // toggle type of camera
    case 'c':
    case 'C':
        m_bPerspective = !m_bPerspective;
        OnDisplay();
        break;
    }
}
//----------------------------------------------------------------------------
void SphereTessellation::OnSpecialKeyDown (int iKey, int, int)
{
    Matrix3f kIncr;

    if ( iKey == KEY_LEFT_ARROW )
    {
        // rotate camera about its 'up' vector
        kIncr.FromAxisAngle(m_kRot.GetColumn(1),0.1f);
        m_kRot = kIncr*m_kRot;
        m_kEye = 2.0f*m_kRot.GetColumn(2);
        OnDisplay();
    }
    else if ( iKey == KEY_RIGHT_ARROW )
    {
        // rotate camera about its 'up' vector
        kIncr.FromAxisAngle(m_kRot.GetColumn(1),-0.1f);
        m_kRot = kIncr*m_kRot;
        m_kEye = 2.0f*m_kRot.GetColumn(2);
        OnDisplay();
    }
    else if ( iKey == KEY_UP_ARROW )
    {
        // rotate camera about its 'right' vector
        kIncr.FromAxisAngle(m_kRot.GetColumn(0),-0.1f);
        m_kRot = kIncr*m_kRot;
        m_kEye = 2.0f*m_kRot.GetColumn(2);
        OnDisplay();
    }
    else if ( iKey == KEY_DOWN_ARROW )
    {
        // rotate camera about its 'right' vector
        kIncr.FromAxisAngle(m_kRot.GetColumn(0),0.1f);
        m_kRot = kIncr*m_kRot;
        m_kEye = 2.0f*m_kRot.GetColumn(2);
        OnDisplay();
    }
}
//----------------------------------------------------------------------------
void SphereTessellation::CreatePolyhedron (bool bDeleteOld)
{
    if ( bDeleteOld )
        QuadricSurfacef::DeletePolyhedron(m_kPoly);

    if ( m_bOctahedron )
        CreateOctahedron(m_kPoly);
    else
        CreateTetrahedron(m_kPoly);

    QuadricSurfacef::TessellateSphere(m_iSteps,m_kPoly);
}
//----------------------------------------------------------------------------
void SphereTessellation::CreateTetrahedron (
    QuadricSurfacef::ConvexPolyhedron& rkTetra)
{
    rkTetra.m_iNumVertices = 4;
    rkTetra.m_apkVertex = new QuadricSurfacef::Vertex[4];
    rkTetra.m_iNumEdges = 6;
    rkTetra.m_apkEdge = new QuadricSurfacef::Edge[6];
    rkTetra.m_iNumTriangles = 4;
    rkTetra.m_apkTriangle = new QuadricSurfacef::Triangle[4];

    // vertices
    rkTetra.m_apkVertex[0].m_pkPoint = new Vector3f;
    rkTetra.m_apkVertex[0].m_pkPoint->X() = 1.0f;
    rkTetra.m_apkVertex[0].m_pkPoint->Y() = 0.0f;
    rkTetra.m_apkVertex[0].m_pkPoint->Z() = 0.0f;
    rkTetra.m_apkVertex[0].m_iNumEdges = 3;
    rkTetra.m_apkVertex[0].m_apkEdge = new QuadricSurfacef::Edge*[3];
    rkTetra.m_apkVertex[0].m_apkEdge[0] = &rkTetra.m_apkEdge[0];
    rkTetra.m_apkVertex[0].m_apkEdge[1] = &rkTetra.m_apkEdge[2];
    rkTetra.m_apkVertex[0].m_apkEdge[2] = &rkTetra.m_apkEdge[3];

    rkTetra.m_apkVertex[1].m_pkPoint = new Vector3f;
    rkTetra.m_apkVertex[1].m_pkPoint->X() = 0.0f;
    rkTetra.m_apkVertex[1].m_pkPoint->Y() = 1.0f;
    rkTetra.m_apkVertex[1].m_pkPoint->Z() = 0.0f;
    rkTetra.m_apkVertex[1].m_iNumEdges = 3;
    rkTetra.m_apkVertex[1].m_apkEdge = new QuadricSurfacef::Edge*[3];
    rkTetra.m_apkVertex[1].m_apkEdge[0] = &rkTetra.m_apkEdge[0];
    rkTetra.m_apkVertex[1].m_apkEdge[1] = &rkTetra.m_apkEdge[1];
    rkTetra.m_apkVertex[1].m_apkEdge[2] = &rkTetra.m_apkEdge[4];

    rkTetra.m_apkVertex[2].m_pkPoint = new Vector3f;
    rkTetra.m_apkVertex[2].m_pkPoint->X() = 0.0f;
    rkTetra.m_apkVertex[2].m_pkPoint->Y() = 0.0f;
    rkTetra.m_apkVertex[2].m_pkPoint->Z() = 1.0f;
    rkTetra.m_apkVertex[2].m_iNumEdges = 3;
    rkTetra.m_apkVertex[2].m_apkEdge = new QuadricSurfacef::Edge*[3];
    rkTetra.m_apkVertex[2].m_apkEdge[0] = &rkTetra.m_apkEdge[1];
    rkTetra.m_apkVertex[2].m_apkEdge[1] = &rkTetra.m_apkEdge[2];
    rkTetra.m_apkVertex[2].m_apkEdge[2] = &rkTetra.m_apkEdge[5];

    rkTetra.m_apkVertex[3].m_pkPoint = new Vector3f;
    rkTetra.m_apkVertex[3].m_pkPoint->X() = Mathf::Sqrt(1.0f/3.0f);
    rkTetra.m_apkVertex[3].m_pkPoint->Y() = Mathf::Sqrt(1.0f/3.0f);
    rkTetra.m_apkVertex[3].m_pkPoint->Z() = Mathf::Sqrt(1.0f/3.0f);
    rkTetra.m_apkVertex[3].m_iNumEdges = 3;
    rkTetra.m_apkVertex[3].m_apkEdge = new QuadricSurfacef::Edge*[3];
    rkTetra.m_apkVertex[3].m_apkEdge[0] = &rkTetra.m_apkEdge[3];
    rkTetra.m_apkVertex[3].m_apkEdge[1] = &rkTetra.m_apkEdge[4];
    rkTetra.m_apkVertex[3].m_apkEdge[2] = &rkTetra.m_apkEdge[5];

    // edges
    rkTetra.m_apkEdge[0].m_apkVertex[0] = &rkTetra.m_apkVertex[0];
    rkTetra.m_apkEdge[0].m_apkVertex[1] = &rkTetra.m_apkVertex[1];
    rkTetra.m_apkEdge[0].m_apkTriangle[0] = &rkTetra.m_apkTriangle[0];
    rkTetra.m_apkEdge[0].m_apkTriangle[1] = &rkTetra.m_apkTriangle[1];

    rkTetra.m_apkEdge[1].m_apkVertex[0] = &rkTetra.m_apkVertex[1];
    rkTetra.m_apkEdge[1].m_apkVertex[1] = &rkTetra.m_apkVertex[2];
    rkTetra.m_apkEdge[1].m_apkTriangle[0] = &rkTetra.m_apkTriangle[0];
    rkTetra.m_apkEdge[1].m_apkTriangle[1] = &rkTetra.m_apkTriangle[2];

    rkTetra.m_apkEdge[2].m_apkVertex[0] = &rkTetra.m_apkVertex[2];
    rkTetra.m_apkEdge[2].m_apkVertex[1] = &rkTetra.m_apkVertex[0];
    rkTetra.m_apkEdge[2].m_apkTriangle[0] = &rkTetra.m_apkTriangle[0];
    rkTetra.m_apkEdge[2].m_apkTriangle[1] = &rkTetra.m_apkTriangle[3];

    rkTetra.m_apkEdge[3].m_apkVertex[0] = &rkTetra.m_apkVertex[0];
    rkTetra.m_apkEdge[3].m_apkVertex[1] = &rkTetra.m_apkVertex[3];
    rkTetra.m_apkEdge[3].m_apkTriangle[0] = &rkTetra.m_apkTriangle[1];
    rkTetra.m_apkEdge[3].m_apkTriangle[1] = &rkTetra.m_apkTriangle[3];

    rkTetra.m_apkEdge[4].m_apkVertex[0] = &rkTetra.m_apkVertex[1];
    rkTetra.m_apkEdge[4].m_apkVertex[1] = &rkTetra.m_apkVertex[3];
    rkTetra.m_apkEdge[4].m_apkTriangle[0] = &rkTetra.m_apkTriangle[1];
    rkTetra.m_apkEdge[4].m_apkTriangle[1] = &rkTetra.m_apkTriangle[2];

    rkTetra.m_apkEdge[5].m_apkVertex[0] = &rkTetra.m_apkVertex[2];
    rkTetra.m_apkEdge[5].m_apkVertex[1] = &rkTetra.m_apkVertex[3];
    rkTetra.m_apkEdge[5].m_apkTriangle[0] = &rkTetra.m_apkTriangle[2];
    rkTetra.m_apkEdge[5].m_apkTriangle[1] = &rkTetra.m_apkTriangle[3];

    // triangles
    rkTetra.m_apkTriangle[0].m_apkVertex[0] = &rkTetra.m_apkVertex[0];
    rkTetra.m_apkTriangle[0].m_apkVertex[1] = &rkTetra.m_apkVertex[2];
    rkTetra.m_apkTriangle[0].m_apkVertex[2] = &rkTetra.m_apkVertex[1];
    rkTetra.m_apkTriangle[0].m_apkEdge[0] = &rkTetra.m_apkEdge[2];
    rkTetra.m_apkTriangle[0].m_apkEdge[1] = &rkTetra.m_apkEdge[1];
    rkTetra.m_apkTriangle[0].m_apkEdge[2] = &rkTetra.m_apkEdge[0];
    rkTetra.m_apkTriangle[0].m_apkAdjacent[0] = &rkTetra.m_apkTriangle[3];
    rkTetra.m_apkTriangle[0].m_apkAdjacent[1] = &rkTetra.m_apkTriangle[2];
    rkTetra.m_apkTriangle[0].m_apkAdjacent[2] = &rkTetra.m_apkTriangle[1];

    rkTetra.m_apkTriangle[1].m_apkVertex[0] = &rkTetra.m_apkVertex[0];
    rkTetra.m_apkTriangle[1].m_apkVertex[1] = &rkTetra.m_apkVertex[1];
    rkTetra.m_apkTriangle[1].m_apkVertex[2] = &rkTetra.m_apkVertex[3];
    rkTetra.m_apkTriangle[1].m_apkEdge[0] = &rkTetra.m_apkEdge[0];
    rkTetra.m_apkTriangle[1].m_apkEdge[1] = &rkTetra.m_apkEdge[4];
    rkTetra.m_apkTriangle[1].m_apkEdge[2] = &rkTetra.m_apkEdge[3];
    rkTetra.m_apkTriangle[1].m_apkAdjacent[0] = &rkTetra.m_apkTriangle[0];
    rkTetra.m_apkTriangle[1].m_apkAdjacent[1] = &rkTetra.m_apkTriangle[2];
    rkTetra.m_apkTriangle[1].m_apkAdjacent[2] = &rkTetra.m_apkTriangle[3];

    rkTetra.m_apkTriangle[2].m_apkVertex[0] = &rkTetra.m_apkVertex[1];
    rkTetra.m_apkTriangle[2].m_apkVertex[1] = &rkTetra.m_apkVertex[2];
    rkTetra.m_apkTriangle[2].m_apkVertex[2] = &rkTetra.m_apkVertex[3];
    rkTetra.m_apkTriangle[2].m_apkEdge[0] = &rkTetra.m_apkEdge[1];
    rkTetra.m_apkTriangle[2].m_apkEdge[1] = &rkTetra.m_apkEdge[5];
    rkTetra.m_apkTriangle[2].m_apkEdge[2] = &rkTetra.m_apkEdge[4];
    rkTetra.m_apkTriangle[2].m_apkAdjacent[0] = &rkTetra.m_apkTriangle[0];
    rkTetra.m_apkTriangle[2].m_apkAdjacent[1] = &rkTetra.m_apkTriangle[3];
    rkTetra.m_apkTriangle[2].m_apkAdjacent[2] = &rkTetra.m_apkTriangle[1];

    rkTetra.m_apkTriangle[3].m_apkVertex[0] = &rkTetra.m_apkVertex[0];
    rkTetra.m_apkTriangle[3].m_apkVertex[1] = &rkTetra.m_apkVertex[3];
    rkTetra.m_apkTriangle[3].m_apkVertex[2] = &rkTetra.m_apkVertex[2];
    rkTetra.m_apkTriangle[3].m_apkEdge[0] = &rkTetra.m_apkEdge[3];
    rkTetra.m_apkTriangle[3].m_apkEdge[1] = &rkTetra.m_apkEdge[5];
    rkTetra.m_apkTriangle[3].m_apkEdge[2] = &rkTetra.m_apkEdge[2];
    rkTetra.m_apkTriangle[3].m_apkAdjacent[0] = &rkTetra.m_apkTriangle[1];
    rkTetra.m_apkTriangle[3].m_apkAdjacent[1] = &rkTetra.m_apkTriangle[2];
    rkTetra.m_apkTriangle[3].m_apkAdjacent[2] = &rkTetra.m_apkTriangle[0];

    // For testing purposes, but not necessary for the algorithm.  This
    // allows the display program to show the subdivision structure.
    for (int i = 0; i < 6; i++)
        rkTetra.m_apkEdge[i].m_iStep = 0;
}
//----------------------------------------------------------------------------
void SphereTessellation::CreateOctahedron (
    QuadricSurfacef::ConvexPolyhedron& rkOct)
{
    rkOct.m_iNumVertices = 6;
    rkOct.m_apkVertex = new QuadricSurfacef::Vertex[6];
    rkOct.m_iNumEdges = 12;
    rkOct.m_apkEdge = new QuadricSurfacef::Edge[12];
    rkOct.m_iNumTriangles = 8;
    rkOct.m_apkTriangle = new QuadricSurfacef::Triangle[8];

    // vertices
    rkOct.m_apkVertex[0].m_pkPoint = new Vector3f;
    rkOct.m_apkVertex[0].m_pkPoint->X() = 0.0f;
    rkOct.m_apkVertex[0].m_pkPoint->Y() = 0.0f;
    rkOct.m_apkVertex[0].m_pkPoint->Z() = 1.0f;
    rkOct.m_apkVertex[0].m_iNumEdges = 4;
    rkOct.m_apkVertex[0].m_apkEdge = new QuadricSurfacef::Edge*[4];
    rkOct.m_apkVertex[0].m_apkEdge[0] = &rkOct.m_apkEdge[0];
    rkOct.m_apkVertex[0].m_apkEdge[1] = &rkOct.m_apkEdge[1];
    rkOct.m_apkVertex[0].m_apkEdge[2] = &rkOct.m_apkEdge[2];
    rkOct.m_apkVertex[0].m_apkEdge[3] = &rkOct.m_apkEdge[3];

    rkOct.m_apkVertex[1].m_pkPoint = new Vector3f;
    rkOct.m_apkVertex[1].m_pkPoint->X() = 1.0f;
    rkOct.m_apkVertex[1].m_pkPoint->Y() = 0.0f;
    rkOct.m_apkVertex[1].m_pkPoint->Z() = 0.0f;
    rkOct.m_apkVertex[1].m_iNumEdges = 4;
    rkOct.m_apkVertex[1].m_apkEdge = new QuadricSurfacef::Edge*[4];
    rkOct.m_apkVertex[1].m_apkEdge[0] = &rkOct.m_apkEdge[0];
    rkOct.m_apkVertex[1].m_apkEdge[1] = &rkOct.m_apkEdge[4];
    rkOct.m_apkVertex[1].m_apkEdge[2] = &rkOct.m_apkEdge[7];
    rkOct.m_apkVertex[1].m_apkEdge[3] = &rkOct.m_apkEdge[8];

    rkOct.m_apkVertex[2].m_pkPoint = new Vector3f;
    rkOct.m_apkVertex[2].m_pkPoint->X() = 0.0f;
    rkOct.m_apkVertex[2].m_pkPoint->Y() = 1.0f;
    rkOct.m_apkVertex[2].m_pkPoint->Z() = 0.0f;
    rkOct.m_apkVertex[2].m_iNumEdges = 4;
    rkOct.m_apkVertex[2].m_apkEdge = new QuadricSurfacef::Edge*[4];
    rkOct.m_apkVertex[2].m_apkEdge[0] = &rkOct.m_apkEdge[1];
    rkOct.m_apkVertex[2].m_apkEdge[1] = &rkOct.m_apkEdge[4];
    rkOct.m_apkVertex[2].m_apkEdge[2] = &rkOct.m_apkEdge[5];
    rkOct.m_apkVertex[2].m_apkEdge[3] = &rkOct.m_apkEdge[9];

    rkOct.m_apkVertex[3].m_pkPoint = new Vector3f;
    rkOct.m_apkVertex[3].m_pkPoint->X() = -1.0f;
    rkOct.m_apkVertex[3].m_pkPoint->Y() = 0.0f;
    rkOct.m_apkVertex[3].m_pkPoint->Z() = 0.0f;
    rkOct.m_apkVertex[3].m_iNumEdges = 4;
    rkOct.m_apkVertex[3].m_apkEdge = new QuadricSurfacef::Edge*[4];
    rkOct.m_apkVertex[3].m_apkEdge[0] = &rkOct.m_apkEdge[2];
    rkOct.m_apkVertex[3].m_apkEdge[1] = &rkOct.m_apkEdge[5];
    rkOct.m_apkVertex[3].m_apkEdge[2] = &rkOct.m_apkEdge[6];
    rkOct.m_apkVertex[3].m_apkEdge[3] = &rkOct.m_apkEdge[10];

    rkOct.m_apkVertex[4].m_pkPoint = new Vector3f;
    rkOct.m_apkVertex[4].m_pkPoint->X() = 0.0f;
    rkOct.m_apkVertex[4].m_pkPoint->Y() = -1.0f;
    rkOct.m_apkVertex[4].m_pkPoint->Z() = 0.0f;
    rkOct.m_apkVertex[4].m_iNumEdges = 4;
    rkOct.m_apkVertex[4].m_apkEdge = new QuadricSurfacef::Edge*[4];
    rkOct.m_apkVertex[4].m_apkEdge[0] = &rkOct.m_apkEdge[3];
    rkOct.m_apkVertex[4].m_apkEdge[1] = &rkOct.m_apkEdge[6];
    rkOct.m_apkVertex[4].m_apkEdge[2] = &rkOct.m_apkEdge[7];
    rkOct.m_apkVertex[4].m_apkEdge[3] = &rkOct.m_apkEdge[11];

    rkOct.m_apkVertex[5].m_pkPoint = new Vector3f;
    rkOct.m_apkVertex[5].m_pkPoint->X() = 0.0f;
    rkOct.m_apkVertex[5].m_pkPoint->Y() = 0.0f;
    rkOct.m_apkVertex[5].m_pkPoint->Z() = -1.0f;
    rkOct.m_apkVertex[5].m_iNumEdges = 4;
    rkOct.m_apkVertex[5].m_apkEdge = new QuadricSurfacef::Edge*[4];
    rkOct.m_apkVertex[5].m_apkEdge[0] = &rkOct.m_apkEdge[8];
    rkOct.m_apkVertex[5].m_apkEdge[1] = &rkOct.m_apkEdge[9];
    rkOct.m_apkVertex[5].m_apkEdge[2] = &rkOct.m_apkEdge[10];
    rkOct.m_apkVertex[5].m_apkEdge[3] = &rkOct.m_apkEdge[11];

    // edges
    rkOct.m_apkEdge[0].m_apkVertex[0] = &rkOct.m_apkVertex[0];
    rkOct.m_apkEdge[0].m_apkVertex[1] = &rkOct.m_apkVertex[1];
    rkOct.m_apkEdge[0].m_apkTriangle[0] = &rkOct.m_apkTriangle[3];
    rkOct.m_apkEdge[0].m_apkTriangle[1] = &rkOct.m_apkTriangle[0];

    rkOct.m_apkEdge[1].m_apkVertex[0] = &rkOct.m_apkVertex[0];
    rkOct.m_apkEdge[1].m_apkVertex[1] = &rkOct.m_apkVertex[2];
    rkOct.m_apkEdge[1].m_apkTriangle[0] = &rkOct.m_apkTriangle[0];
    rkOct.m_apkEdge[1].m_apkTriangle[1] = &rkOct.m_apkTriangle[1];

    rkOct.m_apkEdge[2].m_apkVertex[0] = &rkOct.m_apkVertex[0];
    rkOct.m_apkEdge[2].m_apkVertex[1] = &rkOct.m_apkVertex[3];
    rkOct.m_apkEdge[2].m_apkTriangle[0] = &rkOct.m_apkTriangle[1];
    rkOct.m_apkEdge[2].m_apkTriangle[1] = &rkOct.m_apkTriangle[2];

    rkOct.m_apkEdge[3].m_apkVertex[0] = &rkOct.m_apkVertex[0];
    rkOct.m_apkEdge[3].m_apkVertex[1] = &rkOct.m_apkVertex[4];
    rkOct.m_apkEdge[3].m_apkTriangle[0] = &rkOct.m_apkTriangle[2];
    rkOct.m_apkEdge[3].m_apkTriangle[1] = &rkOct.m_apkTriangle[3];

    rkOct.m_apkEdge[4].m_apkVertex[0] = &rkOct.m_apkVertex[1];
    rkOct.m_apkEdge[4].m_apkVertex[1] = &rkOct.m_apkVertex[2];
    rkOct.m_apkEdge[4].m_apkTriangle[0] = &rkOct.m_apkTriangle[0];
    rkOct.m_apkEdge[4].m_apkTriangle[1] = &rkOct.m_apkTriangle[4];

    rkOct.m_apkEdge[5].m_apkVertex[0] = &rkOct.m_apkVertex[2];
    rkOct.m_apkEdge[5].m_apkVertex[1] = &rkOct.m_apkVertex[3];
    rkOct.m_apkEdge[5].m_apkTriangle[0] = &rkOct.m_apkTriangle[1];
    rkOct.m_apkEdge[5].m_apkTriangle[1] = &rkOct.m_apkTriangle[5];

    rkOct.m_apkEdge[6].m_apkVertex[0] = &rkOct.m_apkVertex[3];
    rkOct.m_apkEdge[6].m_apkVertex[1] = &rkOct.m_apkVertex[4];
    rkOct.m_apkEdge[6].m_apkTriangle[0] = &rkOct.m_apkTriangle[2];
    rkOct.m_apkEdge[6].m_apkTriangle[1] = &rkOct.m_apkTriangle[6];

    rkOct.m_apkEdge[7].m_apkVertex[0] = &rkOct.m_apkVertex[4];
    rkOct.m_apkEdge[7].m_apkVertex[1] = &rkOct.m_apkVertex[1];
    rkOct.m_apkEdge[7].m_apkTriangle[0] = &rkOct.m_apkTriangle[3];
    rkOct.m_apkEdge[7].m_apkTriangle[1] = &rkOct.m_apkTriangle[7];

    rkOct.m_apkEdge[8].m_apkVertex[0] = &rkOct.m_apkVertex[1];
    rkOct.m_apkEdge[8].m_apkVertex[1] = &rkOct.m_apkVertex[5];
    rkOct.m_apkEdge[8].m_apkTriangle[0] = &rkOct.m_apkTriangle[7];
    rkOct.m_apkEdge[8].m_apkTriangle[1] = &rkOct.m_apkTriangle[4];

    rkOct.m_apkEdge[9].m_apkVertex[0] = &rkOct.m_apkVertex[2];
    rkOct.m_apkEdge[9].m_apkVertex[1] = &rkOct.m_apkVertex[5];
    rkOct.m_apkEdge[9].m_apkTriangle[0] = &rkOct.m_apkTriangle[4];
    rkOct.m_apkEdge[9].m_apkTriangle[1] = &rkOct.m_apkTriangle[5];

    rkOct.m_apkEdge[10].m_apkVertex[0] = &rkOct.m_apkVertex[3];
    rkOct.m_apkEdge[10].m_apkVertex[1] = &rkOct.m_apkVertex[5];
    rkOct.m_apkEdge[10].m_apkTriangle[0] = &rkOct.m_apkTriangle[5];
    rkOct.m_apkEdge[10].m_apkTriangle[1] = &rkOct.m_apkTriangle[6];

    rkOct.m_apkEdge[11].m_apkVertex[0] = &rkOct.m_apkVertex[4];
    rkOct.m_apkEdge[11].m_apkVertex[1] = &rkOct.m_apkVertex[5];
    rkOct.m_apkEdge[11].m_apkTriangle[0] = &rkOct.m_apkTriangle[6];
    rkOct.m_apkEdge[11].m_apkTriangle[1] = &rkOct.m_apkTriangle[7];

    // triangles
    rkOct.m_apkTriangle[0].m_apkVertex[0] = &rkOct.m_apkVertex[0];
    rkOct.m_apkTriangle[0].m_apkVertex[1] = &rkOct.m_apkVertex[1];
    rkOct.m_apkTriangle[0].m_apkVertex[2] = &rkOct.m_apkVertex[2];
    rkOct.m_apkTriangle[0].m_apkEdge[0] = &rkOct.m_apkEdge[0];
    rkOct.m_apkTriangle[0].m_apkEdge[1] = &rkOct.m_apkEdge[4];
    rkOct.m_apkTriangle[0].m_apkEdge[2] = &rkOct.m_apkEdge[1];
    rkOct.m_apkTriangle[0].m_apkAdjacent[0] = &rkOct.m_apkTriangle[3];
    rkOct.m_apkTriangle[0].m_apkAdjacent[1] = &rkOct.m_apkTriangle[4];
    rkOct.m_apkTriangle[0].m_apkAdjacent[2] = &rkOct.m_apkTriangle[1];

    rkOct.m_apkTriangle[1].m_apkVertex[0] = &rkOct.m_apkVertex[0];
    rkOct.m_apkTriangle[1].m_apkVertex[1] = &rkOct.m_apkVertex[2];
    rkOct.m_apkTriangle[1].m_apkVertex[2] = &rkOct.m_apkVertex[3];
    rkOct.m_apkTriangle[1].m_apkEdge[0] = &rkOct.m_apkEdge[1];
    rkOct.m_apkTriangle[1].m_apkEdge[1] = &rkOct.m_apkEdge[5];
    rkOct.m_apkTriangle[1].m_apkEdge[2] = &rkOct.m_apkEdge[2];
    rkOct.m_apkTriangle[1].m_apkAdjacent[0] = &rkOct.m_apkTriangle[0];
    rkOct.m_apkTriangle[1].m_apkAdjacent[1] = &rkOct.m_apkTriangle[5];
    rkOct.m_apkTriangle[1].m_apkAdjacent[2] = &rkOct.m_apkTriangle[2];

    rkOct.m_apkTriangle[2].m_apkVertex[0] = &rkOct.m_apkVertex[0];
    rkOct.m_apkTriangle[2].m_apkVertex[1] = &rkOct.m_apkVertex[3];
    rkOct.m_apkTriangle[2].m_apkVertex[2] = &rkOct.m_apkVertex[4];
    rkOct.m_apkTriangle[2].m_apkEdge[0] = &rkOct.m_apkEdge[2];
    rkOct.m_apkTriangle[2].m_apkEdge[1] = &rkOct.m_apkEdge[6];
    rkOct.m_apkTriangle[2].m_apkEdge[2] = &rkOct.m_apkEdge[3];
    rkOct.m_apkTriangle[2].m_apkAdjacent[0] = &rkOct.m_apkTriangle[1];
    rkOct.m_apkTriangle[2].m_apkAdjacent[1] = &rkOct.m_apkTriangle[6];
    rkOct.m_apkTriangle[2].m_apkAdjacent[2] = &rkOct.m_apkTriangle[3];

    rkOct.m_apkTriangle[3].m_apkVertex[0] = &rkOct.m_apkVertex[0];
    rkOct.m_apkTriangle[3].m_apkVertex[1] = &rkOct.m_apkVertex[4];
    rkOct.m_apkTriangle[3].m_apkVertex[2] = &rkOct.m_apkVertex[1];
    rkOct.m_apkTriangle[3].m_apkEdge[0] = &rkOct.m_apkEdge[3];
    rkOct.m_apkTriangle[3].m_apkEdge[1] = &rkOct.m_apkEdge[7];
    rkOct.m_apkTriangle[3].m_apkEdge[2] = &rkOct.m_apkEdge[0];
    rkOct.m_apkTriangle[3].m_apkAdjacent[0] = &rkOct.m_apkTriangle[2];
    rkOct.m_apkTriangle[3].m_apkAdjacent[1] = &rkOct.m_apkTriangle[7];
    rkOct.m_apkTriangle[3].m_apkAdjacent[2] = &rkOct.m_apkTriangle[0];

    rkOct.m_apkTriangle[4].m_apkVertex[0] = &rkOct.m_apkVertex[5];
    rkOct.m_apkTriangle[4].m_apkVertex[1] = &rkOct.m_apkVertex[2];
    rkOct.m_apkTriangle[4].m_apkVertex[2] = &rkOct.m_apkVertex[1];
    rkOct.m_apkTriangle[4].m_apkEdge[0] = &rkOct.m_apkEdge[9];
    rkOct.m_apkTriangle[4].m_apkEdge[1] = &rkOct.m_apkEdge[4];
    rkOct.m_apkTriangle[4].m_apkEdge[2] = &rkOct.m_apkEdge[8];
    rkOct.m_apkTriangle[4].m_apkAdjacent[0] = &rkOct.m_apkTriangle[5];
    rkOct.m_apkTriangle[4].m_apkAdjacent[1] = &rkOct.m_apkTriangle[0];
    rkOct.m_apkTriangle[4].m_apkAdjacent[2] = &rkOct.m_apkTriangle[7];

    rkOct.m_apkTriangle[5].m_apkVertex[0] = &rkOct.m_apkVertex[5];
    rkOct.m_apkTriangle[5].m_apkVertex[1] = &rkOct.m_apkVertex[3];
    rkOct.m_apkTriangle[5].m_apkVertex[2] = &rkOct.m_apkVertex[2];
    rkOct.m_apkTriangle[5].m_apkEdge[0] = &rkOct.m_apkEdge[10];
    rkOct.m_apkTriangle[5].m_apkEdge[1] = &rkOct.m_apkEdge[5];
    rkOct.m_apkTriangle[5].m_apkEdge[2] = &rkOct.m_apkEdge[9];
    rkOct.m_apkTriangle[5].m_apkAdjacent[0] = &rkOct.m_apkTriangle[6];
    rkOct.m_apkTriangle[5].m_apkAdjacent[1] = &rkOct.m_apkTriangle[1];
    rkOct.m_apkTriangle[5].m_apkAdjacent[2] = &rkOct.m_apkTriangle[4];

    rkOct.m_apkTriangle[6].m_apkVertex[0] = &rkOct.m_apkVertex[5];
    rkOct.m_apkTriangle[6].m_apkVertex[1] = &rkOct.m_apkVertex[4];
    rkOct.m_apkTriangle[6].m_apkVertex[2] = &rkOct.m_apkVertex[3];
    rkOct.m_apkTriangle[6].m_apkEdge[0] = &rkOct.m_apkEdge[11];
    rkOct.m_apkTriangle[6].m_apkEdge[1] = &rkOct.m_apkEdge[6];
    rkOct.m_apkTriangle[6].m_apkEdge[2] = &rkOct.m_apkEdge[10];
    rkOct.m_apkTriangle[6].m_apkAdjacent[0] = &rkOct.m_apkTriangle[7];
    rkOct.m_apkTriangle[6].m_apkAdjacent[1] = &rkOct.m_apkTriangle[2];
    rkOct.m_apkTriangle[6].m_apkAdjacent[2] = &rkOct.m_apkTriangle[5];

    rkOct.m_apkTriangle[7].m_apkVertex[0] = &rkOct.m_apkVertex[5];
    rkOct.m_apkTriangle[7].m_apkVertex[1] = &rkOct.m_apkVertex[1];
    rkOct.m_apkTriangle[7].m_apkVertex[2] = &rkOct.m_apkVertex[4];
    rkOct.m_apkTriangle[7].m_apkEdge[0] = &rkOct.m_apkEdge[8];
    rkOct.m_apkTriangle[7].m_apkEdge[1] = &rkOct.m_apkEdge[7];
    rkOct.m_apkTriangle[7].m_apkEdge[2] = &rkOct.m_apkEdge[11];
    rkOct.m_apkTriangle[7].m_apkAdjacent[0] = &rkOct.m_apkTriangle[4];
    rkOct.m_apkTriangle[7].m_apkAdjacent[1] = &rkOct.m_apkTriangle[3];
    rkOct.m_apkTriangle[7].m_apkAdjacent[2] = &rkOct.m_apkTriangle[6];

    // For testing purposes, but not necessary for the algorithm.  This
    // allows the display program to show the subdivision structure.
    for (int i = 0; i < 12; i++)
        rkOct.m_apkEdge[i].m_iStep = 0;
}
//----------------------------------------------------------------------------
void SphereTessellation::ProjectPoint (const Vector3f& rkPoint, int& riX,
    int& riY) const
{
    // projection plane is Dot(eye,(x,y,z)) = -4
    if ( m_bPerspective )
    {
        // perspective projection
        float fT = 8.0f/(4.0f-m_kEye.Dot(rkPoint));
        Vector3f kProj = (1.0f-fT)*m_kEye + fT*rkPoint;
        Vector3f kDir = kProj + m_kEye;
        Vector3f kProd = kDir*m_kRot;

        riX = g_iSize/4 + (int)((g_iSize/8)*(kProd.X() + 2.0f));
        riY = g_iSize/4 + (int)((g_iSize/8)*(kProd.Y() + 2.0f));
    }
    else
    {
        // parallel projection
        Vector3f kDir = rkPoint + m_kEye;
        Vector3f kProd = kDir*m_kRot;

        riX = (int)((g_iSize/4)*(kProd.X() + 2.0f));
        riY = (int)((g_iSize/4)*(kProd.Y() + 2.0f));
    }
}
//----------------------------------------------------------------------------
void SphereTessellation::DrawPolyhedron ()
{
    for (int i = 0; i < m_kPoly.m_iNumEdges; i++)
    {
        const Vector3f* pkP0 = m_kPoly.m_apkEdge[i].m_apkVertex[0]->m_pkPoint;
        int iX0, iY0;
        ProjectPoint(*pkP0,iX0,iY0);

        const Vector3f* pkP1 = m_kPoly.m_apkEdge[i].m_apkVertex[1]->m_pkPoint;
        int iX1, iY1;
        ProjectPoint(*pkP1,iX1,iY1);

        int iIndex = m_kPoly.m_apkEdge[i].m_iStep % 4;
        DrawLine(iX0,iY0,iX1,iY1,ms_akColor[iIndex]);
    }
}
//----------------------------------------------------------------------------
