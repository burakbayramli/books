//----------------------------------------------------------------------------------
// File:   Grid.cpp
// Author: Ignacio Llamas
// Email:  sdkfeedback@nvidia.com
// 
// Copyright (c) 2007 NVIDIA Corporation. All rights reserved.
//
// TO  THE MAXIMUM  EXTENT PERMITTED  BY APPLICABLE  LAW, THIS SOFTWARE  IS PROVIDED
// *AS IS*  AND NVIDIA AND  ITS SUPPLIERS DISCLAIM  ALL WARRANTIES,  EITHER  EXPRESS
// OR IMPLIED, INCLUDING, BUT NOT LIMITED  TO, IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL  NVIDIA OR ITS SUPPLIERS
// BE  LIABLE  FOR  ANY  SPECIAL,  INCIDENTAL,  INDIRECT,  OR  CONSEQUENTIAL DAMAGES
// WHATSOEVER (INCLUDING, WITHOUT LIMITATION,  DAMAGES FOR LOSS OF BUSINESS PROFITS,
// BUSINESS INTERRUPTION, LOSS OF BUSINESS INFORMATION, OR ANY OTHER PECUNIARY LOSS)
// ARISING OUT OF THE  USE OF OR INABILITY  TO USE THIS SOFTWARE, EVEN IF NVIDIA HAS
// BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
//
//
//----------------------------------------------------------------------------------

#include "Smoke.h"
#include "Grid.h"
#include <math.h>

struct VS_INPUT_FLUIDSIM_STRUCT
{   
    D3DXVECTOR3 Pos; // Clip space position for slice vertices
    D3DXVECTOR3 Tex; // Cell coordinates in 0-"texture dimension" range
};

Grid::Grid( ID3D10Device* pd3dDevice ) : m_pD3DDevice(NULL), layout(NULL), renderQuadBuffer(NULL),
    slicesBuffer(NULL), boundarySlicesBuffer(NULL), boundaryLinesBuffer(NULL), 
    numVerticesRenderQuad(0), numVerticesSlices(0), numVerticesBoundarySlices(0),
    numVerticesBoundaryLines(0), cols(0), rows(0)
{
    SAFE_ACQUIRE(m_pD3DDevice, pd3dDevice);
}

HRESULT Grid::Initialize( int gridWidth, int gridHeight, int gridDepth,ID3D10EffectTechnique* technique )
{
    HRESULT hr;

    dim[0] = gridWidth;
    dim[1] = gridHeight;
    dim[2] = gridDepth;

    maxDim = max( max( dim[0], dim[1] ), dim[2] );

    ComputeRowColsForFlat3DTexture(dim[2], &cols, &rows);

    V_RETURN(CreateVertexBuffers(technique));

    return S_OK;
}

Grid::~Grid()
{
    SAFE_RELEASE(layout);
    SAFE_RELEASE(renderQuadBuffer);
    SAFE_RELEASE(slicesBuffer);
    SAFE_RELEASE(boundarySlicesBuffer);
    SAFE_RELEASE(boundaryLinesBuffer);

    SAFE_RELEASE(m_pD3DDevice);    
}


HRESULT Grid::CreateVertexBuffers( ID3D10EffectTechnique* technique )
{
    HRESULT hr(S_OK);

    // Create layout
    D3D10_INPUT_ELEMENT_DESC layoutDesc[] = 
    {
        { "POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT,       0, 0, D3D10_INPUT_PER_VERTEX_DATA, 0 },
        { "TEXCOORD", 0, DXGI_FORMAT_R32G32B32_FLOAT,       0,12, D3D10_INPUT_PER_VERTEX_DATA, 0 }, 
    };
    UINT numElements = sizeof(layoutDesc)/sizeof(layoutDesc[0]);
    CreateLayout( layoutDesc, numElements, technique, &layout);

    int index(0);
    VS_INPUT_FLUIDSIM_STRUCT *renderQuad(NULL);
    VS_INPUT_FLUIDSIM_STRUCT *slices(NULL);
    VS_INPUT_FLUIDSIM_STRUCT *boundarySlices(NULL);
    VS_INPUT_FLUIDSIM_STRUCT *boundaryLines(NULL);

    try
    {
        #define VERTICES_PER_SLICE 6
        #define VERTICES_PER_LINE 2
        #define LINES_PER_SLICE 4

        numVerticesRenderQuad = VERTICES_PER_SLICE * dim[2];
        renderQuad = new VS_INPUT_FLUIDSIM_STRUCT[ numVerticesRenderQuad ];

        numVerticesSlices = VERTICES_PER_SLICE * (dim[2] - 2);
        slices = new VS_INPUT_FLUIDSIM_STRUCT[ numVerticesSlices ];

        numVerticesBoundarySlices = VERTICES_PER_SLICE * 2;
        boundarySlices = new VS_INPUT_FLUIDSIM_STRUCT[ numVerticesBoundarySlices ];

        numVerticesBoundaryLines = VERTICES_PER_LINE * LINES_PER_SLICE * (dim[2]);
        boundaryLines = new VS_INPUT_FLUIDSIM_STRUCT[ numVerticesBoundaryLines ];
    }
    catch(...)
    {
        hr = E_OUTOFMEMORY;
        goto cleanup;
    }
    
    assert(renderQuad && numVerticesSlices && numVerticesBoundarySlices && numVerticesBoundaryLines);

    // Vertex buffer for "dim[2]" quads to draw all the slices of the 3D-texture as a flat 3D-texture
    // (used to draw all the individual slices at once to the screen buffer)
    index = 0;
    for(int z=0; z<dim[2]; z++)
        InitScreenSlice(&renderQuad,z,index);
    V_RETURN(CreateVertexBuffer(sizeof(VS_INPUT_FLUIDSIM_STRUCT)*numVerticesRenderQuad,
        D3D10_BIND_VERTEX_BUFFER, &renderQuadBuffer, renderQuad, numVerticesRenderQuad));

    // Vertex buffer for "dim[2]" quads to draw all the slices to a 3D texture
    // (a Geometry Shader is used to send each quad to the appropriate slice)
    index = 0;
    for( int z = 1; z < dim[2]-1; z++ )
        InitSlice( z, &slices, index );
    assert(index==numVerticesSlices);
    V_RETURN(CreateVertexBuffer(sizeof(VS_INPUT_FLUIDSIM_STRUCT)*numVerticesSlices,
        D3D10_BIND_VERTEX_BUFFER, &slicesBuffer, slices , numVerticesSlices));

    // Vertex buffers for boundary geometry
    //   2 boundary slices
    index = 0;
    InitBoundaryQuads(&boundarySlices,index);
    assert(index==numVerticesBoundarySlices);
    V_RETURN(CreateVertexBuffer(sizeof(VS_INPUT_FLUIDSIM_STRUCT)*numVerticesBoundarySlices,
        D3D10_BIND_VERTEX_BUFFER, &boundarySlicesBuffer, boundarySlices, numVerticesBoundarySlices));
    //   ( 4 * "dim[2]" ) boundary lines
    index = 0;
    InitBoundaryLines(&boundaryLines,index);
    assert(index==numVerticesBoundaryLines);
    V_RETURN(CreateVertexBuffer(sizeof(VS_INPUT_FLUIDSIM_STRUCT)*numVerticesBoundaryLines,
        D3D10_BIND_VERTEX_BUFFER, &boundaryLinesBuffer, boundaryLines, numVerticesBoundaryLines));

cleanup:
    delete [] renderQuad;
    renderQuad = NULL;

    delete [] slices;
    slices = NULL;

    delete [] boundarySlices;
    boundarySlices = NULL;

    delete [] boundaryLines;
    boundaryLines = NULL;

    return hr;
}

void Grid::InitScreenSlice(VS_INPUT_FLUIDSIM_STRUCT** vertices, int z, int& index )
{
    VS_INPUT_FLUIDSIM_STRUCT tempVertex1;
    VS_INPUT_FLUIDSIM_STRUCT tempVertex2;
    VS_INPUT_FLUIDSIM_STRUCT tempVertex3;
    VS_INPUT_FLUIDSIM_STRUCT tempVertex4;

    // compute the offset (px, py) in the "flat 3D-texture" space for the slice with given 'z' coordinate
    int column      = z % cols;
    int row         = (int)floorf((float)(z/cols));
    int px = column * dim[0];
    int py = row    * dim[1];

    float w = float(dim[0]);
    float h = float(dim[1]);

    float Width  = float(cols * dim[0]);
    float Height = float(rows * dim[1]);

    tempVertex1.Pos   = D3DXVECTOR3( px*2.0f/Width-1.0f     , -(py*2.0f/Height) + 1.0f      , 0.0f      );
    tempVertex1.Tex   = D3DXVECTOR3( 0                      ,  0                            , float(z)  );

    tempVertex2.Pos   = D3DXVECTOR3( (px+w)*2.0f/Width -1.0f, -((py)*2.0f/Height) + 1.0f    , 0.0f      );
    tempVertex2.Tex   = D3DXVECTOR3( w                      ,  0                            , float(z)  );

    tempVertex3.Pos   = D3DXVECTOR3( (px+w)*2.0f/Width -1.0f, -((py+h)*2.0f/Height) + 1.0f  , 0.0f      );
    tempVertex3.Tex   = D3DXVECTOR3( w                      ,  h                            , float(z)  );

    tempVertex4.Pos   = D3DXVECTOR3( (px)*2.0f/Width -1.0f  , -((py+h)*2.0f/Height) + 1.0f  , 0.0f      );
    tempVertex4.Tex   = D3DXVECTOR3( 0                      ,  h                            , float(z)  );

    (*vertices)[index++] = tempVertex1;
    (*vertices)[index++] = tempVertex2;
    (*vertices)[index++] = tempVertex3;
    (*vertices)[index++] = tempVertex1;
    (*vertices)[index++] = tempVertex3;
    (*vertices)[index++] = tempVertex4;
}

void Grid::InitSlice( int z, VS_INPUT_FLUIDSIM_STRUCT** vertices, int& index )
{
    VS_INPUT_FLUIDSIM_STRUCT tempVertex1;
    VS_INPUT_FLUIDSIM_STRUCT tempVertex2;
    VS_INPUT_FLUIDSIM_STRUCT tempVertex3;
    VS_INPUT_FLUIDSIM_STRUCT tempVertex4;

    int w = dim[0];
    int h = dim[1];

    tempVertex1.Pos = D3DXVECTOR3( 1*2.0f/w - 1.0f      , -1*2.0f/h + 1.0f      , 0.0f      );
    tempVertex1.Tex = D3DXVECTOR3( 1.0f                 ,  1.0f                 , float(z)  );

    tempVertex2.Pos = D3DXVECTOR3( (w-1.0f)*2.0f/w-1.0f , -1*2.0f/h + 1.0f      , 0.0f      );
    tempVertex2.Tex = D3DXVECTOR3( (w-1.0f)             ,   1.0f                , float(z)  );

    tempVertex3.Pos = D3DXVECTOR3( (w-1.0f)*2.0f/w-1.0f , -(h-1)*2.0f/h+1.0f    , 0.0f      );
    tempVertex3.Tex = D3DXVECTOR3( (w-1.0f)             , (h-1.0f)              , float(z)  );

    tempVertex4.Pos = D3DXVECTOR3( 1*2.0f/w - 1.0f      , -(h-1.0f)*2.0f/h+1.0f , 0.0f      );
    tempVertex4.Tex = D3DXVECTOR3( 1.0f                 , (h-1.0f)              , float(z)  );


    (*vertices)[index++] = tempVertex1;
    (*vertices)[index++] = tempVertex2;
    (*vertices)[index++] = tempVertex3;
    (*vertices)[index++] = tempVertex1;
    (*vertices)[index++] = tempVertex3;
    (*vertices)[index++] = tempVertex4;

}

void Grid::InitLine( float x1, float y1, float x2, float y2, int z,
                    VS_INPUT_FLUIDSIM_STRUCT** vertices, int& index )
{
    VS_INPUT_FLUIDSIM_STRUCT tempVertex;
    int w = dim[0];
    int h = dim[1];

    tempVertex.Pos  = D3DXVECTOR3( x1*2.0f/w - 1.0f , -y1*2.0f/h + 1.0f , 0.5f      );
    tempVertex.Tex  = D3DXVECTOR3( 0.0f             , 0.0f              , float(z)  );
    (*vertices)[index++] = tempVertex;

    tempVertex.Pos  = D3DXVECTOR3( x2*2.0f/w - 1.0f , -y2*2.0f/h + 1.0f , 0.5f      );
    tempVertex.Tex  = D3DXVECTOR3( 0.0f             , 0.0f              , float(z)  );
    (*vertices)[index++] = tempVertex;
}


void Grid::InitBoundaryQuads( VS_INPUT_FLUIDSIM_STRUCT** vertices, int& index )
{
    InitSlice( 0, vertices, index );
    InitSlice( dim[2]-1, vertices, index );
}

void Grid::InitBoundaryLines( VS_INPUT_FLUIDSIM_STRUCT** vertices, int& index )
{
    int w = dim[0];
    int h = dim[1];

    for( int z = 0; z < dim[2]; z++ )
    {
        // bottom
        InitLine( 0.0f, 1.0f, float(w), 1.0f, z, vertices, index );
        // top
        InitLine( 0.0f, float(h), float(w), float(h), z, vertices, index );
        // left
        InitLine( 1.0f, 0.0f, 1.0f, float(h), z, vertices, index );
        //right
        InitLine( float(w), 0.0f, float(w), float(h), z, vertices, index );
    }
}

void Grid::DrawSlices( void )
{
    UINT stride[1] = { sizeof(VS_INPUT_FLUIDSIM_STRUCT) };
    UINT offset[1] = { 0 };
    DrawPrimitive( D3D10_PRIMITIVE_TOPOLOGY_TRIANGLELIST, layout, &slicesBuffer,
        stride, offset, 0, numVerticesSlices );
}

void Grid::DrawSlicesToScreen( void )
{
    UINT stride[1] = { sizeof(VS_INPUT_FLUIDSIM_STRUCT) };
    UINT offset[1] = { 0 };
    DrawPrimitive( D3D10_PRIMITIVE_TOPOLOGY_TRIANGLELIST, layout, &renderQuadBuffer,
        stride, offset, 0, numVerticesRenderQuad );

}

void Grid::DrawBoundaryQuads( void )
{
    UINT stride[1] = { sizeof(VS_INPUT_FLUIDSIM_STRUCT) };
    UINT offset[1] = { 0 };
    DrawPrimitive( D3D10_PRIMITIVE_TOPOLOGY_TRIANGLELIST, layout, &boundarySlicesBuffer,
        stride, offset, 0, numVerticesBoundarySlices );

}

void Grid::DrawBoundaryLines( void )
{

    UINT stride[1] = { sizeof(VS_INPUT_FLUIDSIM_STRUCT) };
    UINT offset[1] = { 0 };
    DrawPrimitive( D3D10_PRIMITIVE_TOPOLOGY_LINELIST, layout, &boundaryLinesBuffer, 
        stride, offset, 0, numVerticesBoundaryLines  );

}

void Grid::DrawPrimitive( D3D10_PRIMITIVE_TOPOLOGY PrimitiveType, ID3D10InputLayout* layout,
                         ID3D10Buffer** vertexBuffer,UINT* stride, UINT* offset, UINT StartVertex,
                         UINT VertexCount )
{
    m_pD3DDevice->IASetPrimitiveTopology( PrimitiveType );
    m_pD3DDevice->IASetInputLayout( layout );
    m_pD3DDevice->IASetVertexBuffers( 0, 1, vertexBuffer, stride, offset );
    m_pD3DDevice->Draw( VertexCount, StartVertex ); 
}

HRESULT Grid::CreateLayout( D3D10_INPUT_ELEMENT_DESC* layoutDesc, UINT numElements,
                           ID3D10EffectTechnique* technique, ID3D10InputLayout** layout)
{
    HRESULT hr;
    D3D10_PASS_DESC PassDesc;
    technique->GetPassByIndex( 0 )->GetDesc( &PassDesc );
    V_RETURN(m_pD3DDevice->CreateInputLayout( layoutDesc, numElements, 
        PassDesc.pIAInputSignature, PassDesc.IAInputSignatureSize, layout ));
    return S_OK;
}

HRESULT Grid::CreateVertexBuffer( int ByteWidth, UINT bindFlags, ID3D10Buffer** vertexBuffer,
                                 VS_INPUT_FLUIDSIM_STRUCT* vertices,int numVertices)
{
    HRESULT hr;

    D3D10_BUFFER_DESC bd;
    bd.ByteWidth = ByteWidth;
    bd.Usage = D3D10_USAGE_DEFAULT;
    bd.BindFlags = D3D10_BIND_VERTEX_BUFFER;
    bd.CPUAccessFlags = 0;
    bd.MiscFlags = 0;

    D3D10_SUBRESOURCE_DATA InitData;
    ZeroMemory( &InitData, sizeof(D3D10_SUBRESOURCE_DATA) );
    InitData.pSysMem = vertices;
    InitData.SysMemPitch = ByteWidth/numVertices;

    V_RETURN( m_pD3DDevice->CreateBuffer( &bd, &InitData,vertexBuffer  ) );

    return S_OK;
}
