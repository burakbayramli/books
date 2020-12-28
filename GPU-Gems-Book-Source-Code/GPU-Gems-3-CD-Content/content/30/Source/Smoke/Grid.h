//----------------------------------------------------------------------------------
// File:   Grid.h
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

#ifndef GRID_H
#define GRID_H

#include "Smoke.h"

struct VS_INPUT_FLUIDSIM_STRUCT;

class Grid
{
public:
    Grid( ID3D10Device* pd3dDevice );
    virtual ~Grid( void );

    HRESULT Initialize( int gridWidth, int gridHeight, int gridDepth,ID3D10EffectTechnique* technique );

    void DrawSlices        ( void );
    void DrawSlicesToScreen( void );
    void DrawBoundaryQuads ( void );
    void DrawBoundaryLines ( void );
    
    int  GetCols(){return cols;};
    int  GetRows(){return rows;};

    int dim[3];
    int maxDim;

protected:

    
    HRESULT CreateVertexBuffers  (ID3D10EffectTechnique* technique);
    void InitScreenSlice        (VS_INPUT_FLUIDSIM_STRUCT** vertices, int z, int& index);
    void InitSlice             (int z, VS_INPUT_FLUIDSIM_STRUCT** vertices, int& index);
    void InitLine              (float x1, float y1, float x2, float y2, int z,
                                    VS_INPUT_FLUIDSIM_STRUCT** vertices, int& index);
    void InitBoundaryQuads     (VS_INPUT_FLUIDSIM_STRUCT** vertices, int& index);
    void InitBoundaryLines     (VS_INPUT_FLUIDSIM_STRUCT** vertices, int& index);

    HRESULT CreateLayout( D3D10_INPUT_ELEMENT_DESC* layoutDesc, UINT numElements,ID3D10EffectTechnique* technique, ID3D10InputLayout** layout);
    HRESULT CreateVertexBuffer( int ByteWidth, UINT bindFlags, ID3D10Buffer** vertexBuffer,VS_INPUT_FLUIDSIM_STRUCT* vertices,int numVertices);

    // D3D10 helper functions
    void DrawPrimitive( D3D10_PRIMITIVE_TOPOLOGY PrimitiveType, ID3D10InputLayout* layout, ID3D10Buffer** vertexBuffer,UINT* stride, UINT* offset, UINT StartVertex, UINT VertexCount );

    // Internal State
    //===============
    ID3D10Device*              m_pD3DDevice;

    ID3D10InputLayout*         layout;
    ID3D10Buffer*              renderQuadBuffer;
    ID3D10Buffer*              slicesBuffer;
    ID3D10Buffer*              boundarySlicesBuffer;
    ID3D10Buffer*              boundaryLinesBuffer;

    int numVerticesRenderQuad;
    int numVerticesSlices;
    int numVerticesBoundarySlices;
    int numVerticesBoundaryLines;
    int cols;
    int rows;
};

#endif

