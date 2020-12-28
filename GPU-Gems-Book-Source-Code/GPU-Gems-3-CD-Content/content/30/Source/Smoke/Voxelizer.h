//----------------------------------------------------------------------------------
// File:   Voxelizer.h
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

#ifndef _VOXELIZER_H_
#define _VOXELIZER_H_

#include "SkinnedMesh.h"

/*=============================================================================
  Arbitrary Mesh Voxelizer.
  Takes as input an arbitrary mesh and matrix transformation and generates a
   voxelized representation of the mesh in the destination texture, where voxels 
   outside the object have 0 value and voxels inside have non-zero value.
  
  The matrix transform is the object to voxel-volume transform, i.e. it is the
   object transform with respect to the coordinate system of the voxel-volume, 
   which has its origin at 0,0,0.

  Proposed Algorithm:

    StencilClipVolume: 
        per-slice stencil XOR or Non-Zero IN/OUT test with moving clip-plane.

     The object is drawn into each slice in the volume, setting an orthogonal
     projection such that the near plane matches the slice in question and the far
     plane is at infinity. The left, right, top and bottom clip planes are set to
     match the volume's side walls.

     A stencil buffer is used in each mesh drawing pass (the same buffer may be used
     for all slices).
     When drawing the mesh the stencil functions are set as follows:
      A) if using the XOR rule, we simply invert the value (STENCIL_OP_INVERT)
      B) if using the Non-Zero rule we increment (STENCIL_OP_INCR) for front faces 
      and decrement for back faces  (D3D10_STENCIL_OP_DECR).
      
      In both cases the end result is that non-zero values are found at those pixels 
      of the slice (voxels) that are inside the mesh.

     This works well only for  closed non-manifold (watertight) meshes.
     For non-closed meshes the results will vary depending on the object orientation 
     with respect to the volume.
     For example a noncapped cylinder, if aligned such that its axis is 
     perpendicular to the slices will yield no voxels inside.
     Conversely, if the axis is parallel to the slices we would get voxels for the
     inside of the same cylinder if it was capped.

  ============================================================================= */


class Voxelizer
{
public:
    Voxelizer(void);
    virtual ~Voxelizer(void);
   
    HRESULT SetDestination(ID3D10Device *pD3DDevice, 
        ID3D10Texture3D *pDstInOutTexture3D, ID3D10Texture3D *pDstVelocityTexture3D = NULL);
    
    HRESULT Voxelize(D3DXMATRIX& objToVolumeXForm, SkinnedMesh *pMesh, 
        int srcSoBuf, int prevSrcSoBuf, float timeStep);

private:
    HRESULT Initialize(void);
    HRESULT InitTextures(void);
    HRESULT InitShaders(void);
    HRESULT InitSlices(void);
    void Cleanup(void);

    void DrawSlices(void);

    HRESULT DoVoxelization(void);
    HRESULT StencilClipVolume(void);
    HRESULT VoxelizeVelocity(void);
    HRESULT RenderClippedMesh(float zNear, float zFar, ID3D10EffectTechnique *pTechnique);

    // INTERNAL STATE
    
    // Destination state
    ID3D10Device                *m_pD3DDevice;
    ID3D10Texture3D             *m_pDstInOutTexture3D;
    ID3D10RenderTargetView      *m_pDstInOutTexRTView;
    ID3D10Texture3D             *m_pDstVelTexture3D;
    ID3D10RenderTargetView      *m_pDstVelTexRTView;

    UINT                        m_width;
    UINT                        m_height;
    UINT                        m_depth;

    //  for flat 3D texture
    int                         m_cols;
    int                         m_rows;
    
    // Source state
    SkinnedMesh                 *m_pSkinnedMesh;
    int                         m_srcSoBuffer;
    int                         m_prevSrcSoBuffer;
    float                       m_timeStep;

    D3DXMATRIX                  m_objToVolumeXForm;

    // Other state
    bool m_initialized;

    // The depth-stencil buffer
    ID3D10Texture2D             *m_pDSTex2D;
    ID3D10DepthStencilView      *m_pDSTex2DDSView;
    ID3D10ShaderResourceView    *m_pDSTex2DSRView;

    // Effect/shader state
    ID3D10Effect                *m_pVoxEffect;
    ID3D10EffectTechnique       *m_pNZTech;
    ID3D10EffectTechnique       *m_pResolveWithPSTech;
    ID3D10EffectTechnique       *m_pGenVelocityWireframeTech;

    ID3D10InputLayout           *m_pSOInputLayout;
    ID3D10EffectMatrixVariable  *m_pWorldViewProjectionVar;
    ID3D10EffectShaderResourceVariable *m_pDSTex2DSRVar;

    // Slices state
    ID3D10InputLayout           *m_pSlicesLayout;
    ID3D10Buffer                *m_pSlicesVB;
};

#endif // _VOXELIZER_H_