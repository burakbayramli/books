//----------------------------------------------------------------------------------
// File:   Voxelizer.fx
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

//--------------------------------------------------------------------------------------
// Textures
//--------------------------------------------------------------------------------------
Texture2D<uint2>      stencilbufferTex2D;

//--------------------------------------------------------------------------------------
// Variables
//--------------------------------------------------------------------------------------
float4x4    WorldViewProjection : WORLDVIEWPROJECTION;

float2 projSpacePixDim; // the dimensions of a pixel in projection space i.e. (2.0/rtWidth, 2.0/rtHeight)
float3 gridDim;
float recTimeStep;

int sliceIdx;       // index of the slice we want to output into
float sliceZ;       // z in the range [-0.5, 0.5] for the slice we are outputting into


//--------------------------------------------------------------------------------------
// Pipeline State definitions
//--------------------------------------------------------------------------------------

// RasterizerState 
RasterizerState RS_CullDisabled
{
  MultiSampleEnable = False;
  CullMode = None;
  ScissorEnable = true;
};


// BlendState
BlendState BS_NoBlending
{
  BlendEnable[0] = false;
  RenderTargetWriteMask[0] = 0x0F;
};


// DepthStencilState
DepthStencilState DSS_NonZeroRule
{
    DepthEnable = FALSE;
    DepthWriteMask = ZERO;
    
    //stencil
    StencilEnable = true;
    StencilReadMask = 0x00;
    StencilWriteMask = 0xFF;
    FrontFaceStencilFunc = Always;
    FrontFaceStencilPass = Decr;
    FrontFaceStencilFail = Keep;
    BackFaceStencilFunc = Always;
    BackFaceStencilPass = Incr;
    BackFaceStencilFail = Keep;
    
};

DepthStencilState DSS_Disabled
{
    DepthEnable = FALSE;
    DepthWriteMask = ZERO;
    
    //stencil
    StencilEnable = FALSE;
    StencilReadMask = 0x00;
    StencilWriteMask = 0x00;
};


//--------------------------------------------------------------------------------------
// Structs
//--------------------------------------------------------------------------------------

// For Technique VoxelizeNZ
struct VsVoxInput
{
    float3 Pos          : POSITION;
};

struct VsVoxOutput
{
    float4 Pos	        : SV_Position;
};


// For technique VoxelizeResolveWithPS
struct VsResInput
{
    float3 Pos          : POSITION;
    float3 Tex          : TEXCOORD;
};

struct VsResOutput
{
    float4 Pos          : POSITION;
    float3 Tex          : TEXCOORD;
};

struct GsResOutput
{
    float4 Pos          : SV_Position;
    float3 Tex          : TEXCOORD;
    uint   RTIndex      : SV_RenderTargetArrayIndex;
};


// For technique GenVelocityWireframe
struct VsGenVelInput
{
    float3 Pos          : POSITION0;
    float3 PrevPos      : POSITION1;
};

struct VsGenVelOutput
{
    float4 Pos	        : POSITION;
    float3 Velocity     : VELOCITY;
};

struct GsGenVelOutput
{
    float4 Pos	        : SV_Position;
    float3 Velocity     : VELOCITY;
    uint   RTIndex      : SV_RenderTargetArrayIndex;
};

struct GsGenVelIntVtx
  // Used internally in GS_GENVELOCITY to store 
  //   the result of an edge-slice intersection
{
    float2 Pos;
    float3 Velocity;
};

struct PsGenVelOutput
{
    float4  Velocity : SV_Target0;
    float   Obstacle : SV_Target1;
};



//--------------------------------------------------------------------------------------
// Vertex Shaders
//--------------------------------------------------------------------------------------

VsVoxOutput VS_VOXELIZE( VsVoxInput input )
{
    VsVoxOutput output;
    output.Pos = mul( float4(input.Pos,1), WorldViewProjection );
    return output;
}

VsGenVelOutput VS_GENVELOCITY( VsGenVelInput input )
{
    VsGenVelOutput output;
    float4 gridPos = mul( float4(input.Pos,1), WorldViewProjection );
    float4 prevGridPos = mul( float4(input.PrevPos,1), WorldViewProjection );
    output.Pos = gridPos;
    output.Velocity = (gridPos - prevGridPos) * 0.5f * gridDim * recTimeStep;
    // - multiply by 0.5f because these positions are in clip space (-1,1) in each axis, 
    //   instead of -0.5 to 0.5 (simulation volume space)
    // - multiply by gridDim to move to simulation voxel space, 
    //   which is the space in which velocity is assumed to be in FluidSim.fx
    return output;
}

VsResOutput VS_RESOLVE( VsResInput input )
{
    VsResOutput output;
    output.Pos = float4(input.Pos,1);
    output.Tex = input.Tex;
    return output;
}



//--------------------------------------------------------------------------------------
// Geometry Shaders
//--------------------------------------------------------------------------------------

[maxvertexcount (3)]
void GS_RESOLVE(triangle VsResOutput input[3], inout TriangleStream<GsResOutput> triStream)
{
    GsResOutput output;
    output.RTIndex = input[0].Tex.z;
    for(int v=0; v<3; v++)
    {
        output.Pos = input[v].Pos;
        output.Tex = input[v].Tex;
        triStream.Append( output );
    }
    triStream.RestartStrip( );
}


void GetEdgePlaneIntersection( VsGenVelOutput vA, VsGenVelOutput vB, float sliceZ, 
                              inout GsGenVelIntVtx intersections[2], inout int idx )
{
    // Compute intersection point (x,y), interpolated normal (projected onto plane) and velocity
    //  float3 P = float3(0, 0, sliceZ);
    //  float3 N = float3(0, 0, 1);
    //  float t = dot((P - vA.Pos), N) / dot((vB.Pos - vA.Pos), N);
    // We can optimize this a bit:
    float t = (sliceZ - vA.Pos.z) / (vB.Pos.z - vA.Pos.z);
    if( (t < 0) || (t > 1) )
        // line-plane intersection is not withing the edge's end-points (A and B)
        return;

    intersections[idx].Pos = lerp(vA.Pos, vB.Pos, t).xy;
    //intersections[idx].Normal = normalize(lerp(vA.Normal, vB.Normal, t).xy);
    intersections[idx].Velocity = lerp(vA.Velocity, vB.Velocity, t);
    idx++;
}

// GS_GENVELOCITY: GS that takes as input one triangle (3 vertices)
//  and outputs:
//      - 0 triangles, if the input triangle doesn't intersect the given slice
//      - 2 triangles, if the input triangle intersects the slice, which form a 1-pixel wide quadrilateral
//          along the triangle/slice intersection edge
[maxvertexcount (4)]
void GS_GENVELOCITY(triangle VsGenVelOutput input[3],
                              inout TriangleStream<GsGenVelOutput> triStream )
{
    GsGenVelOutput output;
    output.RTIndex = sliceIdx;

    float minZ = min( min(input[0].Pos.z, input[1].Pos.z), input[2].Pos.z);
    float maxZ = max( max(input[0].Pos.z, input[1].Pos.z), input[2].Pos.z);
    if( (sliceZ < minZ) || (sliceZ > maxZ) )
        // this triangle doesn't intersect the slice
        return;

    GsGenVelIntVtx intersections[2];
    for( int i=0; i<2; i++ )
    {
        intersections[i].Pos = 0;
        intersections[i].Velocity = 0;
    }

    int idx = 0;
    if( idx < 2 )
        GetEdgePlaneIntersection(input[0], input[1], sliceZ, intersections, idx);
    if( idx < 2 )
        GetEdgePlaneIntersection(input[1], input[2], sliceZ, intersections, idx);
    if( idx < 2 )
        GetEdgePlaneIntersection(input[2], input[0], sliceZ, intersections, idx);

    if( idx < 2 )
        return;

    float sqrtOf2 = 1.414; // the diagonal of a pixel
    float2 normal = sqrtOf2 * normalize(
        cross( (input[1].Pos - input[0].Pos), (input[2].Pos - input[0].Pos)).xy);

    for(int i=0; i<2; i++)
    {
        output.Pos = float4(intersections[i].Pos, 0, 1);
        output.Velocity = intersections[i].Velocity;
        triStream.Append( output );

        output.Pos = float4((intersections[i].Pos + (normal * projSpacePixDim)), 0, 1);
        output.Velocity = intersections[i].Velocity;
        triStream.Append( output );
    }
    triStream.RestartStrip( );
}



//--------------------------------------------------------------------------------------
// Pixel Shaders
//--------------------------------------------------------------------------------------

float4 PS_RESOLVE( GsResOutput input ) : SV_Target
{
    if( stencilbufferTex2D.Load(int3(input.Tex.x, input.Tex.y,0)).g )
        return 0.5;
    return 0;
}

PsGenVelOutput PS_GENVELOCITY( GsGenVelOutput input )
{
    PsGenVelOutput output;
    output.Velocity = float4(input.Velocity, 1.0);
    output.Obstacle = 1.0;
    return output;
}



//--------------------------------------------------------------------------------------

technique10 VoxelizeNZ
{
    pass NonZeroRule
    {
        SetVertexShader( CompileShader(vs_4_0, VS_VOXELIZE()) );
        SetGeometryShader( NULL );
        SetPixelShader( NULL );
        SetRasterizerState( RS_CullDisabled );
        SetBlendState( BS_NoBlending, float4( 0.0f, 0.0f, 0.0f, 0.0f ), 0xFFFFFFFF );
        SetDepthStencilState( DSS_NonZeroRule, 0 );
    }
}

technique10 VoxelizeResolveWithPS
{
    pass ResolveWithPS
    {
        SetVertexShader( CompileShader( vs_4_0, VS_RESOLVE()) );
        SetGeometryShader ( CompileShader(gs_4_0, GS_RESOLVE()) );
        SetPixelShader(CompileShader( ps_4_0, PS_RESOLVE()) );
        SetRasterizerState( RS_CullDisabled );
        SetBlendState( BS_NoBlending, float4( 0.0f, 0.0f, 0.0f, 0.0f ), 0xFFFFFFFF );
        SetDepthStencilState( DSS_Disabled, 0 );
    }
}

technique10 GenVelocityWireframe
{
    pass
    {
        SetVertexShader( CompileShader( vs_4_0, VS_GENVELOCITY()) );
        SetGeometryShader ( CompileShader(gs_4_0, GS_GENVELOCITY()) );
        SetPixelShader( CompileShader( ps_4_0, PS_GENVELOCITY()) );
        SetRasterizerState( RS_CullDisabled );
        SetBlendState( BS_NoBlending, float4( 0.0f, 0.0f, 0.0f, 0.0f ), 0xFFFFFFFF );
        SetDepthStencilState( DSS_Disabled, 0 );
    }
}
