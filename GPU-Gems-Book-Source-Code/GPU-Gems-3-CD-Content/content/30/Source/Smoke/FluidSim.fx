//----------------------------------------------------------------------------------
// File:   FluidSim.fx
// Author: Sarah Tariq and Ignacio Llamas
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
// Shaders to implement a "stable fluids" style semi-Lagrangian solver for 3D smoke
//--------------------------------------------------------------------------------------
// It assumes the velocity and pressure grids are collocated
// It handles boundary conditions for static obstacles stored as an in/out voxel volume
// BFECC is supported for smoke density advection
// The diffusion step is skipped
//--------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------
// Textures
//--------------------------------------------------------------------------------------

Texture3D Texture_velocity0;
Texture3D Texture_velocity1;
Texture3D Texture_color;
Texture3D Texture_obstacles;
Texture3D Texture_obstvelocity;
Texture3D Texture_pressure;
Texture3D Texture_tempscalar;
Texture3D Texture_tempvector;


//--------------------------------------------------------------------------------------
// Variables
//--------------------------------------------------------------------------------------

float       textureHeight;
float       textureWidth;
float       textureDepth;

int         textureNumber = 1;

float4      obstVelocity = float4(0, 0, 0, 0);

float       modulate = 1.0;
float       size;
float3      center; 
float4      splatColor;
float       epsilon;
float       timestep;
float       forward = 1.0;
float3      halfVolumeDim;

float3      boxLBDcorner;
float3      boxRTUcorner;


//--------------------------------------------------------------------------------------
// Pipeline State definitions
//--------------------------------------------------------------------------------------

SamplerState samPointClamp
{
    Filter = MIN_MAG_MIP_POINT;
    AddressU = Clamp;
    AddressV = Clamp;
    AddressW = Clamp;
};

SamplerState samLinear
{
    Filter = MIN_MAG_MIP_LINEAR;
    AddressU = Clamp;
    AddressV = Clamp;
    AddressW = Clamp;
};


BlendState NoBlending
{
    AlphaToCoverageEnable = FALSE;
    BlendEnable[0] = FALSE;
    RenderTargetWriteMask[0] = 0x0F;
};

DepthStencilState DisableDepth
{
        DepthEnable = false;
        DepthWriteMask = ZERO;
        DepthFunc = Less;

        //Stencil
        StencilEnable = false;
        StencilReadMask = 0xFF;
        StencilWriteMask = 0x00;
};

BlendState AdditiveBlending
{
    AlphaToCoverageEnable = FALSE;
    BlendEnable[0] = TRUE;
    SrcBlend = ONE;
    DestBlend = ONE;
    BlendOp = ADD;
    SrcBlendAlpha = ONE;
    DestBlendAlpha = ONE;
    BlendOpAlpha = ADD;
    RenderTargetWriteMask[0] = 0x0F;
}; 

BlendState AlphaBlending
{
    AlphaToCoverageEnable = FALSE;
    BlendEnable[0] = TRUE;
    SrcBlend = SRC_ALPHA;
    DestBlend = INV_SRC_ALPHA;
    BlendOp = ADD;
    SrcBlendAlpha = ONE;
    DestBlendAlpha = ONE;
    BlendOpAlpha = ADD;
    RenderTargetWriteMask[0] = 0x0F;
}; 

RasterizerState CullBack
{
    MultiSampleEnable = False;
    CullMode = Back;
};

RasterizerState CullNone
{
    MultiSampleEnable = False;
    CullMode=None;
};


//--------------------------------------------------------------------------------------
// Structs
//--------------------------------------------------------------------------------------

struct VS_INPUT_FLUIDSIM
{
    float3 position          : POSITION;    // 2D slice vertex coordinates in clip space
    float3 textureCoords0    : TEXCOORD;    // 3D cell coordinates (x,y,z in 0-dimension range)
};

struct VS_OUTPUT_FLUIDSIM
{
    float4 pos               : SV_Position;
    float3 cell0             : TEXCOORD0;
    float3 texcoords         : TEXCOORD1;
    float2 LR                : TEXCOORD2;
    float2 BT                : TEXCOORD3;
    float2 DU                : TEXCOORD4;
};

struct GS_OUTPUT_FLUIDSIM
{
    float4 pos               : SV_Position; // 2D slice vertex coordinates in homogenous clip space
    float3 cell0             : TEXCOORD0;   // 3D cell coordinates (x,y,z in 0-dimension range)
    float3 texcoords         : TEXCOORD1;   // 3D cell texcoords (x,y,z in 0-1 range)
    float2 LR                : TEXCOORD2;   // 3D cell texcoords for the Left and Right neighbors
    float2 BT                : TEXCOORD3;   // 3D cell texcoords for the Bottom and Top neighbors
    float2 DU                : TEXCOORD4;   // 3D cell texcoords for the Down and Up neighbors
    uint RTIndex             : SV_RenderTargetArrayIndex;  // used to choose the destination slice
};

#define LEFTCELL    float3 (input.LR.x, input.texcoords.y, input.texcoords.z)
#define RIGHTCELL   float3 (input.LR.y, input.texcoords.y, input.texcoords.z)
#define BOTTOMCELL  float3 (input.texcoords.x, input.BT.x, input.texcoords.z)
#define TOPCELL     float3 (input.texcoords.x, input.BT.y, input.texcoords.z)
#define DOWNCELL    float3 (input.texcoords.x, input.texcoords.y, input.DU.x)
#define UPCELL      float3 (input.texcoords.x, input.texcoords.y, input.DU.y)

//--------------------------------------------------------------------------------------
// Vertex shaders
//--------------------------------------------------------------------------------------

VS_OUTPUT_FLUIDSIM VS_GRID( VS_INPUT_FLUIDSIM input)
{
    VS_OUTPUT_FLUIDSIM output = (VS_OUTPUT_FLUIDSIM)0;

    output.pos = float4(input.position.x, input.position.y, input.position.z, 1.0);
    output.cell0 = float3(input.textureCoords0.x, input.textureCoords0.y, input.textureCoords0.z);
    output.texcoords = float3( (input.textureCoords0.x)/(textureWidth),
                              (input.textureCoords0.y)/(textureHeight), 
                              (input.textureCoords0.z+0.5)/(textureDepth));

    float x = output.texcoords.x;
    float y = output.texcoords.y;
    float z = output.texcoords.z;

    // compute single texel offsets in each dimension
    float invW = 1.0/textureWidth;
    float invH = 1.0/textureHeight;
    float invD = 1.0/textureDepth;

    output.LR = float2(x - invW, x + invW);
    output.BT = float2(y - invH, y + invH);
    output.DU = float2(z - invD, z + invD);

    return output;
}

[maxvertexcount (3)]
void GS_ARRAY(triangle VS_OUTPUT_FLUIDSIM In[3], inout TriangleStream<GS_OUTPUT_FLUIDSIM> triStream)
{
    GS_OUTPUT_FLUIDSIM Out;
    // cell0.z of the first vertex in the triangle determines the destination slice index
    Out.RTIndex = In[0].cell0.z;
    for(int v=0; v<3; v++)
    {
        Out.pos          = In[v].pos; 
        Out.cell0        = In[v].cell0;
        Out.texcoords    = In[v].texcoords;
        Out.LR           = In[v].LR;
        Out.BT           = In[v].BT;
        Out.DU           = In[v].DU;
        triStream.Append( Out );
    }
    triStream.RestartStrip( );
}

[maxvertexcount (2)]
void GS_ARRAY_LINE(line VS_OUTPUT_FLUIDSIM In[2], inout LineStream<GS_OUTPUT_FLUIDSIM> Stream)
{
    GS_OUTPUT_FLUIDSIM Out;
    // cell0.z of the first vertex in the line determines the destination slice index
    Out.RTIndex = In[0].cell0.z;
    for(int v=0; v<2; v++)
    {
        Out.pos          = In[v].pos; 
        Out.cell0        = In[v].cell0;
        Out.texcoords    = In[v].texcoords;
        Out.LR           = In[v].LR;
        Out.BT           = In[v].BT;
        Out.DU           = In[v].DU;

        Stream.Append( Out );
    }
    Stream.RestartStrip( );
}


//--------------------------------------------------------------------------------------
// Helper functions
//--------------------------------------------------------------------------------------

float4 GetObstVelocity( float3 cellTexCoords )
{
    return Texture_obstvelocity.SampleLevel(samPointClamp, cellTexCoords, 0);
}

bool IsNonEmptyCell( float3 cellTexCoords )
{
    return (Texture_obstacles.SampleLevel(samPointClamp, cellTexCoords, 0).r > 0.0);
}

bool IsBoundaryCell( float3 cellTexCoords )
{
    return (Texture_obstacles.SampleLevel(samPointClamp, cellTexCoords, 0).r > 0.9);    
}

float3 GetAdvectedPosTexCoords(GS_OUTPUT_FLUIDSIM input)
{
    float3 pos = input.cell0;

    pos -= timestep * forward *
        Texture_velocity0.SampleLevel( samPointClamp, input.texcoords, 0 ).xyz;

    return float3(pos.x/textureWidth, pos.y/textureHeight, (pos.z+0.5)/textureDepth);
}

//--------------------------------------------------------------------------------------
// Pixel shaders
//--------------------------------------------------------------------------------------

float4 PS_ADVECT_BFECC( GS_OUTPUT_FLUIDSIM input ) : SV_Target
{
    if( IsNonEmptyCell(input.texcoords.xyz) )
        return 0;

    float3 npos = GetAdvectedPosTexCoords(input);

    float4 r;
    float3 diff = abs( halfVolumeDim - input.cell0 );

    // Must use regular semi-Lagrangian advection instead of BFECC at the volume boundaries
    if( (diff.x > (halfVolumeDim.x-4)) || (diff.y > (halfVolumeDim.y-4)) || (diff.z > (halfVolumeDim.z-4)) )
    {
       r = Texture_color.SampleLevel( samLinear, npos, 0);
    }
    else
    {
        // Texture_color contains \phi^n; Texture_tempscalar contains \bar{\phi}
        //  (i.e.: the result of 1 forward advection step, followed by a backwards advection step)
        r = 1.5f * Texture_color.SampleLevel( samLinear, npos, 0)
            - 0.5f * Texture_tempscalar.SampleLevel( samLinear, npos, 0);
    }

    r = saturate(r);
    return r*modulate;
}

float4 PS_ADVECT( GS_OUTPUT_FLUIDSIM input ) : SV_Target
{
    if( IsNonEmptyCell(input.texcoords.xyz) )
        return 0;

    float3 npos = GetAdvectedPosTexCoords(input);

    return Texture_color.SampleLevel( samLinear, npos, 0) * modulate;
}

float4 PS_ADVECT_VEL( GS_OUTPUT_FLUIDSIM input ) : SV_Target
{
    float3 npos = GetAdvectedPosTexCoords(input);

    return Texture_velocity0.SampleLevel( samLinear, npos, 0) * modulate;  
}

float4 PS_VORTICITY( GS_OUTPUT_FLUIDSIM input ) : SV_Target
{
    float4 L = Texture_velocity1.SampleLevel( samPointClamp, LEFTCELL, 0 );
    float4 R = Texture_velocity1.SampleLevel( samPointClamp, RIGHTCELL, 0 );
    float4 B = Texture_velocity1.SampleLevel( samPointClamp, BOTTOMCELL, 0 );
    float4 T = Texture_velocity1.SampleLevel( samPointClamp, TOPCELL, 0 );
    float4 D = Texture_velocity1.SampleLevel( samPointClamp, DOWNCELL, 0 );
    float4 U = Texture_velocity1.SampleLevel( samPointClamp, UPCELL, 0 );

    float4 vorticity;
    vorticity.xyz = 0.5 * float3( (( T.z - B.z ) - ( U.y - D.y )) ,
                                 (( U.x - D.x ) - ( R.z - L.z )) ,
                                 (( R.y - L.y ) - ( T.x - B.x )) );
                                 
    return vorticity;
}

float4 PS_CONFINEMENT( GS_OUTPUT_FLUIDSIM input ) : SV_Target
{
    // Texture_tempvector contains the vorticity computed by PS_VORTICITY
    float4 omega = Texture_tempvector.SampleLevel( samPointClamp, input.texcoords, 0 );

    // Potential optimization: don't find length multiple times - do once for the entire texture
    float omegaL = length( Texture_tempvector.SampleLevel( samPointClamp, LEFTCELL, 0 ) );
    float omegaR = length( Texture_tempvector.SampleLevel( samPointClamp, RIGHTCELL, 0 ) );
    float omegaB = length( Texture_tempvector.SampleLevel( samPointClamp, BOTTOMCELL, 0 ) );
    float omegaT = length( Texture_tempvector.SampleLevel( samPointClamp, TOPCELL, 0 ) );
    float omegaD = length( Texture_tempvector.SampleLevel( samPointClamp, DOWNCELL, 0 ) );
    float omegaU = length( Texture_tempvector.SampleLevel( samPointClamp, UPCELL, 0 ) );

    float3 eta = 0.5 * float3( omegaR - omegaL,
                              omegaT - omegaB,
                              omegaU - omegaD );

    eta = normalize( eta + float3(0.001,0.001,0.001) );

    float4 force;
    force.xyz = timestep * epsilon * float3( eta.y * omega.z - eta.z * omega.y,
                                            eta.z * omega.x - eta.x * omega.z,
                                            eta.x * omega.y - eta.y * omega.x );
    
    // Note: the result is added to the current velocity at each cell using "additive blending"
    return force;
}

float PS_DIVERGENCE( GS_OUTPUT_FLUIDSIM input ) : SV_Target
{
    float4 fieldL = Texture_velocity1.SampleLevel( samPointClamp, LEFTCELL, 0 );
    float4 fieldR = Texture_velocity1.SampleLevel( samPointClamp, RIGHTCELL, 0 );
    float4 fieldB = Texture_velocity1.SampleLevel( samPointClamp, BOTTOMCELL, 0 );
    float4 fieldT = Texture_velocity1.SampleLevel( samPointClamp, TOPCELL, 0 );
    float4 fieldD = Texture_velocity1.SampleLevel( samPointClamp, DOWNCELL, 0 );
    float4 fieldU = Texture_velocity1.SampleLevel( samPointClamp, UPCELL, 0 );

    if( IsBoundaryCell(LEFTCELL) )  fieldL = GetObstVelocity(LEFTCELL);
    if( IsBoundaryCell(RIGHTCELL) ) fieldR = GetObstVelocity(RIGHTCELL);
    if( IsBoundaryCell(BOTTOMCELL) )fieldB = GetObstVelocity(BOTTOMCELL);
    if( IsBoundaryCell(TOPCELL) )   fieldT = GetObstVelocity(TOPCELL);
    if( IsBoundaryCell(DOWNCELL) )  fieldD = GetObstVelocity(DOWNCELL);
    if( IsBoundaryCell(UPCELL) )    fieldU = GetObstVelocity(UPCELL);

    float divergence =  0.5 *
        ( ( fieldR.x - fieldL.x ) + ( fieldT.y - fieldB.y ) + ( fieldU.z - fieldD.z ) );

    return divergence;
}

float PS_JACOBI( GS_OUTPUT_FLUIDSIM input ) : SV_Target
{
    float pCenter = Texture_pressure.SampleLevel( samPointClamp, input.texcoords, 0 );
    // Texture_tempvector contains the "divergence" computed by PS_DIVERGENCE
    float bC = Texture_tempvector.SampleLevel( samPointClamp, input.texcoords, 0 );

    float pL = Texture_pressure.SampleLevel( samPointClamp, LEFTCELL, 0 );
    float pR = Texture_pressure.SampleLevel( samPointClamp, RIGHTCELL, 0 );
    float pB = Texture_pressure.SampleLevel( samPointClamp, BOTTOMCELL, 0 );
    float pT = Texture_pressure.SampleLevel( samPointClamp, TOPCELL, 0 );
    float pD = Texture_pressure.SampleLevel( samPointClamp, DOWNCELL, 0 );
    float pU = Texture_pressure.SampleLevel( samPointClamp, UPCELL, 0 );

    if( IsBoundaryCell(LEFTCELL) )  pL = pCenter;
    if( IsBoundaryCell(RIGHTCELL) ) pR = pCenter;
    if( IsBoundaryCell(BOTTOMCELL) )pB = pCenter;
    if( IsBoundaryCell(TOPCELL) )   pT = pCenter; 
    if( IsBoundaryCell(DOWNCELL) )  pD = pCenter;  
    if( IsBoundaryCell(UPCELL) )    pU = pCenter;

    return( pL + pR + pB + pT + pU + pD - bC ) /6.0;
}

float4 PS_PROJECT( GS_OUTPUT_FLUIDSIM input ): SV_Target
{
    if( IsBoundaryCell(input.texcoords.xyz) )
        return GetObstVelocity(input.texcoords.xyz);

    float pCenter = Texture_pressure.SampleLevel( samPointClamp, input.texcoords, 0 ); 
    float pL = Texture_pressure.SampleLevel( samPointClamp, LEFTCELL, 0 );
    float pR = Texture_pressure.SampleLevel( samPointClamp, RIGHTCELL, 0 );
    float pB = Texture_pressure.SampleLevel( samPointClamp, BOTTOMCELL, 0 );
    float pT = Texture_pressure.SampleLevel( samPointClamp, TOPCELL, 0 );
    float pD = Texture_pressure.SampleLevel( samPointClamp, DOWNCELL, 0 );
    float pU = Texture_pressure.SampleLevel( samPointClamp, UPCELL, 0 );

    float4 velocity;
    float3 obstV = float3(0,0,0);
    float3 vMask = float3(1,1,1);
    float3 vLeft = GetObstVelocity(LEFTCELL);
    float3 vRight = GetObstVelocity(RIGHTCELL);
    float3 vBottom = GetObstVelocity(BOTTOMCELL);
    float3 vTop = GetObstVelocity(TOPCELL);
    float3 vDown = GetObstVelocity(DOWNCELL);
    float3 vUp = GetObstVelocity(UPCELL);
    float3 v;

    if( IsBoundaryCell(LEFTCELL) )  { pL = pCenter; obstV.x = vLeft.x; vMask.x = 0; }
    if( IsBoundaryCell(RIGHTCELL) ) { pR = pCenter; obstV.x = vRight.x; vMask.x = 0; }
    if( IsBoundaryCell(BOTTOMCELL) ){ pB = pCenter; obstV.y = vBottom.y; vMask.y = 0; }
    if( IsBoundaryCell(TOPCELL) )   { pT = pCenter; obstV.y = vTop.y; vMask.y = 0; }
    if( IsBoundaryCell(DOWNCELL) )  { pD = pCenter; obstV.z = vDown.z; vMask.z = 0; }
    if( IsBoundaryCell(UPCELL) )    { pU = pCenter; obstV.z = vUp.z; vMask.z = 0; }

    v = ( Texture_velocity1.SampleLevel( samPointClamp, input.texcoords, 0 ).xyz -
                 (0.5*modulate*float3( pR - pL, pT - pB, pU - pD )) );

    velocity.xyz = (vMask * v) + obstV;

    return velocity;
}

float4 PS_GAUSSIAN( GS_OUTPUT_FLUIDSIM input ) : SV_Target 
{
    if( IsNonEmptyCell(input.texcoords.xyz) )
        return 0;

    float dist = length( input.cell0 - center ) * size;
    float4 result;
    result.rgb = splatColor;    // + sin(splatColor.rgb*10.0+cell*5.0)*0.2;
    result.a = exp( -dist*dist );

    return result;
}

float4 PS_DRAW_TEXTURE( VS_OUTPUT_FLUIDSIM input ) : SV_Target
{
    if( textureNumber == 1)
        return abs(Texture_color.SampleLevel(samLinear,input.texcoords,0)).xxxx;
    else if( textureNumber == 2)
        return abs(Texture_velocity0.SampleLevel(samLinear,input.texcoords,0));
    else
        return float4(abs(Texture_obstvelocity.SampleLevel(samLinear,input.texcoords,0).xy),
            abs(Texture_obstacles.SampleLevel(samLinear,input.texcoords,0).r),1 );
}

float4 PS_DRAW_WHITE( GS_OUTPUT_FLUIDSIM input ) : SV_Target
{
    return float4(1,1,1,1);
}

bool PointIsInsideBox(float3 p, float3 LBUcorner, float3 RTDcorner)
{
    return ((p.x > LBUcorner.x) && (p.x < RTDcorner.x)
        &&  (p.y > LBUcorner.y) && (p.y < RTDcorner.y)
        &&  (p.z > LBUcorner.z) && (p.z < RTDcorner.z));
}

struct PSDrawBoxOut
{
    float4 obstacle : SV_TARGET0;
    float4 velocity : SV_TARGET1;
};

PSDrawBoxOut PS_DRAW_BOX( GS_OUTPUT_FLUIDSIM input )
{
    PSDrawBoxOut voxel;
    float3 innerboxLBDcorner = boxLBDcorner + 1;
    float3 innerboxRTUcorner = boxRTUcorner - 1;
    // cells completely inside box = 1.0
    if(PointIsInsideBox(input.cell0, innerboxLBDcorner, innerboxRTUcorner))
    {
        voxel.obstacle = 0.5;
        voxel.velocity = 0;
        return voxel;
    }

    // cells in box boundary = 0.5
    if(PointIsInsideBox(input.cell0, boxLBDcorner, boxRTUcorner))
    {
        voxel.obstacle = 1.0;
        voxel.velocity = float4(obstVelocity.xyz,1);
        return voxel;
    }

    return (PSDrawBoxOut)0;
}


//--------------------------------------------------------------------------------------
// Techniques
//--------------------------------------------------------------------------------------

technique10 Advect
{
    pass 
    {
        SetVertexShader(CompileShader( vs_4_0,  VS_GRID()          ));
        SetGeometryShader( CompileShader( gs_4_0,  GS_ARRAY()      ));
        SetPixelShader( CompileShader( ps_4_0,  PS_ADVECT()      ));

        SetBlendState( NoBlending, float4( 0.0f, 0.0f, 0.0f, 0.0f ), 0xFFFFFFFF );
        SetDepthStencilState( DisableDepth, 0 );
        SetRasterizerState(CullNone);
    }
}

technique10 AdvectBFECC
{
    pass
    {
        SetVertexShader(CompileShader( vs_4_0,  VS_GRID()          ));
        SetGeometryShader( CompileShader( gs_4_0,  GS_ARRAY()      ));
        SetPixelShader( CompileShader( ps_4_0,  PS_ADVECT_BFECC()  ));

        SetBlendState( NoBlending, float4( 0.0f, 0.0f, 0.0f, 0.0f ), 0xFFFFFFFF );
        SetDepthStencilState( DisableDepth, 0 );
        SetRasterizerState(CullNone);
    }
}

technique10 AdvectVel
{
    pass
    {
        SetVertexShader(CompileShader( vs_4_0,  VS_GRID()          ));
        SetGeometryShader( CompileShader( gs_4_0,  GS_ARRAY()      ));
        SetPixelShader( CompileShader( ps_4_0,  PS_ADVECT_VEL()    ));

        SetBlendState( NoBlending, float4( 0.0f, 0.0f, 0.0f, 0.0f ), 0xFFFFFFFF );
        SetDepthStencilState( DisableDepth, 0 );
        SetRasterizerState(CullNone);
    }
}

technique10 Vorticity
{
    pass
    {
        SetVertexShader(CompileShader( vs_4_0,  VS_GRID()          ));
        SetGeometryShader( CompileShader( gs_4_0,  GS_ARRAY()      ));
        SetPixelShader( CompileShader( ps_4_0,  PS_VORTICITY()     ));

        SetBlendState( NoBlending, float4( 0.0f, 0.0f, 0.0f, 0.0f ), 0xFFFFFFFF );
        SetDepthStencilState( DisableDepth, 0 );
        SetRasterizerState(CullNone);
    }
}

technique10 Confinement
{
    pass
    {
        SetVertexShader(CompileShader( vs_4_0,  VS_GRID()          ));
        SetGeometryShader( CompileShader( gs_4_0,  GS_ARRAY()      ));
        SetPixelShader( CompileShader( ps_4_0,  PS_CONFINEMENT()   ));

        SetBlendState( AdditiveBlending, float4( 0.0f, 0.0f, 0.0f, 0.0f ), 0xFFFFFFFF );
        SetDepthStencilState( DisableDepth, 0 );
        SetRasterizerState(CullNone);
    }
}

technique10 Gaussian
{
    pass
    {
        SetVertexShader(CompileShader( vs_4_0,  VS_GRID()          ));
        SetGeometryShader( CompileShader( gs_4_0,  GS_ARRAY()      ));
        SetPixelShader( CompileShader( ps_4_0,  PS_GAUSSIAN()      ));

        SetBlendState( AlphaBlending, float4( 0.0f, 0.0f, 0.0f, 0.0f ), 0xFFFFFFFF );
        SetDepthStencilState( DisableDepth, 0 );
        SetRasterizerState(CullNone);
    }
}

technique10 Divergence
{
    pass
    {
        SetVertexShader(CompileShader( vs_4_0,  VS_GRID()          ));
        SetGeometryShader( CompileShader( gs_4_0,  GS_ARRAY()      ));
        SetPixelShader( CompileShader( ps_4_0,  PS_DIVERGENCE()    ));

        SetBlendState( NoBlending, float4( 0.0f, 0.0f, 0.0f, 0.0f ), 0xFFFFFFFF );
        SetDepthStencilState( DisableDepth, 0 );
        SetRasterizerState(CullNone);
    }
}

technique10 Jacobi
{
    pass
    {
        SetVertexShader(CompileShader( vs_4_0,  VS_GRID()          ));
        SetGeometryShader( CompileShader( gs_4_0,  GS_ARRAY()      ));
        SetPixelShader( CompileShader( ps_4_0,  PS_JACOBI()      ));

        SetBlendState( NoBlending, float4( 0.0f, 0.0f, 0.0f, 0.0f ), 0xFFFFFFFF );
        SetDepthStencilState( DisableDepth, 0 );
        SetRasterizerState(CullNone);
    }
}

technique10 Project
{
    pass
    {
        SetVertexShader(CompileShader( vs_4_0,  VS_GRID()          ));
        SetGeometryShader( CompileShader( gs_4_0,  GS_ARRAY()      ));
        SetPixelShader( CompileShader( ps_4_0,  PS_PROJECT()       ));

        SetBlendState( NoBlending, float4( 0.0f, 0.0f, 0.0f, 0.0f ), 0xFFFFFFFF );
        SetDepthStencilState( DisableDepth, 0 );
        SetRasterizerState(CullNone);
    }
}

    // All the techniques below are use to either draw to the screen
    //  or to initialize the textures

technique10 DrawTexture
{
    pass
    {
        SetVertexShader(CompileShader( vs_4_0,  VS_GRID()          ));
        SetGeometryShader( NULL );
        SetPixelShader( CompileShader( ps_4_0,  PS_DRAW_TEXTURE()  ));

        SetBlendState( NoBlending, float4( 0.0f, 0.0f, 0.0f, 0.0f ), 0xFFFFFFFF );
        SetDepthStencilState( DisableDepth, 0 );
        SetRasterizerState(CullNone);    
    }
}

technique10 DrawWhiteTriangles
{
    pass
    {
        SetVertexShader(CompileShader( vs_4_0,  VS_GRID()          ));
        SetGeometryShader( CompileShader( gs_4_0,  GS_ARRAY()      ));
        SetPixelShader( CompileShader( ps_4_0,  PS_DRAW_WHITE()    ));

        SetBlendState( NoBlending, float4( 0.0f, 0.0f, 0.0f, 0.0f ), 0xFFFFFFFF );
        SetDepthStencilState( DisableDepth, 0 );
        SetRasterizerState(CullBack);
    }
}

technique10 DrawWhiteLines
{
    pass
    {
        SetVertexShader(CompileShader( vs_4_0,  VS_GRID()          ));
        SetGeometryShader( CompileShader( gs_4_0,  GS_ARRAY_LINE() ));
        SetPixelShader( CompileShader( ps_4_0,  PS_DRAW_WHITE()    ));

        SetBlendState( NoBlending, float4( 0.0f, 0.0f, 0.0f, 0.0f ), 0xFFFFFFFF );
        SetDepthStencilState( DisableDepth, 0 );
        SetRasterizerState(CullBack);
    }
}

technique10 DrawBox
{
    pass
    {
        SetVertexShader(CompileShader( vs_4_0,  VS_GRID()          ));
        SetGeometryShader( CompileShader( gs_4_0,  GS_ARRAY()      ));
        SetPixelShader( CompileShader( ps_4_0,  PS_DRAW_BOX()      ));

        SetBlendState( NoBlending, float4( 0.0f, 0.0f, 0.0f, 0.0f ), 0xFFFFFFFF );
        SetDepthStencilState( DisableDepth, 0 );
        SetRasterizerState(CullBack);
    }
}

