// David Eberly, Geometric Tools, Redmond WA 98052
// Copyright (c) 1998-2025
// Distributed under the Boost Software License, Version 1.0.
// https://www.boost.org/LICENSE_1_0.txt
// https://www.geometrictools.com/License/Boost/LICENSE_1_0.txt
// Version: 6.0.2022.01.03

cbuffer PVWMatrix
{
    float4x4 pvwMatrix;
};

cbuffer Material
{
    float4 materialEmissive;
    float4 materialAmbient;
};

cbuffer Lighting
{
    float4 lightingAmbient;
    float4 lightingAttenuation;
};

struct VS_INPUT
{
    float3 modelPosition : POSITION;
};

struct VS_OUTPUT
{
    float4 vertexColor : COLOR0;
    float4 clipPosition : SV_POSITION;
};

VS_OUTPUT VSMain(VS_INPUT input)
{
    VS_OUTPUT output;

    float3 ambient = lightingAttenuation.w * lightingAmbient.rgb;
    output.vertexColor.rgb = materialEmissive.rgb + materialAmbient.rgb * ambient;
    output.vertexColor.a = 1.0f;
#if GTE_USE_MAT_VEC
    output.clipPosition = mul(pvwMatrix, float4(input.modelPosition, 1.0f));
#else
    output.clipPosition = mul(float4(input.modelPosition, 1.0f), pvwMatrix);
#endif
    return output;
}
