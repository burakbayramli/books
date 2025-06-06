// David Eberly, Geometric Tools, Redmond WA 98052
// Copyright (c) 1998-2025
// Distributed under the Boost Software License, Version 1.0.
// https://www.boost.org/LICENSE_1_0.txt
// https://www.geometrictools.com/License/Boost/LICENSE_1_0.txt
// Version: 6.0.2022.01.03

// The 3D image is stored as a tiled 2D texture, in which case the selection
// of neighboring pixels is handled by using a precomputed offset texture.

Texture2D<float> inImage;
Texture2D<int2> inOffset;
RWTexture2D<float> outImage;

[numthreads(NUM_X_THREADS, NUM_Y_THREADS, 1)]
void CSMain(int2 t : SV_DispatchThreadID)
{
    outImage[t] = inImage[t + inOffset[t]];
}
