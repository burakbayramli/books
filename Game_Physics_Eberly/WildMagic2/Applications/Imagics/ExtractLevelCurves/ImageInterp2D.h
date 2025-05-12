// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef IMAGEINTERP2D_H
#define IMAGEINTERP2D_H

#include "WmlEdgeKey.h"
#include "WmlImages.h"
using namespace Wml;

#include <vector>
using namespace std;


class ImageInterp2D : public ImageInt2D
{
public:
    ImageInterp2D (int iXBound, int iYBound, int* aiData);
    ImageInterp2D (const char* acFilename);

    float LinearInterpolate (float fX, float fY) const;
    float BilinearInterpolate (float fX, float fY) const;

    class Vertex
    {
    public:
        Vertex (int iXNumer = 0, int iXDenom = 0, int iYNumer = 0,
            int iYDenom = 0);

        // for STL map
        bool operator< (const Vertex& rkVertex) const;

        void GetPair (float& rfX, float& rfY) const;
        void GetPair (double& rdX, double& rdY) const;

        // vertices stored as rational numbers (xnumer/xdenom,ynumer/ydenom)
        int m_iXNumer, m_iXDenom, m_iYNumer, m_iYDenom;
    };

    // The extraction assumes linear interpolation (decomposition of image
    // domain into tetrahedra).  To make the extraction fast, the arrays
    // make no attempt to store only unique values.  This supports rapid
    // interactive exploration of various level sets in an image until a
    // desired one is found.  At that time call MakeUnique to eliminate the
    // redundant information.
    void ExtractLevelSetLinear (int iLevel, int& riVertexQuantity,
        Vertex*& rakVertex, int& riEdgeQuantity, EdgeKey*& rakEdge);

    // The extraction assumes bilinear interpolation.
    void ExtractLevelSetBilinear (int iLevel, int& riVertexQuantity,
        Vertex*& rakVertex, int& riEdgeQuantity, EdgeKey*& rakEdge);

    // Remove redundant vertices and edges.
    static void MakeUnique (int& riVertexQuantity, Vertex*& rakVertex,
        int& riEdgeQuantity, EdgeKey*& rakEdge);

protected:
    // support for extraction of level sets
    typedef vector<Vertex> VertexArray;
    typedef vector<EdgeKey> EdgeArray;

    void AddVertex (VertexArray& rkVArray, int iXNumer, int iXDenom,
        int iYNumer, int iYDenom);

    void AddEdge (VertexArray& rkVArray, EdgeArray& rkEArray, int iXNumer0,
        int iXDenom0, int iYNumer0, int iYDenom0, int iXNumer1, int iXDenom1,
        int iYNumer1, int iYDenom1);

    // support for extraction with linear interpolation
    void ProcessTriangle (int iLevel, VertexArray& rkVA, EdgeArray& rkEA,
        int iX0, int iY0, int iF0, int iX1, int iY1, int iF1, int iX2,
        int iY2, int iF2);

    // support for extraction with bilinear interpolation
    void ProcessSquare (int iLevel, VertexArray& rkVA, EdgeArray& rkEA,
        int iX, int iXP, int iY, int iYP, int iF00, int iF10, int iF11,
        int iF01);
};

#endif
