// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef IMAGETESSELATOR_H
#define IMAGETESSELATOR_H

class ImageTessellator
{
public:
    ImageTessellator (int iSize, const int* aiImage);
    ~ImageTessellator ();

    void Tessellate (int iTolerance);
    int GetNumTriangles () const { return m_iNumTriangles; }
    const int* GetIndices () const { return m_aiIndex; }

protected:
    class Vertex
    {
    public:
        Vertex ();
        void Enable ();
        void Disable ();

        Vertex* m_apkDependent[2];
        bool m_bEnabled;
    };

    class Block
    {
    public:
        void Initialize (ImageTessellator* pTess, int iBlock, int iX,
            int iY, int iStride, bool bEven);

        void Simplify (ImageTessellator* pTess, int iBlock);

        int m_iX, m_iY, m_iStride;
        int m_iDelta[5], m_iMin, m_iMax;
    };

    void SelectTriangles (int iT, int iL, int iR);

    int m_iSize, m_iSizeM1, m_iQuantity;
    const int* m_aiImage;
    int m_iNumTriangles, m_iMaxNumTriangles;
    int m_iTolerance;
    int* m_aiIndex;

    int m_iNumBlocks;
    Block* m_akBlock;

    Vertex* m_akVertex;

    friend class Block;
    friend class Vertex;
};

#endif
