// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLMESHSMOOTHER_H
#define WMLMESHSMOOTHER_H

#include "WmlVector3.h"

namespace Wml
{

template <class Real>
class WML_ITEM MeshSmoother
{
public:
    // The caller is responsible for deleting the input arrays.
    MeshSmoother (int iVQuantity, Vector3<Real>* akVertex, int iTQuantity,
        int* aiConnect);

    virtual ~MeshSmoother ();

    // For deferred construction and destruction.  The caller is responsible
    // for deleting the input arrays.
    MeshSmoother ();
    void Create (int iVQuantity, Vector3<Real>* akVertex, int iTQuantity,
        int* aiConnect);
    void Destroy ();

    // input values from the constructor
    int GetVQuantity () const;
    const Vector3<Real>* GetVertices () const;
    int GetTQuantity () const;
    const int* GetConnect () const;

    // derived quantites from the input mesh
    const Vector3<Real>* GetNormals () const;
    const Vector3<Real>* GetMeans () const;

    // Apply one iteration of the smoother.  The input time is supported for
    // applications where the surface evolution is time-dependent.
    void Update (Real fTime = (Real)0.0);

protected:
    virtual bool VertexInfluenced (int i, Real fTime);
    virtual Real GetTangentWeight (int i, Real fTime);
    virtual Real GetNormalWeight (int i, Real fTime);

    int m_iVQuantity;
    Vector3<Real>* m_akVertex;
    int m_iTQuantity;
    int* m_aiConnect;

    Vector3<Real>* m_akNormal;
    Vector3<Real>* m_akMean;
    int* m_aiNeighborCount;
};

typedef MeshSmoother<float> MeshSmootherf;
typedef MeshSmoother<double> MeshSmootherd;

}

#endif
