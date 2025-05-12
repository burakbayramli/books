// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLMESHCURVATURE_H
#define WMLMESHCURVATURE_H

#include "WmlMatrix3.h"

namespace Wml
{

template <class Real>
class WML_ITEM MeshCurvature
{
public:
    // The caller is responsible for deleting the input arrays.
    MeshCurvature (int iVQuantity, const Vector3<Real>* akVertex,
        int iTQuantity, const int* aiConnect);

    virtual ~MeshCurvature ();

    // input values from the constructor
    int GetVQuantity () const;
    const Vector3<Real>* GetVertices () const;
    int GetTQuantity () const;
    const int* GetConnect () const;

    // derived quantites from the input mesh
    const Vector3<Real>* GetNormals () const;
    const Real* GetMinCurvatures () const;
    const Real* GetMaxCurvatures () const;
    const Vector3<Real>* GetMinDirections () const;
    const Vector3<Real>* GetMaxDirections () const;

protected:
    int m_iVQuantity;
    const Vector3<Real>* m_akVertex;
    int m_iTQuantity;
    const int* m_aiConnect;

    Vector3<Real>* m_akNormal;
    Real* m_afMinCurvature;
    Real* m_afMaxCurvature;
    Vector3<Real>* m_akMinDirection;
    Vector3<Real>* m_akMaxDirection;
};

typedef MeshCurvature<float> MeshCurvaturef;
typedef MeshCurvature<double> MeshCurvatured;

}

#endif
