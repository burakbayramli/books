// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLMORPHCONTROLLER_H
#define WMLMORPHCONTROLLER_H

#include "WmlController.h"
#include "WmlGeometry.h"

namespace Wml
{

class WML_ITEM MorphController : public Controller
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // Construction and destruction.  Any data that remains in the vertex
    // arrays or weight arrays will be deleted by MorphController.  If
    // that data is to be retrieved by the application so that is is not
    // deleted by the destructor, call SetVertices and SetWeights with null
    // pointers.
    MorphController ();
    virtual ~MorphController ();

    // Target access.  A call to SetTargetQuantity will reallocate the
    // array of vertex pointers.  The application should provide the actual
    // arrays of vertices by calls to SetVertices.  It is possible to set
    // the vertices without changing the target quantity.  In this case the
    // SetVertices call returns the pointer to the old data and the
    // application is responsible for deleting the old data.
    void SetTargetQuantity (int iTargetQuantity);
    int GetTargetQuantity () const;
    Vector3f* SetVertices (int i, Vector3f* akVertex);
    Vector3f* GetVertices (int i);

    // Key access.  A call to SetKeyQuantity will reallocate the array of
    // weight pointers.  The application should provide the actual arrays of
    // weights by calls to SetWeights.
    void SetKeyQuantity (int iKeyQuantity);
    int GetKeyQuantity () const;
    float* SetTimes (float* afTime);
    float* GetTimes ();
    float* SetWeights (int i, float* afWeight);
    float* GetWeights (int i);

    // updates
    virtual bool Update (float fAppTime);
    bool& UpdateNormals ();

protected:
    void DeleteTargets ();
    void DeleteTimes ();
    void DeleteWeights ();

    // lookup on bounding keys
    void GetKeyInfo (float fCtrlTime, float& rfTime, float& rfOmTime,
        int& ri0, int& ri1);

    // Target geometry.  The number of vertices per target must match the
    // number of vertices in the managed geometry object.  The array of
    // vertices at location 0 are those of one of the targets.  Based on the
    // comments about morph keys, the array at location i >= 1 is computed
    // as the difference between the i-th target and the 0-th target.
    int m_iTargetQuantity;
    Vector3f** m_aakVertex;  // [target_quantity][geometry_quantity]
    bool m_bUpdateNormals;

    // Morph keys.  The morphed object is a combination of N targets by
    // weights w[0] through w[N-1] with w[i] in [0,1] and sum_i w[i] = 1.
    // Each combination is sum_{i=0}^{N-1} w[i]*X[i] where X[i] is a vertex
    // of the i-th target.  This can be rewritten as a combination
    // X[0] + sum_{i=0}^{N-2} w[i] Y[i] where w'[i] = w[i+1] and
    // Y[i] = X[i+1] - X[0].  The weights stored in this class are the
    // w'[i] (to reduce storage).  This also reduces computation time by a
    // small amount (coefficient of X[0] is 1, so no multiplication must
    // occur).
    int m_iKeyQuantity;
    float* m_afTime;  // [key_quantity]
    float** m_aafWeight;  // [key_quantity][target_quantity-1]

    // for O(1) lookup on bounding keys
    int m_iLastIndex;
};

WmlSmartPointer(MorphController);
WmlRegisterStream(MorphController);
#include "WmlMorphController.inl"

}

#endif
