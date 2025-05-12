// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef SILHOUETTE_H
#define SILHOUETTE_H

#include "WmlApplication2.h"
#include "WmlConvexPolyhedron3.h"
#include "WmlMatrix3.h"
#include <vector>
using namespace std;

class Silhouette : public Application2
{
public:
    Silhouette ();
    virtual ~Silhouette ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnDisplay ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);
    virtual void OnSpecialKeyDown (int iKey, int iX, int iY);

protected:
    void ConstructCube ();
    void ProjectPoint (const Vector3f& rkPoint, int& riX, int& riY);
    void DrawPolyhedron ();
    void DrawTerminator ();

    ConvexPolyhedron3f m_kPoly;
    Vector3f m_kEye;
    vector<Vector3f> m_kTerminator;
    Matrix3f m_kRot;
    Matrix3f m_kRotXP, m_kRotXM, m_kRotYP, m_kRotYM, m_kProd;
};

#endif
