// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef SPHERETESSELLATION_H
#define SPHERETESSELLATION_H

#include "WmlApplication2.h"
#include "WmlQuadricSurface.h"

class SphereTessellation : public Application2
{
public:
    SphereTessellation ();
    virtual ~SphereTessellation ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnDisplay ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);
    virtual void OnSpecialKeyDown (int iKey, int iX, int iY);

protected:
    void CreatePolyhedron (bool bDeleteOld);
    void CreateTetrahedron (QuadricSurfacef::ConvexPolyhedron& rkTetra);
    void CreateOctahedron (QuadricSurfacef::ConvexPolyhedron& rkOct);
    void ProjectPoint (const Vector3f& rkPoint, int& riX, int& riY) const;
    void DrawPolyhedron ();

    QuadricSurfacef::ConvexPolyhedron m_kPoly;
    Vector3f m_kEye;
    Matrix3f m_kRot;
    int m_iSteps;

    // Inscribed polyhedron is an octahedron if 'true', a tetrahedron if
    // 'false'.
    bool m_bOctahedron;

    // A perspective camera is used if 'true', a parallel camera if 'false'.
    bool m_bPerspective;

    static Color ms_akColor[4];
};

#endif
