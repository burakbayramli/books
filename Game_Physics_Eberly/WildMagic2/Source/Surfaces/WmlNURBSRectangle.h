// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLNURBSRECTANGLE_H
#define WMLNURBSRECTANGLE_H

#include "WmlParametricSurface.h"
#include "WmlBSplineBasis.h"

namespace Wml
{

template <class Real>
class WML_ITEM NURBSRectangle : public ParametricSurface<Real>
{
public:
    // Construction and destruction.   The caller is responsible for deleting
    // the input arrays if they were dynamically allocated.  Internal copies
    // of the arrays are made, so to dynamically change control points,
    // control weights, or knots, you must use the 'SetControlPoint',
    // 'GetControlPoint', 'SetControlWeight', 'GetControlWeight', and 'Knot'
    // member functions.

    // The homogeneous input points are (x,y,z,w) where the (x,y,z) values are
    // stored in the akCtrlPoint array and the w values are stored in the
    // afCtrlWeight array.  The output points from curve evaluations are of
    // the form (x',y',z') = (x/w,y/w,z/w).

    // Spline types for curves are
    //   open uniform (OU)
    //   periodic uniform (PU)
    //   open nonuniform (ON)
    // For tensor product surfaces, you have to choose a type for each of two
    // dimensions, leading to nine possible spline types for surfaces.  The
    // constructors below represent these choices.

    // (OU,OU), (OU,PU), (PU,OU), or (PU,PU)
    NURBSRectangle (int iNumUCtrlPoints, int iNumVCtrlPoints,
        Vector3<Real>** aakCtrlPoint, Real** aafCtrlWeight, int iUDegree,
        int iVDegree, bool bULoop, bool bVLoop, bool bUOpen, bool bVOpen);

    // (OU,ON) or (PU,ON)
    NURBSRectangle (int iNumUCtrlPoints, int iNumVCtrlPoints,
        Vector3<Real>** aakCtrlPoint, Real** aafCtrlWeight, int iUDegree,
        int iVDegree, bool bULoop, bool bVLoop, bool bUOpen, Real* afVKnot);

    // (ON,OU) or (ON,PU)
    NURBSRectangle (int iNumUCtrlPoints, int iNumVCtrlPoints,
        Vector3<Real>** aakCtrlPoint, Real** aafCtrlWeight, int iUDegree,
        int iVDegree, bool bULoop, bool bVLoop, Real* afUKnot, bool bVOpen);

    // (ON,ON)
    NURBSRectangle (int iNumUCtrlPoints, int iNumVCtrlPoints,
        Vector3<Real>** aakCtrlPoint, Real** aafCtrlWeight, int iUDegree,
        int iVDegree, bool bULoop, bool bVLoop, Real* afUKnot, Real* afVKnot);

    virtual ~NURBSRectangle ();

    int GetNumCtrlPoints (int iDim) const;
    int GetDegree (int iDim) const;
    bool IsOpen (int iDim) const;
    bool IsUniform (int iDim) const;
    bool IsLoop (int iDim) const;

    // Control points and weights may be changed at any time.  If either input
    // index is invalid, the return value is undefined.
    void SetControlPoint (int iUIndex, int iVIndex,
        const Vector3<Real>& rkCtrl);
    const Vector3<Real>& GetControlPoint (int iUIndex, int iVIndex) const;
    void SetControlWeight (int iUIndex, int iVIndex, Real fWeight);
    Real GetControlWeight (int iUIndex, int iVIndex) const;

    // The knot values can be changed only if the surface is nonuniform in the
    // selected dimension and only if the input index is valid.  If these
    // conditions are not satisfied, the return values are undefined.
    Real& Knot (int iDim, int i);

    // The spline is defined for 0 <= u <= 1 and 0 <= v <= 1.  The input
    // values should be in this domain.  Any inputs smaller than 0 are clamped
    // to 0.  Any inputs larger than 1 are clamped to 1.
    virtual Vector3<Real> GetPosition (Real fU, Real fV) const;
    virtual Vector3<Real> GetDerivativeU (Real fU, Real fV) const;
    virtual Vector3<Real> GetDerivativeV (Real fU, Real fV) const;
    virtual Vector3<Real> GetDerivativeUU (Real fU, Real fV) const;
    virtual Vector3<Real> GetDerivativeUV (Real fU, Real fV) const;
    virtual Vector3<Real> GetDerivativeVV (Real fU, Real fV) const;

    // If you need position and derivatives at the same time, it is more
    // efficient to call these functions.  Pass the addresses of those
    // quantities whose values you want.  You may pass NULL in any argument
    // whose value you do not want.
    void Get (Real fU, Real fV, Vector3<Real>* pkPos, Vector3<Real>* pkDerU,
        Vector3<Real>* pkDerV, Vector3<Real>* pkDerUU, Vector3<Real>* pkDerUV,
        Vector3<Real>* pkDerVV) const;

protected:
    // Replicate the necessary number of control points when the Create
    // function has bLoop equal to true, in which case the spline surface
    // must be a closed surface in the corresponding dimension.
    void CreateControl (Vector3<Real>** aakCtrlPoint, Real** aafCtrlWeight);

    int m_iNumUCtrlPoints, m_iNumVCtrlPoints;
    Vector3<Real>** m_aakCtrlPoint;  // ctrl[unum][vnum]
    Real** m_aafCtrlWeight;    // weight[unum][vnum]
    bool m_abLoop[2];
    BSplineBasis<Real> m_akBasis[2];
    int m_iUReplicate, m_iVReplicate;

private:
    static Vector3<Real> ms_kInvalidCtrlPoint;
    static Real ms_fInvalidCtrlWeight;
};

typedef NURBSRectangle<float> NURBSRectanglef;
typedef NURBSRectangle<double> NURBSRectangled;

}

#endif
