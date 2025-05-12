// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLNURBSCURVE3_H
#define WMLNURBSCURVE3_H

#include "WmlSingleCurve3.h"
#include "WmlBSplineBasis.h"

namespace Wml
{

template <class Real>
class WML_ITEM NURBSCurve3 : public SingleCurve3<Real>
{
public:
    // Construction and destruction.  The caller is responsible for deleting
    // the input arrays if they were dynamically allocated.  Internal copies
    // of the arrays are made, so to dynamically change control points,
    // control weights, or knots, you must use the 'SetControlPoint',
    // 'GetControlPoint', 'SetControlWeight', 'GetControlWeight', and 'Knot'
    // member functions.

    // The homogeneous input points are (x,y,z,w) where the (x,y,z) values are
    // stored in the akCtrlPoint array and the w values are stored in the
    // afCtrlWeight array.  The output points from curve evaluations are of
    // the form (x',y',z') = (x/w,y/w,z/w).

    // Uniform spline.  The number of control points is n+1 >= 2.  The degree
    // of the spline is d and must satisfy 1 <= d <= n.  The knots are
    // implicitly calculated in [0,1].  If bOpen is 'true', the spline is
    // open and the knots are
    //   t[i] = 0,               0 <= i <= d
    //          (i-d)/(n+1-d),   d+1 <= i <= n
    //          1,               n+1 <= i <= n+d+1
    // If bOpen is 'false', the spline is periodic and the knots are
    //   t[i] = (i-d)/(n+1-d),   0 <= i <= n+d+1
    // If bLoop is 'true', extra control points are added to generate a closed
    // curve.  For an open spline, the control point array is reallocated and
    // one extra control point is added, set to the first control point
    // C[n+1] = C[0].  For a periodic spline, the control point array is
    // reallocated and the first d points are replicated.  In either case the
    // knot array is calculated accordingly.

    NURBSCurve3 (int iNumCtrlPoints, Vector3<Real>* akCtrlPoint,
        Real* afCtrlWeight, int iDegree, bool bLoop, bool bOpen);

    // Open, nonuniform spline.  The knot array must have n-d elements.  The
    // elements must be nondecreasing.  Each element must be in [0,1].
    NURBSCurve3 (int iNumCtrlPoints, Vector3<Real>* akCtrlPoint,
        Real* afCtrlWeight, int iDegree, bool bLoop, Real* afKnot);

    virtual ~NURBSCurve3 ();

    int GetNumCtrlPoints () const;
    int GetDegree () const;
    bool IsOpen () const;
    bool IsUniform () const;
    bool IsLoop () const;

    // Control points and weights may be changed at any time.  The input index
    // should be valid (0 <= i <= n).  If it is invalid, the return value is
    // undefined.
    void SetControlPoint (int i, const Vector3<Real>& rkCtrl);
    const Vector3<Real>& GetControlPoint (int i) const;
    void SetControlWeight (int i, Real fWeight);
    Real GetControlWeight (int i) const;

    // The knot values can be changed only if the basis function is nonuniform
    // and the input index is valid (0 <= i <= n-d-1).  If these conditions
    // are not satisfied, the return value is undefined.
    Real& Knot (int i);

    // The spline is defined for 0 <= t <= 1.  If a t-value is outside [0,1],
    // an open spline clamps t to [0,1].  That is, if t > 1, t is set to 1;
    // if t < 0, t is set to 0.  A periodic spline wraps to to [0,1].  That
    // is, if t is outside [0,1], then t is set to t-floor(t).
    virtual Vector3<Real> GetPosition (Real fTime) const;
    virtual Vector3<Real> GetFirstDerivative (Real fTime) const;
    virtual Vector3<Real> GetSecondDerivative (Real fTime) const;
    virtual Vector3<Real> GetThirdDerivative (Real fTime) const;

    // If you need position and derivatives at the same time, it is more
    // efficient to call these functions.  Pass the addresses of those
    // quantities whose values you want.  You may pass NULL in any argument
    // whose value you do not want.
    void Get (Real fTime, Vector3<Real>* pkPos, Vector3<Real>* pkDer1,
        Vector3<Real>* pkDer2, Vector3<Real>* pkDer3) const;

    // TO DO.  This is not yet implemented.
    virtual Real GetVariation (Real fT0, Real fT1,
        const Vector3<Real>* pkP0 = NULL,
        const Vector3<Real>* pkP1 = NULL) const;

protected:
    // Replicate the necessary number of control points when the Create
    // function has bLoop equal to true, in which case the spline curve must
    // be a closed curve.
    void CreateControl (Vector3<Real>* akCtrlPoint, Real* afCtrlWeight);

    int m_iNumCtrlPoints;
    Vector3<Real>* m_akCtrlPoint;  // ctrl[n+1]
    Real* m_afCtrlWeight;    // weight[n+1]
    bool m_bLoop;
    BSplineBasis<Real> m_kBasis;
    int m_iReplicate;  // the number of replicated control points

private:
    static Vector3<Real> ms_kInvalidCtrlPoint;
};

typedef NURBSCurve3<float> NURBSCurve3f;
typedef NURBSCurve3<double> NURBSCurve3d;

}

#endif
