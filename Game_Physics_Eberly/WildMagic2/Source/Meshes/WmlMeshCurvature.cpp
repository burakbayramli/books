// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlMeshCurvature.h"
#include "WmlMatrix2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
MeshCurvature<Real>::MeshCurvature (int iVQuantity,
    const Vector3<Real>* akVertex, int iTQuantity, const int* aiConnect)
    :
    m_akVertex(akVertex),
    m_aiConnect(aiConnect)
{
    m_iVQuantity = iVQuantity;
    m_iTQuantity = iTQuantity;

    // compute normal vectors
    m_akNormal = new Vector3<Real>[m_iVQuantity];
    memset(m_akNormal,0,m_iVQuantity*sizeof(Vector3<Real>));
    int i, iV0, iV1, iV2;
    for (i = 0; i < m_iTQuantity; i++)
    {
        // get vertex indices
        iV0 = *aiConnect++;
        iV1 = *aiConnect++;
        iV2 = *aiConnect++;

        // compute the normal (length provides a weighted sum)
        Vector3<Real> kEdge1 = m_akVertex[iV1] - m_akVertex[iV0];
        Vector3<Real> kEdge2 = m_akVertex[iV2] - m_akVertex[iV0];
        Vector3<Real> kNormal = kEdge1.Cross(kEdge2);

        m_akNormal[iV0] += kNormal;
        m_akNormal[iV1] += kNormal;
        m_akNormal[iV2] += kNormal;
    }
    for (i = 0; i < m_iVQuantity; i++)
        m_akNormal[i].Normalize();

    // compute the matrix of normal derivatives
    Matrix3<Real>* akDNormal = new Matrix3<Real>[m_iVQuantity];
    Matrix3<Real>* akWWTrn = new Matrix3<Real>[m_iVQuantity];
    Matrix3<Real>* akDWTrn = new Matrix3<Real>[m_iVQuantity];
    memset(akWWTrn,0,m_iVQuantity*sizeof(Matrix3<Real>));
    memset(akDWTrn,0,m_iVQuantity*sizeof(Matrix3<Real>));

    int iRow, iCol;
    aiConnect = m_aiConnect;
    for (i = 0; i < m_iTQuantity; i++)
    {
        // get vertex indices
        int aiV[3];
        aiV[0] = *aiConnect++;
        aiV[1] = *aiConnect++;
        aiV[2] = *aiConnect++;

        for (int j = 0; j < 3; j++)
        {
            iV0 = aiV[j];
            iV1 = aiV[(j+1)%3];
            iV2 = aiV[(j+2)%3];

            // Compute edge from V0 to V1, project to tangent plane of vertex,
            // and compute difference of adjacent normals.
            Vector3<Real> kE = m_akVertex[iV1] - m_akVertex[iV0];
            Vector3<Real> kW = kE - (kE.Dot(m_akNormal[iV0]))*m_akNormal[iV0];
            Vector3<Real> kD = m_akNormal[iV1] - m_akNormal[iV0];
            for (iRow = 0; iRow < 3; iRow++)
            {
                for (iCol = 0; iCol < 3; iCol++)
                {
                    akWWTrn[iV0][iRow][iCol] += kW[iRow]*kW[iCol];
                    akDWTrn[iV0][iRow][iCol] += kD[iRow]*kW[iCol];
                }
            }

            // Compute edge from V0 to V2, project to tangent plane of vertex,
            // and compute difference of adjacent normals.
            kE = m_akVertex[iV2] - m_akVertex[iV0];
            kW = kE - (kE.Dot(m_akNormal[iV0]))*m_akNormal[iV0];
            kD = m_akNormal[iV2] - m_akNormal[iV0];
            for (iRow = 0; iRow < 3; iRow++)
            {
                for (iCol = 0; iCol < 3; iCol++)
                {
                    akWWTrn[iV0][iRow][iCol] += kW[iRow]*kW[iCol];
                    akDWTrn[iV0][iRow][iCol] += kD[iRow]*kW[iCol];
                }
            }
        }
    }

    // Add in N*N^T to W*W^T for numerical stability.  In theory 0*0^T gets
    // added to D*W^T, but of course no update needed in the implementation.
    // Compute the matrix of normal derivatives.
    for (i = 0; i < m_iVQuantity; i++)
    {
        for (iRow = 0; iRow < 3; iRow++)
        {
            for (iCol = 0; iCol < 3; iCol++)
            {
                akWWTrn[i][iRow][iCol] = ((Real)0.5)*akWWTrn[i][iRow][iCol] +
                    m_akNormal[i][iRow]*m_akNormal[i][iCol];
                akDWTrn[i][iRow][iCol] *= (Real)0.5;
            }
        }

        akDNormal[i] = akDWTrn[i]*akWWTrn[i].Inverse();
    }

    delete[] akWWTrn;
    delete[] akDWTrn;

    // If N is a unit-length normal at a vertex, let U and V be unit-length
    // tangents so that {U, V, N} is an orthonormal set.  Define the matrix
    // J = [U | V], a 3-by-2 matrix whose columns are U and V.  Define J^T
    // to be the transpose of J, a 2-by-3 matrix.  Let dN/dX denote the
    // matrix of first-order derivatives of the normal vector field.  The
    // shape matrix is
    //   S = (J^T * J)^{-1} * J^T * dN/dX * J = J^T * dN/dX * J
    // where the superscript of -1 denotes the inverse.  (The formula allows
    // for J built from non-perpendicular vectors.) The matrix S is 2-by-2.
    // The principal curvatures are the eigenvalues of S.  If k is a principal
    // curvature and W is the 2-by-1 eigenvector corresponding to it, then
    // S*W = k*W (by definition).  The corresponding 3-by-1 tangent vector at
    // the vertex is called the principal direction for k, and is J*W.
    m_afMinCurvature = new Real[m_iVQuantity];
    m_afMaxCurvature = new Real[m_iVQuantity];
    m_akMinDirection = new Vector3<Real>[m_iVQuantity];
    m_akMaxDirection = new Vector3<Real>[m_iVQuantity];
    for (i = 0; i < m_iVQuantity; i++)
    {
        // compute U and V given N
        Vector3<Real> kU, kV;
        Vector3<Real>::GenerateOrthonormalBasis(kU,kV,m_akNormal[i],true);

        // Compute S = J^T * dN/dX * J.  In theory S is symmetric, but
        // because we have estimated dN/dX, we must slightly adjust our
        // calculations to make sure S is symmetric.
        Real fS01 = kU.Dot(akDNormal[i]*kV);
        Real fS10 = kV.Dot(akDNormal[i]*kU);
        Real fSAvr = ((Real)0.5)*(fS01+fS10);
        Matrix2<Real> kS
        (
            kU.Dot(akDNormal[i]*kU), fSAvr,
            fSAvr, kV.Dot(akDNormal[i]*kV)
        );

        // compute the eigenvalues of S (min and max curvatures)
        Real fTrace = kS[0][0] + kS[1][1];
        Real fDet = kS[0][0]*kS[1][1] - kS[0][1]*kS[1][0];
        Real fDiscr = fTrace*fTrace - ((Real)4.0)*fDet;
        Real fRootDiscr = Math<Real>::Sqrt(Math<Real>::FAbs(fDiscr));
        m_afMinCurvature[i] = ((Real)0.5)*(fTrace - fRootDiscr);
        m_afMaxCurvature[i] = ((Real)0.5)*(fTrace + fRootDiscr);

        Vector2<Real> kTest;
        Real fTest;

        // compute the eigenvectors of S
        Vector2<Real> kW0(kS[0][1],m_afMinCurvature[i]-kS[0][0]);
        Vector2<Real> kW1(m_afMinCurvature[i]-kS[1][1],kS[1][0]);
        if ( kW0.SquaredLength() >= kW1.SquaredLength() )
        {
            kW0.Normalize();
            m_akMinDirection[i] = kW0.X()*kU + kW0.Y()*kV;

            kTest = kS*kW0 - m_afMinCurvature[i]*kW0;
            fTest = kTest.Length();
        }
        else
        {
            kW1.Normalize();
            m_akMinDirection[i] = kW1.X()*kU + kW1.Y()*kV;

            kTest = kS*kW1 - m_afMinCurvature[i]*kW1;
            fTest = kTest.Length();
        }

        kW0 = Vector2<Real>(kS[0][1],m_afMaxCurvature[i]-kS[0][0]);
        kW1 = Vector2<Real>(m_afMaxCurvature[i]-kS[1][1],kS[1][0]);
        if ( kW0.SquaredLength() >= kW1.SquaredLength() )
        {
            kW0.Normalize();
            m_akMaxDirection[i] = kW0.X()*kU + kW0.Y()*kV;

            kTest = kS*kW0 - m_afMaxCurvature[i]*kW0;
            fTest = kTest.Length();
        }
        else
        {
            kW1.Normalize();
            m_akMaxDirection[i] = kW1.X()*kU + kW1.Y()*kV;

            kTest = kS*kW1 - m_afMaxCurvature[i]*kW1;
            fTest = kTest.Length();
        }
    }
}
//----------------------------------------------------------------------------
template <class Real>
MeshCurvature<Real>::~MeshCurvature ()
{
    delete[] m_akNormal;
    delete[] m_afMinCurvature;
    delete[] m_afMaxCurvature;
    delete[] m_akMinDirection;
    delete[] m_akMaxDirection;
}
//----------------------------------------------------------------------------
template <class Real>
int MeshCurvature<Real>::GetVQuantity () const
{
    return m_iVQuantity;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>* MeshCurvature<Real>::GetVertices () const
{
    return m_akVertex;
}
//----------------------------------------------------------------------------
template <class Real>
int MeshCurvature<Real>::GetTQuantity () const
{
    return m_iTQuantity;
}
//----------------------------------------------------------------------------
template <class Real>
const int* MeshCurvature<Real>::GetConnect () const
{
    return m_aiConnect;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>* MeshCurvature<Real>::GetNormals () const
{
    return m_akNormal;
}
//----------------------------------------------------------------------------
template <class Real>
const Real* MeshCurvature<Real>::GetMinCurvatures () const
{
    return m_afMinCurvature;
}
//----------------------------------------------------------------------------
template <class Real>
const Real* MeshCurvature<Real>::GetMaxCurvatures () const
{
    return m_afMaxCurvature;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>* MeshCurvature<Real>::GetMinDirections () const
{
    return m_akMinDirection;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>* MeshCurvature<Real>::GetMaxDirections () const
{
    return m_akMaxDirection;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM MeshCurvature<float>;
template class WML_ITEM MeshCurvature<double>;
}
//----------------------------------------------------------------------------
