// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "WmlMassSpringVolume.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real, class TVector>
MassSpringVolume<Real,TVector>::MassSpringVolume (int iSlices, int iRows,
    int iCols, Real fStep)
    :
    ParticlePhysics<Real,TVector>(iSlices*iRows*iCols,fStep)
{
    m_iSlices = iSlices;
    m_iRows = iRows;
    m_iCols = iCols;
    m_iSliceQuantity = m_iRows*m_iCols;
    m_iSlicesM1 = m_iSlices-1;
    m_iRowsM1 = m_iRows-1;
    m_iColsM1 = m_iCols-1;

    m_aaakPosition = new TVector**[m_iSlices];
    m_aaakVelocity = new TVector**[m_iSlices];
    for (int iSlice = 0; iSlice < m_iSlices; iSlice++)
    {
        m_aaakPosition[iSlice] = new TVector*[m_iRows];
        m_aaakVelocity[iSlice] = new TVector*[m_iRows];
        for (int iRow = 0; iRow < m_iRows; iRow++)
        {
            int i = m_iCols*(iRow + m_iRows*iSlice);
            m_aaakPosition[iSlice][iRow] = &m_akPosition[i];
            m_aaakVelocity[iSlice][iRow] = &m_akVelocity[i];
        }
    }

    Allocate3D(m_iCols,m_iRows,m_iSlicesM1,m_aaafConstantS);
    Allocate3D(m_iCols,m_iRows,m_iSlicesM1,m_aaafLengthS);
    Allocate3D(m_iCols,m_iRowsM1,m_iSlices,m_aaafConstantR);
    Allocate3D(m_iCols,m_iRowsM1,m_iSlices,m_aaafLengthR);
    Allocate3D(m_iColsM1,m_iRows,m_iSlices,m_aaafConstantC);
    Allocate3D(m_iColsM1,m_iRows,m_iSlices,m_aaafLengthC);
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
MassSpringVolume<Real,TVector>::~MassSpringVolume ()
{
    for (int iSlice = 0; iSlice < m_iSlices; iSlice++)
    {
        delete[] m_aaakPosition[iSlice];
        delete[] m_aaakVelocity[iSlice];
    }
    delete[] m_aaakPosition;
    delete[] m_aaakVelocity;

    Deallocate3D(m_iRows,m_iSlicesM1,m_aaafConstantS);
    Deallocate3D(m_iRows,m_iSlicesM1,m_aaafLengthS);
    Deallocate3D(m_iRowsM1,m_iSlices,m_aaafConstantR);
    Deallocate3D(m_iRowsM1,m_iSlices,m_aaafLengthR);
    Deallocate3D(m_iRows,m_iSlices,m_aaafConstantC);
    Deallocate3D(m_iRows,m_iSlices,m_aaafLengthC);
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
int MassSpringVolume<Real,TVector>::GetSlices () const
{
    return m_iSlices;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
int MassSpringVolume<Real,TVector>::GetRows () const
{
    return m_iRows;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
int MassSpringVolume<Real,TVector>::GetCols () const
{
    return m_iCols;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
void MassSpringVolume<Real,TVector>::SetMass (int iSlice, int iRow, int iCol,
    Real fMass)
{
    assert( 0 <= iSlice && iSlice < m_iSlices
        &&  0 <= iRow && iRow < m_iRows
        &&  0 <= iCol && iCol < m_iCols );

    int i = GetIndex(iSlice,iRow,iCol);
    ParticlePhysics<Real,TVector>::SetMass(i,fMass);
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
Real MassSpringVolume<Real,TVector>::GetMass (int iSlice, int iRow, int iCol)
    const
{
    assert( 0 <= iSlice && iSlice < m_iSlices
        &&  0 <= iRow && iRow < m_iRows
        &&  0 <= iCol && iCol < m_iCols );

    int i = GetIndex(iSlice,iRow,iCol);
    return ParticlePhysics<Real,TVector>::GetMass(i);
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
TVector*** MassSpringVolume<Real,TVector>::Positions3D () const
{
    return m_aaakPosition;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
TVector& MassSpringVolume<Real,TVector>::Position (int iSlice, int iRow,
    int iCol)
{
    assert( 0 <= iSlice && iSlice < m_iSlices
        &&  0 <= iRow && iRow < m_iRows
        &&  0 <= iCol && iCol < m_iCols );

    int i = GetIndex(iSlice,iRow,iCol);
    return ParticlePhysics<Real,TVector>::Position(i);
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
TVector*** MassSpringVolume<Real,TVector>::Velocities3D () const
{
    return m_aaakVelocity;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
TVector& MassSpringVolume<Real,TVector>::Velocity (int iSlice, int iRow,
    int iCol)
{
    assert( 0 <= iSlice && iSlice < m_iSlices
        &&  0 <= iRow && iRow < m_iRows
        &&  0 <= iCol && iCol < m_iCols );

    int i = GetIndex(iSlice,iRow,iCol);
    return ParticlePhysics<Real,TVector>::Velocity(i);
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
Real& MassSpringVolume<Real,TVector>::ConstantS (int iSlice, int iRow,
    int iCol)
{
    assert( 0 <= iSlice && iSlice < m_iSlices-1
        &&  0 <= iRow && iRow < m_iRows
        &&  0 <= iCol && iCol < m_iCols );

    return m_aaafConstantS[iSlice][iRow][iCol];
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
Real& MassSpringVolume<Real,TVector>::LengthS (int iSlice, int iRow, int iCol)
{
    assert( 0 <= iSlice && iSlice < m_iSlices-1
        &&  0 <= iRow && iRow < m_iRows
        &&  0 <= iCol && iCol < m_iCols );

    return m_aaafLengthS[iSlice][iRow][iCol];
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
Real& MassSpringVolume<Real,TVector>::ConstantR (int iSlice, int iRow,
    int iCol)
{
    assert( 0 <= iSlice && iSlice < m_iSlices
        &&  0 <= iRow && iRow < m_iRows-1
        &&  0 <= iCol && iCol < m_iCols );

    return m_aaafConstantR[iSlice][iRow][iCol];
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
Real& MassSpringVolume<Real,TVector>::LengthR (int iSlice, int iRow, int iCol)
{
    assert( 0 <= iSlice && iSlice < m_iSlices
        &&  0 <= iRow && iRow < m_iRows-1
        &&  0 <= iCol && iCol < m_iCols );

    return m_aaafLengthR[iSlice][iRow][iCol];
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
Real& MassSpringVolume<Real,TVector>::ConstantC (int iSlice, int iRow,
    int iCol)
{
    assert( 0 <= iSlice && iSlice < m_iSlices
        &&  0 <= iRow && iRow < m_iRows
        &&  0 <= iCol && iCol < m_iCols-1 );

    return m_aaafConstantC[iSlice][iRow][iCol];
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
Real& MassSpringVolume<Real,TVector>::LengthC (int iSlice, int iRow, int iCol)
{
    assert( 0 <= iSlice && iSlice < m_iSlices
        &&  0 <= iRow && iRow < m_iRows
        &&  0 <= iCol && iCol < m_iCols-1 );

    return m_aaafLengthC[iSlice][iRow][iCol];
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
TVector MassSpringVolume<Real,TVector>::Impulse (int i, Real fTime,
    const TVector* akPosition, const TVector* akVelocity)
{
    // Compute spring forces on position X[i].  The positions are not
    // necessarily m_akPosition since the RK4 solver in ParticlePhysics
    // evaluates the impulse function at intermediate positions.  The end
    // points of the curve of masses must be handled separately since each
    // has only one spring attached to it.

    TVector kImpulse = ExternalImpulse(i,fTime,akPosition,akVelocity);

    TVector kDiff, kForce;
    Real fRatio;

    int iSlice, iRow, iCol, iPrev, iNext;
    GetCoordinates(i,iSlice,iRow,iCol);

    if ( iSlice > 0 )
    {
        iPrev = i - m_iSliceQuantity;  // index to previous slice-neighbor
        kDiff = akPosition[iPrev] - akPosition[i];
        fRatio = LengthS(iSlice-1,iRow,iCol)/kDiff.Length();
        kForce = ConstantS(iSlice-1,iRow,iCol)*(((Real)1.0)-fRatio)*kDiff;
        kImpulse += m_afInvMass[i]*kForce;
    }

    if ( iSlice < m_iSlicesM1 )
    {
        iNext = i + m_iSliceQuantity;  // index to next slice-neighbor
        kDiff = akPosition[iNext] - akPosition[i];
        fRatio = LengthS(iSlice,iRow,iCol)/kDiff.Length();
        kForce = ConstantS(iSlice,iRow,iCol)*(((Real)1.0)-fRatio)*kDiff;
        kImpulse += m_afInvMass[i]*kForce;
    }

    if ( iRow > 0 )
    {
        iPrev = i - m_iCols;  // index to previous row-neighbor
        kDiff = akPosition[iPrev] - akPosition[i];
        fRatio = LengthR(iSlice,iRow-1,iCol)/kDiff.Length();
        kForce = ConstantR(iSlice,iRow-1,iCol)*(((Real)1.0)-fRatio)*kDiff;
        kImpulse += m_afInvMass[i]*kForce;
    }

    if ( iRow < m_iRowsM1 )
    {
        iNext = i + m_iCols;  // index to next row-neighbor
        kDiff = akPosition[iNext] - akPosition[i];
        fRatio = LengthR(iSlice,iRow,iCol)/kDiff.Length();
        kForce = ConstantR(iSlice,iRow,iCol)*(((Real)1.0)-fRatio)*kDiff;
        kImpulse += m_afInvMass[i]*kForce;
    }

    if ( iCol > 0 )
    {
        iPrev = i - 1;  // index to previous col-neighbor
        kDiff = akPosition[iPrev] - akPosition[i];
        fRatio = LengthC(iSlice,iRow,iCol-1)/kDiff.Length();
        kForce = ConstantC(iSlice,iRow,iCol-1)*(((Real)1.0)-fRatio)*kDiff;
        kImpulse += m_afInvMass[i]*kForce;
    }

    if ( iCol < m_iColsM1 )
    {
        iNext = i + 1;  // index to next col-neighbor
        kDiff = akPosition[iNext] - akPosition[i];
        fRatio = LengthC(iSlice,iRow,iCol)/kDiff.Length();
        kForce = ConstantC(iSlice,iRow,iCol)*(((Real)1.0)-fRatio)*kDiff;
        kImpulse += m_afInvMass[i]*kForce;
    }

    return kImpulse;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
TVector MassSpringVolume<Real,TVector>::ExternalImpulse (int, Real,
    const TVector*, const TVector*)
{
    return TVector::ZERO;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
int MassSpringVolume<Real,TVector>::GetIndex (int iSlice, int iRow, int iCol)
    const
{
    return iCol + m_iCols*(iRow + m_iRows*iSlice);
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
void MassSpringVolume<Real,TVector>::GetCoordinates (int i, int& riSlice,
    int& riRow, int& riCol) const
{
    riCol = i % m_iCols;
    i = (i - riCol)/m_iCols;
    riRow = i % m_iRows;
    riSlice = i / m_iRows;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM MassSpringVolume<float,Vector3f>;
template class WML_ITEM MassSpringVolume<double,Vector3d>;
}
//----------------------------------------------------------------------------
