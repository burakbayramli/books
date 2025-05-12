// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "WmlMassSpringSurface.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real, class TVector>
MassSpringSurface<Real,TVector>::MassSpringSurface (int iRows, int iCols,
    Real fStep)
    :
    ParticlePhysics<Real,TVector>(iRows*iCols,fStep)
{
    m_iRows = iRows;
    m_iCols = iCols;
    m_iRowsM1 = m_iRows-1;
    m_iColsM1 = m_iCols-1;

    m_aakPosition = new TVector*[m_iRows];
    m_aakVelocity = new TVector*[m_iRows];
    for (int iRow = 0; iRow < m_iRows; iRow++)
    {
        m_aakPosition[iRow] = &m_akPosition[m_iCols*iRow];
        m_aakVelocity[iRow] = &m_akVelocity[m_iCols*iRow];
    }

    Allocate2D(m_iCols,m_iRowsM1,m_aafConstantR);
    Allocate2D(m_iCols,m_iRowsM1,m_aafLengthR);
    Allocate2D(m_iColsM1,m_iRows,m_aafConstantC);
    Allocate2D(m_iColsM1,m_iRows,m_aafLengthC);
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
MassSpringSurface<Real,TVector>::~MassSpringSurface ()
{
    delete[] m_aakPosition;
    delete[] m_aakVelocity;
    Deallocate2D(m_aafConstantR);
    Deallocate2D(m_aafLengthR);
    Deallocate2D(m_aafConstantC);
    Deallocate2D(m_aafLengthC);
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
int MassSpringSurface<Real,TVector>::GetRows () const
{
    return m_iRows;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
int MassSpringSurface<Real,TVector>::GetCols () const
{
    return m_iCols;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
void MassSpringSurface<Real,TVector>::SetMass (int iRow, int iCol, Real fMass)
{
    assert( 0 <= iRow && iRow < m_iRows && 0 <= iCol && iCol < m_iCols );
    ParticlePhysics<Real,TVector>::SetMass(GetIndex(iRow,iCol),fMass);
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
Real MassSpringSurface<Real,TVector>::GetMass (int iRow, int iCol) const
{
    assert( 0 <= iRow && iRow < m_iRows && 0 <= iCol && iCol < m_iCols );
    return ParticlePhysics<Real,TVector>::GetMass(GetIndex(iRow,iCol));
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
TVector** MassSpringSurface<Real,TVector>::Positions2D () const
{
    return m_aakPosition;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
TVector& MassSpringSurface<Real,TVector>::Position (int iRow, int iCol)
{
    assert( 0 <= iRow && iRow < m_iRows && 0 <= iCol && iCol < m_iCols );
    return ParticlePhysics<Real,TVector>::Position(GetIndex(iRow,iCol));
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
TVector** MassSpringSurface<Real,TVector>::Velocities2D () const
{
    return m_aakVelocity;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
TVector& MassSpringSurface<Real,TVector>::Velocity (int iRow, int iCol)
{
    assert( 0 <= iRow && iRow < m_iRows && 0 <= iCol && iCol < m_iCols );
    return ParticlePhysics<Real,TVector>::Velocity(GetIndex(iRow,iCol));
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
Real& MassSpringSurface<Real,TVector>::ConstantR (int iRow, int iCol)
{
    assert( 0 <= iRow && iRow < m_iRows-1 && 0 <= iCol && iCol < m_iCols );
    return m_aafConstantR[iRow][iCol];
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
Real& MassSpringSurface<Real,TVector>::LengthR (int iRow, int iCol)
{
    assert( 0 <= iRow && iRow < m_iRows-1 && 0 <= iCol && iCol < m_iCols );
    return m_aafLengthR[iRow][iCol];
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
Real& MassSpringSurface<Real,TVector>::ConstantC (int iRow, int iCol)
{
    assert( 0 <= iRow && iRow < m_iRows && 0 <= iCol && iCol < m_iCols-1 );
    return m_aafConstantC[iRow][iCol];
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
Real& MassSpringSurface<Real,TVector>::LengthC (int iRow, int iCol)
{
    assert( 0 <= iRow && iRow < m_iRows && 0 <= iCol && iCol < m_iCols-1 );
    return m_aafLengthC[iRow][iCol];
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
TVector MassSpringSurface<Real,TVector>::Impulse (int i, Real fTime,
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

    int iRow, iCol, iPrev, iNext;
    GetCoordinates(i,iRow,iCol);

    if ( iRow > 0 )
    {
        iPrev = i - m_iCols;  // index to previous row-neighbor
        kDiff = akPosition[iPrev] - akPosition[i];
        fRatio = LengthR(iRow-1,iCol)/kDiff.Length();
        kForce = ConstantR(iRow-1,iCol)*(((Real)1.0)-fRatio)*kDiff;
        kImpulse += m_afInvMass[i]*kForce;
    }

    if ( iRow < m_iRowsM1 )
    {
        iNext = i + m_iCols;  // index to next row-neighbor
        kDiff = akPosition[iNext] - akPosition[i];
        fRatio = LengthR(iRow,iCol)/kDiff.Length();
        kForce = ConstantR(iRow,iCol)*(((Real)1.0)-fRatio)*kDiff;
        kImpulse += m_afInvMass[i]*kForce;
    }

    if ( iCol > 0 )
    {
        iPrev = i - 1;  // index to previous col-neighbor
        kDiff = akPosition[iPrev] - akPosition[i];
        fRatio = LengthC(iRow,iCol-1)/kDiff.Length();
        kForce = ConstantC(iRow,iCol-1)*(((Real)1.0)-fRatio)*kDiff;
        kImpulse += m_afInvMass[i]*kForce;
    }

    if ( iCol < m_iColsM1 )
    {
        iNext = i + 1;  // index to next col-neighbor
        kDiff = akPosition[iNext] - akPosition[i];
        fRatio = LengthC(iRow,iCol)/kDiff.Length();
        kForce = ConstantC(iRow,iCol)*(((Real)1.0)-fRatio)*kDiff;
        kImpulse += m_afInvMass[i]*kForce;
    }

    return kImpulse;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
TVector MassSpringSurface<Real,TVector>::ExternalImpulse (int, Real,
    const TVector*, const TVector*)
{
    return TVector::ZERO;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
int MassSpringSurface<Real,TVector>::GetIndex (int iRow, int iCol) const
{
    return iCol + m_iCols*iRow;
}
//----------------------------------------------------------------------------
template <class Real, class TVector>
void MassSpringSurface<Real,TVector>::GetCoordinates (int i, int& riRow,
    int& riCol) const
{
    riCol = i % m_iCols;
    riRow = i / m_iCols;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM MassSpringSurface<float,Vector2f>;
template class WML_ITEM MassSpringSurface<double,Vector2d>;
template class WML_ITEM MassSpringSurface<float,Vector3f>;
template class WML_ITEM MassSpringSurface<double,Vector3d>;
}
//----------------------------------------------------------------------------
