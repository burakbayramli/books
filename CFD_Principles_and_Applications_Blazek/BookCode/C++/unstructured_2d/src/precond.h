/// @file precond.h
///
/// Definition of the class related to low Mach-number preconditioning.
/// Collection of matrices and operators (all formulations assume
/// general 3-D flow).
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 2, 2014
// 
//=============================================================================
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
//
//*****************************************************************************

#ifndef PRECOND_H_INCLUDED
#define PRECOND_H_INCLUDED

#include "defs.h"

/// @class Precond
/// Encompasses variables and functions related to low Mach-number
/// preconditioning.
///
class Precond
{
public:

  bool switchedOn;  /**< on/off switch (true, false) */
  REAL preCoeff,    /**< parameter K */
       machRef2;    /**< reference Mach number squared */

  // functions

  /// Initializes data of low Mach-number preconditioning.
  ///
  Precond()
  {
    switchedOn = false;
    preCoeff   = 0.15;
    machRef2   = 0.01;
  }

  //***************************************************************************

  /// Computes the preconditioning parameter.
  ///
  /// @param gam  ratio of specific heats
  /// @param c    speed of sound
  /// @param q2   total velocity squared
  ///
  REAL ComputeTheta( REAL gam, REAL c, REAL q2 ) const
  {
    REAL c2, mach2, mref2, beta;

    c2    = c*c;
    mach2 = q2/c2;
    mref2 = MAX(MIN(mach2,1.0),preCoeff*machRef2);
    beta  = mref2/(1.0+(gam-1.0)*mref2);

    return(1.0/(beta*c2));
  }

  //***************************************************************************

  /// Computes transformation matrix from primitive to conservative
  /// variables P (equivalent to the preconditioning matrix G).
  ///
  /// @param wvec   vector of conservative variables
  /// @param wpvec  vector of primitive variables
  /// @param H      total enthalpy
  /// @param theta  preconditioning parameter
  /// @param rhoT   derivative of density wrp. to temperature
  /// @param hp     derivative of enthalpy wrp. to pressure
  /// @param hT     derivative of enthalpy wrp. to temperature
  /// @param pmat   matrix P
  ///
  static void Prim2Cons( REAL wvec[], REAL wpvec[], REAL H, REAL theta,
                         REAL rhoT, REAL hp, REAL hT, REAL pmat[5][5] )
  {
    pmat[0][0] = theta;
    pmat[1][0] = theta*wpvec[1];
    pmat[2][0] = theta*wpvec[2];
    pmat[3][0] = theta*wpvec[3];
    pmat[4][0] = theta*H - 1. - wvec[0]*hp;

    pmat[0][1] = 0.;
    pmat[1][1] = wvec[0];
    pmat[2][1] = 0.;
    pmat[3][1] = 0.;
    pmat[4][1] = wvec[1];

    pmat[0][2] = 0.;
    pmat[1][2] = 0.;
    pmat[2][2] = wvec[0];
    pmat[3][2] = 0.;
    pmat[4][2] = wvec[2];

    pmat[0][3] = 0.;
    pmat[1][3] = 0.;
    pmat[2][3] = 0.;
    pmat[3][3] = wvec[0];
    pmat[4][3] = wvec[3];

    pmat[0][4] = rhoT;
    pmat[1][4] = rhoT*wpvec[1];
    pmat[2][4] = rhoT*wpvec[2];
    pmat[3][4] = rhoT*wpvec[3];
    pmat[4][4] = rhoT*H + wvec[0]*hT;
  }
  
  //***************************************************************************

  /// Computes transformation matrix from conservative to primitive variables
  /// P^-1 (equivalent to the inverse of the preconditioning matrix G^-1).
  ///
  /// @param wvec   vector of conservative variables
  /// @param wpvec  vector of primitive variables
  /// @param H      total enthalpy
  /// @param q2     total velocity squared
  /// @param theta  preconditioning parameter
  /// @param rhoT   derivative of density wrp. to temperature
  /// @param hp     derivative of enthalpy wrp. to pressure
  /// @param hT     derivative of enthalpy wrp. to temperature
  /// @param pmat   inverse of matrix P
  ///
  static void Cons2Prim( REAL wvec[], REAL wpvec[], REAL H, REAL q2,
                         REAL theta, REAL rhoT, REAL hp, REAL hT,
                         REAL pmat[5][5] )
  {
    double rrho, Hq2, ra1, d1, d2;

    rrho = 1./wvec[0];
    Hq2  = H - q2;
    ra1  = 1./(wvec[0]*theta*hT + rhoT*(1.-wvec[0]*hp));
    d1   = rhoT*ra1;
    d2   = theta*ra1;

    pmat[0][0] = ra1*(wvec[0]*hT+rhoT*Hq2);
    pmat[1][0] = -wpvec[1]*rrho;
    pmat[2][0] = -wpvec[2]*rrho;
    pmat[3][0] = -wpvec[3]*rrho;
    pmat[4][0] = ra1*(1.-theta*Hq2-wvec[0]*hp);

    pmat[0][1] = d1*wpvec[1];
    pmat[1][1] = rrho;
    pmat[2][1] = 0.;
    pmat[3][1] = 0.;
    pmat[4][1] = -d2*wpvec[1];

    pmat[0][2] = d1*wpvec[2];
    pmat[1][2] = 0.;
    pmat[2][2] = rrho;
    pmat[3][2] = 0.;
    pmat[4][2] = -d2*wpvec[2];

    pmat[0][3] = d1*wpvec[3];
    pmat[1][3] = 0.;
    pmat[2][3] = 0.;
    pmat[3][3] = rrho;
    pmat[4][3] = -d2*wpvec[3];

    pmat[0][4] = -d1;
    pmat[1][4] = 0.;
    pmat[2][4] = 0.;
    pmat[3][4] = 0.;
    pmat[4][4] = d2;
  }
  
  //***************************************************************************

  /// Computes matrix of eigenvalues of the preconditioned system.
  ///
  /// @param wvec   vector of conservative variables
  /// @param wpvec  vector of primitive variables
  /// @param V      contravariant velocity
  /// @param theta  preconditioning parameter
  /// @param rhop   derivative of density wrp. to pressure
  /// @param rhoT   derivative of density wrp. to temperature
  /// @param hp     derivative of enthalpy wrp. to pressure
  /// @param hT     derivative of enthalpy wrp. to temperature
  /// @param lam    matrix of eigenvalues
  ///
  static void Eigenvalues( REAL wvec[], REAL wpvec[], REAL V,
                           REAL theta, REAL rhop, REAL rhoT,
                           REAL hp, REAL hT, REAL lam[5][5] )
  {
    REAL a1, ra1g, a4, a5, cc2;

    a1   = wvec[0]*rhop*hT + rhoT*(1.-wvec[0]*hp);
    ra1g = 1./(wvec[0]*theta*hT + rhoT*(1.-wvec[0]*hp));
    a4   = a1*ra1g;
    a5   = wvec[0]*hT*ra1g;
    cc2  = SQRT(V*V*(a4-1.)*(a4-1.)+4.*a5);

    lam[0][0] = V;
    lam[1][0] = 0.;
    lam[2][0] = 0.;
    lam[3][0] = 0.;
    lam[4][0] = 0.;

    lam[0][1] = 0.;
    lam[1][1] = V;
    lam[2][1] = 0.;
    lam[3][1] = 0.;
    lam[4][1] = 0.;

    lam[0][2] = 0.;
    lam[1][2] = 0.;
    lam[2][2] = V;
    lam[3][2] = 0.;
    lam[4][2] = 0.;

    lam[0][3] = 0.;
    lam[1][3] = 0.;
    lam[2][3] = 0.;
    lam[3][3] = 0.5*((a4+1.)*V+cc2);
    lam[4][3] = 0.;

    lam[0][4] = 0.;
    lam[1][4] = 0.;
    lam[2][4] = 0.;
    lam[3][4] = 0.;
    lam[4][4] = 0.5*((a4+1.)*V-cc2);
  }
  
  //***************************************************************************

  /// Computes matrix of left eigenvectors of G^-1*A_c,p (i.e., (T_p)^-1).
  ///
  /// @param wvec   vector of conservative variables
  /// @param wpvec  vector of primitive variables
  /// @param nvec   components of the unit normal vector
  /// @param V      contravariant velocity
  /// @param theta  preconditioning parameter
  /// @param rhop   derivative of density wrp. to pressure
  /// @param rhoT   derivative of density wrp. to temperature
  /// @param hp     derivative of enthalpy wrp. to pressure
  /// @param hT     derivative of enthalpy wrp. to temperature
  /// @param evl    matrix of left eigenvectors (T_p)^-1
  ///
  static void LeftEigenvec( REAL wvec[], REAL wpvec[], REAL nvec[], REAL V,
                            REAL theta, REAL rhop, REAL rhoT,
                            REAL hp, REAL hT, REAL evl[5][5] )
  {
    REAL a1, a1g, ra1g, a4, a5, a6, a7, cc;
    REAL h1, h2, h3;

    a1   = wvec[0]*rhop*hT + rhoT*(1.-wvec[0]*hp);
    a1g  = wvec[0]*theta*hT + rhoT*(1.-wvec[0]*hp);
    ra1g = 1./a1g;
    a4   = a1*ra1g;
    a5   = wvec[0]*hT*ra1g;
    cc   = 0.5*SQRT(V*V*(a4-1.)*(a4-1.)+4.*a5);
    a6   = wvec[0]*a5;
    a7   = (V*(a4-1.))/(4.*cc);
    h1   = rhoT*ra1g;
    h2   = a6/cc;
    h3   = a6/wpvec[4];

    evl[0][0] = -h1*nvec[0];
    evl[1][0] = -h1*nvec[1];
    evl[2][0] = -h1*nvec[2];
    evl[3][0] =  0.5 + a7;
    evl[4][0] =  0.5 - a7;

    evl[0][1] =  0.;
    evl[1][1] = -h2*nvec[2];
    evl[2][1] =  h2*nvec[1];
    evl[3][1] =  0.5*h2*nvec[0];
    evl[4][1] = -0.5*h2*nvec[0];

    evl[0][2] =  h2*nvec[2];
    evl[1][2] =  0.;
    evl[2][2] = -h2*nvec[0];
    evl[3][2] =  0.5*h2*nvec[1];
    evl[4][2] = -0.5*h2*nvec[1];

    evl[0][3] = -h2*nvec[1];
    evl[1][3] =  h2*nvec[0];
    evl[2][3] =  0.;
    evl[3][3] =  0.5*h2*nvec[2];
    evl[4][3] = -0.5*h2*nvec[2];

    evl[0][4] = -h3*nvec[0];
    evl[1][4] = -h3*nvec[1];
    evl[2][4] = -h3*nvec[2];
    evl[3][4] =  0.;
    evl[4][4] =  0.;
  }
  
  //***************************************************************************

  /// Computes matrix of right eigenvectors of G^-1*A_c,p multiplied
  /// by the preconditioning matrix G (i.e., G*T_p).
  ///
  /// @param wvec   vector of conservative variables
  /// @param wpvec  vector of primitive variables
  /// @param nvec   components of the unit normal vector
  /// @param V      contravariant velocity
  /// @param H      total enthalpy
  /// @param theta  preconditioning parameter
  /// @param rhop   derivative of density wrp. to pressure
  /// @param rhoT   derivative of density wrp. to temperature
  /// @param hp     derivative of enthalpy wrp. to pressure
  /// @param hT     derivative of enthalpy wrp. to temperature
  /// @param evr    matrix of right eigenvectors multiplied by G (G*T_p)
  ///
  static void RightEigenvec( REAL wvec[], REAL wpvec[], REAL nvec[], REAL V,
                             REAL H, REAL theta, REAL rhop, REAL rhoT,
                             REAL hp, REAL hT, REAL evr[5][5] )
  {
    REAL a1, a1g, ra1g, a4, a5, a8, a9, a10, cc;
    REAL h1, h2, h3;

    a1   = wvec[0]*rhop*hT + rhoT*(1.-wvec[0]*hp);
    a1g  = wvec[0]*theta*hT + rhoT*(1.-wvec[0]*hp);
    ra1g = 1./a1g;
    a4   = a1*ra1g;
    a5   = wvec[0]*hT*ra1g;
    cc   = 0.5*SQRT(V*V*(a4-1.)*(a4-1.)+4.*a5);
    a8   = rhoT*wpvec[4]/wvec[0];
    a9   = -0.5*V*(a4-1.);
    a10  = a8*H + hT*wpvec[4];
    h1   = a9 + cc;
    h2   = a9 - cc;
    h3   = a1g/(wvec[0]*hT);

    evr[0][0] = -a8*nvec[0];
    evr[1][0] = -a8*wpvec[1]*nvec[0];
    evr[2][0] =  cc*nvec[2] - a8*wpvec[2]*nvec[0];
    evr[3][0] = -cc*nvec[1] - a8*wpvec[3]*nvec[0];
    evr[4][0] =  cc*(wpvec[2]*nvec[2]-wpvec[3]*nvec[1]) - a10*nvec[0];

    evr[0][1] = -a8*nvec[1];
    evr[1][1] = -cc*nvec[2] - a8*wpvec[1]*nvec[1];
    evr[2][1] = -a8*wpvec[2]*nvec[1];
    evr[3][1] =  cc*nvec[0] - a8*wpvec[3]*nvec[1];
    evr[4][1] =  cc*(wpvec[3]*nvec[0]-wpvec[1]*nvec[2]) - a10*nvec[1];

    evr[0][2] = -a8*nvec[2];
    evr[1][2] =  cc*nvec[1] - a8*wpvec[1]*nvec[2];
    evr[2][2] = -cc*nvec[0] - a8*wpvec[2]*nvec[2];
    evr[3][2] = -a8*wpvec[3]*nvec[2];
    evr[4][2] =  cc*(wpvec[1]*nvec[1]-wpvec[2]*nvec[0]) - a10*nvec[2];

    evr[0][3] = h3;
    evr[1][3] = wpvec[1] + h1*nvec[0];
    evr[2][3] = wpvec[2] + h1*nvec[1];
    evr[3][3] = wpvec[3] + h1*nvec[2];
    evr[4][3] = H + h1*V;

    evr[0][4] = h3;
    evr[1][4] = wpvec[1] + h2*nvec[0];
    evr[2][4] = wpvec[2] + h2*nvec[1];
    evr[3][4] = wpvec[3] + h2*nvec[2];
    evr[4][4] = H + h2*V;

    evr[0][0] *= h3;
    evr[0][1] *= h3;
    evr[0][2] *= h3;

    evr[1][0] *= h3;
    evr[1][1] *= h3;
    evr[1][2] *= h3;
    evr[1][3] *= h3;
    evr[1][4] *= h3;

    evr[2][0] *= h3;
    evr[2][1] *= h3;
    evr[2][2] *= h3;
    evr[2][3] *= h3;
    evr[2][4] *= h3;

    evr[3][0] *= h3;
    evr[3][1] *= h3;
    evr[3][2] *= h3;
    evr[3][3] *= h3;
    evr[3][4] *= h3;

    evr[4][0] *= h3;
    evr[4][1] *= h3;
    evr[4][2] *= h3;
    evr[4][3] *= h3;
    evr[4][4] *= h3;
  }
  
  //***************************************************************************

  /// Computes matrix product (T_p^-1) * (P^-1).
  ///
  /// @param wvec   vector of conservative variables
  /// @param wpvec  vector of primitive variables
  /// @param nvec   components of the unit normal vector
  /// @param V      contravariant velocity
  /// @param H      total enthalpy
  /// @param theta  preconditioning parameter
  /// @param rhop   derivative of density wrp. to pressure
  /// @param rhoT   derivative of density wrp. to temperature
  /// @param hp     derivative of enthalpy wrp. to pressure
  /// @param hT     derivative of enthalpy wrp. to temperature
  /// @param q2     total velocity squared
  /// @param mat    resulting matrix
  ///
  static void MatprodTp1_P1( REAL wvec[], REAL wpvec[], REAL nvec[], REAL V,
                             REAL H, REAL theta, REAL rhop, REAL rhoT,
                             REAL hp, REAL hT, REAL q2, REAL mat[5][5] )
  {
    REAL a1, a1g, ra1g, a4, a5, a5rp, a5c, a5c5, a5rt2, a7, cc, a14, a15, a15rt,
         a16, a16rt, a17, a17rt, h0, rhoT2, vc;

    a1    = wvec[0]*rhop*hT + rhoT*(1.-wvec[0]*hp);
    a1g   = wvec[0]*theta*hT + rhoT*(1.-wvec[0]*hp);
    ra1g  = 1./a1g;
    a4    = a1*ra1g;
    a5    = wvec[0]*hT*ra1g;
    cc    = 0.5*SQRT(V*V*(a4-1.)*(a4-1.)+4.*a5);
    a7    = V*(a4-1.)/(4.*cc);
    h0    = (a5*wvec[0])/(a1*wpvec[4]);
    a5rp  = rhop*h0;
    a5c   = a5/cc;
    a5c5  = 0.5*a5c;
    rhoT2 = (rhoT*rhoT)/(a1*a1g);
    a5rt2 = a5rp - rhoT2;
    a14   = h0*(1.-rhop*(H-q2)-wvec[0]*hp);
    a15   = (H-q2)*rhoT + wvec[0]*hT;
    a15rt = rhoT*a15/(a1*a1g);
    a16   = (0.5+a7)/a1;
    a16rt = a16*rhoT;
    a17   = (0.5-a7)/a1;
    a17rt = a17*rhoT;
    vc    = nvec[0]*wpvec[1] + nvec[1]*wpvec[2] + nvec[2]*wpvec[3];

    mat[0][0] = a5c*(nvec[1]*wpvec[3]-nvec[2]*wpvec[2]) - nvec[0]*(a14+a15rt);
    mat[0][1] = (wpvec[1]*nvec[0])*a5rt2;
    mat[0][2] = (wpvec[2]*nvec[0])*a5rt2 + a5c*nvec[2];
    mat[0][3] = (wpvec[3]*nvec[0])*a5rt2 - a5c*nvec[1];
    mat[0][4] = -nvec[0]*a5rt2;

    mat[1][0] = a5c*(nvec[2]*wpvec[1]-nvec[0]*wpvec[3]) - nvec[1]*(a14+a15rt);
    mat[1][1] = (wpvec[1]*nvec[1])*a5rt2 - a5c*nvec[2];
    mat[1][2] = (wpvec[2]*nvec[1])*a5rt2;
    mat[1][3] = (wpvec[3]*nvec[1])*a5rt2 + a5c*nvec[0];
    mat[1][4] = -nvec[1]*a5rt2;

    mat[2][0] = a5c*(nvec[0]*wpvec[2]-nvec[1]*wpvec[1]) - nvec[2]*(a14+a15rt);
    mat[2][1] = (wpvec[1]*nvec[2])*a5rt2 + a5c*nvec[1];
    mat[2][2] = (wpvec[2]*nvec[2])*a5rt2 - a5c*nvec[0];
    mat[2][3] = (wpvec[3]*nvec[2])*a5rt2;
    mat[2][4] = -nvec[2]*a5rt2;

    mat[3][0] = a15*a16 - a5c5*vc;
    mat[3][1] = a16rt*wpvec[1] + a5c5*nvec[0];
    mat[3][2] = a16rt*wpvec[2] + a5c5*nvec[1];
    mat[3][3] = a16rt*wpvec[3] + a5c5*nvec[2];
    mat[3][4] = -a16rt;

    mat[4][0] = a15*a17 + a5c5*vc;
    mat[4][1] = a17rt*wpvec[1] - a5c5*nvec[0];
    mat[4][2] = a17rt*wpvec[2] - a5c5*nvec[1];
    mat[4][3] = a17rt*wpvec[3] - a5c5*nvec[2];
    mat[4][4] = -a17rt;
  }
  
  //***************************************************************************

  /// Computes matrix times the inverse of a similar matrix, where both matrices
  /// have the structure of the preconditioning (or the transformation) matrix.
  /// Thus, products like P*G^-1 or G*P^-1 can be evaluated efficiently.
  ///
  /// @param wpvec  vector of primitive variables
  /// @param q2     total velocity squared
  /// @param amat   matrix A
  /// @param bmat   matrix B
  /// @param cmat   resulting matrix C, i.e., C=A*B
  ///
  static void MatrixTimesInverse( REAL wpvec[], REAL q2, REAL amat[5][5],
                                  REAL bmat[5][5], REAL cmat[5][5] )
  {
    REAL a1, a2, a3, a4, a5;

    a1 = amat[0][0]*bmat[0][0] + amat[0][4]*bmat[4][0];
    a2 = amat[0][0]*bmat[0][1] + amat[0][4]*bmat[4][1];
    a3 = amat[0][0]*bmat[0][2] + amat[0][4]*bmat[4][2];
    a4 = amat[0][0]*bmat[0][3] + amat[0][4]*bmat[4][3];
    a5 = amat[0][0]*bmat[0][4] + amat[0][4]*bmat[4][4];

    cmat[0][0] = a1;
    cmat[1][0] = wpvec[1]*a1 - wpvec[1];
    cmat[2][0] = wpvec[2]*a1 - wpvec[2];
    cmat[3][0] = wpvec[3]*a1 - wpvec[3];
    cmat[4][0] = amat[4][0]*bmat[0][0] + amat[4][4]*bmat[4][0] - q2;

    cmat[0][1] = a2;
    cmat[1][1] = wpvec[1]*a2 + 1.;
    cmat[2][1] = wpvec[2]*a2;
    cmat[3][1] = wpvec[3]*a2;
    cmat[4][1] = amat[4][0]*bmat[0][1] + amat[4][4]*bmat[4][1] + wpvec[1];

    cmat[0][2] = a3;
    cmat[1][2] = wpvec[1]*a3;
    cmat[2][2] = wpvec[2]*a3 + 1.;
    cmat[3][2] = wpvec[3]*a3;
    cmat[4][2] = amat[4][0]*bmat[0][2] + amat[4][4]*bmat[4][2] + wpvec[2];

    cmat[0][3] = a4;
    cmat[1][3] = wpvec[1]*a4;
    cmat[2][3] = wpvec[2]*a4;
    cmat[3][3] = wpvec[3]*a4 + 1.;
    cmat[4][3] = amat[4][0]*bmat[0][3] + amat[4][4]*bmat[4][3] + wpvec[3];

    cmat[0][4] = a5;
    cmat[1][4] = wpvec[1]*a5;
    cmat[2][4] = wpvec[2]*a5;
    cmat[3][4] = wpvec[3]*a5;
    cmat[4][4] = amat[4][0]*bmat[0][4] + amat[4][4]*bmat[4][4];
  }
  
  //***************************************************************************

  /// Computes matrix times vector (n=5).
  ///
  /// @param a  5x5 matrix A
  /// @param v  vector v (length 5)
  /// @param c  resulting vector c, i.e., c=A*v
  ///
  static void MatVecProd5( REAL a[5][5], REAL v[], REAL c[] )
  {
    c[0] = a[0][0]*v[0] + a[0][1]*v[1] + a[0][2]*v[2] +
           a[0][3]*v[3] + a[0][4]*v[4];
    c[1] = a[1][0]*v[0] + a[1][1]*v[1] + a[1][2]*v[2] +
           a[1][3]*v[3] + a[1][4]*v[4];
    c[2] = a[2][0]*v[0] + a[2][1]*v[1] + a[2][2]*v[2] +
           a[2][3]*v[3] + a[2][4]*v[4];
    c[3] = a[3][0]*v[0] + a[3][1]*v[1] + a[3][2]*v[2] +
           a[3][3]*v[3] + a[3][4]*v[4];
    c[4] = a[4][0]*v[0] + a[4][1]*v[1] + a[4][2]*v[2] +
           a[4][3]*v[3] + a[4][4]*v[4];
  }

private:

  Precond( const Precond &precond );             // override default copy constructor
  Precond & operator = (const Precond &precond); // and assignment operator
};

#endif // PRECOND_H_INCLUDED